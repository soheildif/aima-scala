package aima.search

//Node :  a node in the search tree
class Node[S <: State, A <: Action](val state: S, val parent: Option[Node[S,A]], 
           val action: Option[A], val depth: Int, val pathCost: Double) {
  
  def solution: List[A] = {
    def loop(node: Node[S,A], a: List[A]): List[A] =
      node.parent match {
        case None => a
        case Some(x) => loop(x, node.action.get :: a)
      }
    loop(this, Nil)
  }
}

object Node {
  def apply[S <: State,A <: Action](state:S) = new Node[S,A](state,None,None,0,0.0)

  def apply[S <: State,A <: Action](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int) =
    new Node[S,A](state,parent,action,depth,0.0)

  def apply[S <: State,A <: Action](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int, pathCost: Double) =
    new Node[S,A](state,parent,action,depth,pathCost)
}


//Possible Search Results
sealed abstract class SearchResult[+A <: Action]
final case class Success[A <: Action](actions: List[A]) extends SearchResult
final case class Failure extends SearchResult
final case class CutOff extends SearchResult[Nothing]

//Uninformed search algorithms
object Uninformed {  

  //Tree Search
  private def TreeSearch[S <: State,A <: Action](problem: Problem[S,A], fringe: Fringe[Node[S,A]]) = {

    def loop(fringe:Fringe[Node[S,A]]): Option[Node[S,A]] =
      fringe.removeFirst match {
        case None => None
        case Some(node) if problem.goalTest(node.state) => 
          println(node.state.toString()) //print the state when goal is reached
          Some(node)
        case Some(node) => loop(fringe.insertAll(expand(node,problem)))
      }
    
    loop(fringe.insert(Node(problem.initialState))) match {
      case None => Failure()
      case Some(node) => Success(node.solution)
    }
  }

  def expand[S <: State, A <: Action](node: Node[S,A], problem: Problem[S,A]) =
    problem.successorFn(node.state).map(
      (t: Tuple2[A,S]) => Node(t._2, Some(node), Some(t._1), node.depth+1, 
                               node.pathCost + problem.stepCost(node.state,t._2)))

  def BreadthFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) = TreeSearch(problem, new FifoFringe[Node[S,A]]())

  def DepthFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) = TreeSearch(problem, new LifoFringe[Node[S,A]]())

  def UniformCostSearch[S <: State, A <: Action](problem: Problem[S,A]) =
    TreeSearch(problem, new PriorityQueueFringe[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      that.pathCost.compare(node.pathCost)
      }))

  def DepthLimitedSearch[S <: State, A <: Action](problem: Problem[S,A], limit: Int) =
    recursiveDLS(Node[S,A](problem.initialState),problem,limit)

  private def recursiveDLS[S <: State, A <: Action](node: Node[S,A], problem: Problem[S,A], limit: Int): SearchResult[A] = {
    if (problem.goalTest(node.state)) Success(node.solution) //success
    else {
      if (node.depth == limit) CutOff() //cut-off limit reached
      else {
        def loop(nodes: List[Node[S,A]], cutoffOccured: Boolean): SearchResult[A] = 
          nodes match {
            case Nil => if(cutoffOccured) CutOff() else Failure()
            case n :: rest => 
              recursiveDLS(n,problem,limit) match {
                case Failure() => loop(rest,cutoffOccured)
                case CutOff() => loop(rest,true)
                case Success(n) => Success(n)
              }
          }
        loop(expand(node,problem),false)
      }
    }
  }

  def IterativeDeepeningSearch[S <: State, A <: Action](problem: Problem[S,A]) = {
    def loop(depth: Int): SearchResult[A] =
      DepthLimitedSearch(problem,depth) match {
        case CutOff() => loop(depth + 1)
        case Failure() => Failure()
        case Success(actions) => Success(actions)
      }
    loop(0)
  }

   def GraphSearch[S <: State,A <: Action](problem: Problem[S,A], fringe: Fringe[Node[S,A]]) = {

    def loop(fringe:Fringe[Node[S,A]], closed: List[Node[S,A]]): Option[Node[S,A]] =
      fringe.removeFirst match {
        case None => None
        case Some(node) if problem.goalTest(node.state) => 
          println(node.state.toString()) //print the state when goal is reached
          Some(node)
        case Some(node) => 
          if(closed.exists(_.state == node.state)) 
             loop(fringe,closed) //ignore successors if node is already visited
          else {
            loop(fringe.insertAll(expand(node,problem)), node :: closed)
          }
      }
    
    loop(fringe.insert(Node(problem.initialState)),Nil) match {
      case None => Failure()
      case Some(node) => Success(node.solution)
    }
  }

  def BreadthFirstGraphSearch[S <: State, A <: Action](problem: Problem[S,A]) =
    GraphSearch(problem, new FifoFringe[Node[S,A]]())

  def DepthFirstGraphSearch[S <: State, A <: Action](problem: Problem[S,A]) =
    GraphSearch(problem, new LifoFringe[Node[S,A]]())
}

//Informed Search Algorithms
object Informed {

  import Uninformed._;

  private def GreedyBestFirstHeuristic[S <: State, A <: Action](node: Node[S,A],problem: Problem[S,A]) =
    problem.estimatedCostToGoal(node.state) //h(n)

  def GreedyBestFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueueFringe[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      GreedyBestFirstHeuristic(that,problem).compare(GreedyBestFirstHeuristic(node,problem))
      }))

  private def AStarHeuristic[S <: State, A <: Action](node: Node[S,A],problem: Problem[S,A]) =
    node.pathCost + problem.estimatedCostToGoal(node.state) //f(n) = g(n) + h(n)

  def AStarSearch[S <: State, A <: Action](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueueFringe[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      AStarHeuristic(that,problem).compare(AStarHeuristic(node,problem))
      }))

  //def RecursiveBestFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) =

/*
  //max(f-value-of-node, f-value-of-parent-node)
  private def RBFSHeuristic[S <: State, A <: Action](node: Node[S,A],problem: Problem[S,A]) =
    node.parent match {
      case Some(paren) => Math.max(node.pathCost + problem.estimatedCostToGoal(node.state),
                                 paren.pathCost + problem.estimatedCostToGoal(paren.state))
      case None => node.pathCost + problem.estimatedCostToGoal(node.state)
    }

  private val Infinity = Math.MAX_DOUBLE
  final case class RBFSFailure(fVal: Double) extends SearchResult

  //returns success, failure AND a new f-cost limit
  private def RBFS[S <: State, A <: Action](problem: Problem[S,A], node: Node[S,A], fLimit: Double): SearchResult[A] = {
    println("print " + node.state)
    if(problem.goalTest(node.state)) Success(node.solution)
    val successors = expand(node,problem)
    successors.foreach( (node) => println("successors " + node.state))
    if(successors.isEmpty) RBFSFailure(Infinity)
    else {
      val successorsPq = new PriorityQueueFringe[(Double,Node[S,A])](
        (arg) => { 
          val (fVal, _) = arg
          new Ordered[(Double,Node[S,A])] {
                      def compare(that: (Double,Node[S,A])) =
                        that._1.compare(fVal)
        }})
      successors.foreach((n) => successorsPq.insert((RBFSHeuristic(n,problem),n)))
      def loop: SearchResult[A] = {
        val Some((best_fVal,bestNode)) = successorsPq.removeFirst
        if(best_fVal > fLimit) RBFSFailure(fLimit)
        else {
          val alternative_fVal =
            successorsPq.removeFirst match {
              case Some((f,n)) => println("best alternative" + n.state);successorsPq.insert((f,n)); f
              case None => best_fVal
            }
          successorsPq.insert((best_fVal,bestNode))
          println("best node is " + bestNode.state)
          RBFS(problem,bestNode,Math.min(fLimit, alternative_fVal)) match {
            case Success(a) => Success(a)
            case RBFSFailure(f) => 
              successorsPq.removeFirst
              successorsPq.insert((f,bestNode))
              loop
          }
        }
      }
      loop
    }
  }

  def RecursiveBestFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) =
    RBFS(problem,Node[S,A](problem.initialState),Infinity) match {
      case Success(x) => Success(x)
      case _ => Failure() }

*/

}

object Local {

  //returns state that maximizes the value
  def HillClimbingSearch[S <: State, A <: Action](problem: Problem[S,A], value: (S)=>Double) = {

    def getHighestValuedSuccessor(s: S): Option[(Double,S)] = {
      val successors = problem.successorFn(s)
      if(!successors.isEmpty) {
        val tmp = successors.map((st) => (value(st._2),st._2))
        Some(tmp.foldLeft(tmp.head)((a,b)=>if(a._1 < b._1) b else a))
      }
      else None
    }
    def loop(current: S): S = {
      getHighestValuedSuccessor(current) match {
        case Some((v,n)) =>
          if(v <= value(current)) current
          else loop(n)
        case None => current
      }
    }
    loop(problem.initialState)
  }

  def SimulatedAnnealingSearch[S <: State, A <: Action](problem: Problem[S,A],value: (S)=>Double, schedule: (Int)=>Double) = {

    val random = new scala.util.Random(new java.util.Random)

    def randomSuccessor(s: S): Option[S] = {
      val successors = problem.successorFn(s)
      if(successors.isEmpty) None
      else {
        Some(successors(random.nextInt(successors.length))._2)
      }
    }
 
    def loop(current: S, t: Int): S = {
      val T = schedule(t)
      if(T == 0.0) current
      else {
        randomSuccessor(current) match {
          case Some(next) => {
            val dE = value(next) - value(current)
            if(dE > 0.0) loop(next,t+1)
            else {
              if(Math.random < Math.exp(dE/T)) loop(next,t+1)
              else loop(current,t+1)
            }}
          case None => current
        }
      }
    }
    loop(problem.initialState,1)
  }
}
