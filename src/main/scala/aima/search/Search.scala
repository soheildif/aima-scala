package aima.search

/* This file contains code for algorithms from Chapter-3, AIMA-3e
 *
 * @author Himanshu Gutpta
 */

/* Node data structure as described in Fig 3.10 */
class Node[S, A](val state: S, val parent: Option[Node[S,A]], 
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

/* Factory for easy Node creation */
object Node {
  def apply[S,A](state:S) = new Node[S,A](state,None,None,0,0.0)

  def apply[S,A](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int) =
    new Node[S,A](state,parent,action,depth,0.0)

  def apply[S,A](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int, pathCost: Double) =
    new Node[S,A](state,parent,action,depth,pathCost)
}


/* Possible search results that various search algorithms can return */
sealed abstract class SearchResult
final case class Success[A](actions: List[A]) extends SearchResult
final case class Failure extends SearchResult
final case class CutOff extends SearchResult

/* ------ Uninformed search algorithms --------- */
object Uninformed {  

  //TODO: Queue does not exit but Frontier and Queue does
  //TODO: Closed-List is gone but Explored-Set is in for graph search
  //TODO: Do we really need the base classes to be declared every where??
  //Tree Search
  private def TreeSearch[S,A](problem: Problem[S,A], fringe: Queue[Node[S,A]]) = {

    def loop(fringe:Queue[Node[S,A]]): Option[Node[S,A]] =
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

  def expand[S, A](node: Node[S,A], problem: Problem[S,A]) =
    problem.successorFn(node.state).map(
      (t: Tuple2[A,S]) => Node(t._2, Some(node), Some(t._1), node.depth+1, 
                               node.pathCost + problem.stepCost(node.state,t._2)))

  def BreadthFirstSearch[S, A](problem: Problem[S,A]) = TreeSearch(problem, new FifoQueue[Node[S,A]]())

  def DepthFirstSearch[S, A](problem: Problem[S,A]) = TreeSearch(problem, new LifoQueue[Node[S,A]]())

  //TODO: Write a test case from Sibiu to Bucharest as in Fig-3.15
  def UniformCostSearch[S, A](problem: Problem[S,A]) =
    TreeSearch(problem, new PriorityQueue[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      that.pathCost.compare(node.pathCost)
      }))

  def DepthLimitedSearch[S, A](problem: Problem[S,A], limit: Int) =
    recursiveDLS(Node[S,A](problem.initialState),problem,limit)

  private def recursiveDLS[S, A](node: Node[S,A], problem: Problem[S,A], limit: Int): SearchResult = {
    if (problem.goalTest(node.state)) Success(node.solution) //success
    else {
      if (node.depth == limit) CutOff() //cut-off limit reached
      else {
        def loop(nodes: List[Node[S,A]], cutoffOccured: Boolean): SearchResult = 
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

  def IterativeDeepeningSearch[S, A](problem: Problem[S,A]) = {
    def loop(depth: Int): SearchResult =
      DepthLimitedSearch(problem,depth) match {
        case CutOff() => loop(depth + 1)
        case Failure() => Failure()
        case Success(actions) => Success(actions)
      }
    loop(0)
  }

   def GraphSearch[S,A](problem: Problem[S,A], fringe: Queue[Node[S,A]]) = {

    //TODO: Change we change closed to explored(see if we can store state instead of node here) and make lookup hash based for O(1)
    //AND s/fringe/frontier; do not use successor function but actions and transitionModel; may be we can get rid of expand now
    def loop(fringe:Queue[Node[S,A]], closed: List[Node[S,A]]): Option[Node[S,A]] =
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

  def BreadthFirstGraphSearch[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new FifoQueue[Node[S,A]]())

  def DepthFirstGraphSearch[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new LifoQueue[Node[S,A]]())
}

//Informed Search Algorithms
object Informed {

  import Uninformed._;

  private def GreedyBestFirstHeuristic[S, A](node: Node[S,A],problem: Problem[S,A]) =
    problem.estimatedCostToGoal(node.state) //h(n)

  def GreedyBestFirstSearch[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      GreedyBestFirstHeuristic(that,problem).compare(GreedyBestFirstHeuristic(node,problem))
      }))

  private def AStarHeuristic[S, A](node: Node[S,A],problem: Problem[S,A]) =
    node.pathCost + problem.estimatedCostToGoal(node.state) //f(n) = g(n) + h(n)

  def AStarSearch[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      AStarHeuristic(that,problem).compare(AStarHeuristic(node,problem))
      }))

  //def RecursiveBestFirstSearch[S, A](problem: Problem[S,A]) =

/*
  //max(f-value-of-node, f-value-of-parent-node)
  private def RBFSHeuristic[S, A](node: Node[S,A],problem: Problem[S,A]) =
    node.parent match {
      case Some(paren) => Math.max(node.pathCost + problem.estimatedCostToGoal(node.state),
                                 paren.pathCost + problem.estimatedCostToGoal(paren.state))
      case None => node.pathCost + problem.estimatedCostToGoal(node.state)
    }

  private val Infinity = Math.MAX_DOUBLE
  final case class RBFSFailure(fVal: Double) extends SearchResult

  //returns success, failure AND a new f-cost limit
  private def RBFS[S, A](problem: Problem[S,A], node: Node[S,A], fLimit: Double): SearchResult[A] = {
    println("print " + node.state)
    if(problem.goalTest(node.state)) Success(node.solution)
    val successors = expand(node,problem)
    successors.foreach( (node) => println("successors " + node.state))
    if(successors.isEmpty) RBFSFailure(Infinity)
    else {
      val successorsPq = new PriorityQueueQueue[(Double,Node[S,A])](
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

  def RecursiveBestFirstSearch[S, A](problem: Problem[S,A]) =
    RBFS(problem,Node[S,A](problem.initialState),Infinity) match {
      case Success(x) => Success(x)
      case _ => Failure() }

*/

}

object Local {

  //returns state that maximizes the value
  def HillClimbingSearch[S, A](problem: Problem[S,A], value: (S)=>Double) = {

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

  def SimulatedAnnealingSearch[S, A](problem: Problem[S,A],value: (S)=>Double, schedule: (Int)=>Double) = {

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
