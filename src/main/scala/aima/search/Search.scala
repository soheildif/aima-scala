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

  def childNode[S,A](problem: Problem[S,A], parent: Node[S,A], action: A) = {
    val state = problem.result(parent.state,action)
    //TODO: change step cost signature
    val pathCost = parent.pathCost + problem.stepCost(parent.state,state)
    new Node[S,A](state,Some(parent),Some(action),parent.depth+1,pathCost)
  }
}


/* Possible search results that various search algorithms can return */
sealed abstract class SearchResult
final case class Success[A](actions: List[A]) extends SearchResult
final case class Failure extends SearchResult
final case class CutOff extends SearchResult

/* ------ Uninformed search algorithms --------- */
object Uninformed {  

  /* Tree-Search as described in Fig 3.7 */
  def TreeSearch[S,A](problem: Problem[S,A], frontier: Queue[Node[S,A]]) = {

    def loop(frontier:Queue[Node[S,A]]): SearchResult =
      frontier.removeFirst match {
        case None => Failure()
        case Some(node) if problem.goalTest(node.state) => 
          //println(node.state.toString())
          Success(node.solution)
        case Some(node) => {
          problem.actions(node.state).foreach( (a:A) => frontier.insert(Node.childNode(problem,node,a)))
          loop(frontier)
        }
      }
    
    loop(frontier.insert(Node(problem.initialState)))
  }

  /* Graph-Search as described in Fig 3.7 */
  def GraphSearch[S,A](problem: Problem[S,A], frontier: Queue[Node[S,A]]) = {

    //TODO: make "explored" a hash based data structure so that lookup
    //is O(1)
    def loop(frontier:Queue[Node[S,A]], explored: List[S]): SearchResult =
      frontier.removeFirst match {
        case None => Failure()
        case Some(node) if problem.goalTest(node.state) => 
          //println(node.state.toString())
          Success(node.solution)
        case Some(node) => 
          if(explored.exists(_ == node.state)) 
             loop(frontier,explored)
          else {
            problem.actions(node.state).foreach((a:A) => frontier.insert(Node.childNode(problem,node,a)))
            loop(frontier, node.state :: explored)
          }
      }
    
    loop(frontier.insert(Node(problem.initialState)),Nil)
  }

  //TODO: see if it can be removed altogether
  def expand[S, A](node: Node[S,A], problem: Problem[S,A]) =
    problem.successorFn(node.state).map(
      (t: Tuple2[A,S]) => Node(t._2, Some(node), Some(t._1), node.depth+1, 
                               node.pathCost + problem.stepCost(node.state,t._2)))

  /* Breadth-First-Search based on Tree-Search */
  def BreadthFirstTreeSearch[S, A](problem: Problem[S,A]) = TreeSearch(problem, new FifoQueue[Node[S,A]]())

  /* Breadth-First-Search based on Graph-Search */
  def BreadthFirstGraphSearch[S, A](problem: Problem[S,A]) = GraphSearch(problem, new FifoQueue[Node[S,A]]())

  /* Depth-First-Search based on Tree-Search */
  def DepthFirstTreeSearch[S, A](problem: Problem[S,A]) = TreeSearch(problem, new LifoQueue[Node[S,A]]())

  /* Depth-First-Search based on Graph-Search */
  def DepthFirstGraphSearch[S, A](problem: Problem[S,A]) = GraphSearch(problem, new LifoQueue[Node[S,A]]())

  /* Uniform-Cost-Search, described in Fig 3.14 */
  def UniformCostSearch[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      that.pathCost.compare(node.pathCost)
      }))

  /* Depth-Limited-Search, described in Fig 3.17 */
  def DepthLimitedSearch[S, A](problem: Problem[S,A], limit: Int) =
    recursiveDLS(Node[S,A](problem.initialState),problem,limit)

  /* Recursive-DLS, described in Fig 3.17 */
  def recursiveDLS[S, A](node: Node[S,A], problem: Problem[S,A], limit: Int): SearchResult = {
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
        loop(problem.actions(node.state).map(Node.childNode(problem,node,_)), false)
      }
    }
  }

  /* Iterative-Deepening-Search, described in Fig 3.18 */
  def IterativeDeepeningSearch[S, A](problem: Problem[S,A]) = {
    def loop(depth: Int): SearchResult =
      DepthLimitedSearch(problem,depth) match {
        case CutOff() => loop(depth + 1)
        case Failure() => Failure()
        case Success(actions) => Success(actions)
      }
    loop(0)
  }
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
