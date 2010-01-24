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
    val pathCost = parent.pathCost + problem.stepCost(parent.state,action,state)
    new Node[S,A](state,Some(parent),Some(action),parent.depth+1,pathCost)
  }
}


/* Possible search results that various search algorithms can return */
sealed abstract class SearchResult[A]
case class Success[A](result: A) extends SearchResult[A]
case class Failure[A] extends SearchResult[A]
case class CutOff[A] extends SearchResult[A]


/*************************************************************************************
 * 
 ************************* Uninformed search algorithms ******************************
 * 
 *************************************************************************************/

/* Tree-Search, described in Fig 3.7 */
object TreeSearch {
  def apply[S,A](problem: Problem[S,A], frontier: Queue[Node[S,A]]) = {
    def loop(frontier:Queue[Node[S,A]]): SearchResult[List[A]] =
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
}
 
/* Graph-Search, described in Fig 3.7 */
object GraphSearch {
  def apply[S,A](problem: Problem[S,A], frontier: Queue[Node[S,A]]) = {

    //TODO: make "explored" a hash based data structure so that lookup
    //is O(1)
    def loop(frontier:Queue[Node[S,A]], explored: List[S]): SearchResult[List[A]] =
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
}

/* Breadth-First-Search based on Tree-Search */
object BreadthFirstTreeSearch {
  def apply[S,A](problem: Problem[S,A]) =
    TreeSearch(problem, new FifoQueue[Node[S,A]]())
}

/* Breadth-First-Search based on Graph-Search */
object BreadthFirstGraphSearch {
  def apply[S,A](problem: Problem[S,A]) =
    GraphSearch(problem, new FifoQueue[Node[S,A]]())
}

/* Depth-First-Search based on Tree-Search */
object DepthFirstTreeSearch {
  def apply[S,A](problem: Problem[S,A]) =
    TreeSearch(problem, new LifoQueue[Node[S,A]]())
}

/* Depth-First-Search based on Graph-Search */
object DepthFirstGraphSearch {
  def apply[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new LifoQueue[Node[S,A]]())
}

/* Uniform-Cost-Search, described in Fig 3.14 */
object UniformCostSearch {
  def apply[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      that.pathCost.compare(node.pathCost)
      }))
}

/* Depth-Limited-Search, described in Fig 3.17 */
object DepthLimitedSearch {
  def apply[S, A](problem: Problem[S,A], limit: Int) =
    recursiveDLS(Node[S,A](problem.initialState),problem,limit)

  /* Recursive-DLS, described in Fig 3.17 */
  private def recursiveDLS[S, A](node: Node[S,A], problem: Problem[S,A], limit: Int): SearchResult[List[A]] = {
    if (problem.goalTest(node.state)) Success(node.solution) //success
    else {
      if (node.depth == limit) CutOff() //cut-off limit reached
      else {
        def loop(nodes: List[Node[S,A]], cutoffOccured: Boolean): SearchResult[List[A]] = 
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
}

/* Iterative-Deepening-Search, described in Fig 3.18 */
object IterativeDeepeningSearch {
  def apply[S, A](problem: Problem[S,A]) = {
    def loop(depth: Int): SearchResult[List[A]] =
      DepthLimitedSearch(problem,depth) match {
        case CutOff() => loop(depth + 1)
        case Failure() => Failure()
        case Success(actions) => Success(actions)
      }
    loop(0)
  }
}


/*************************************************************************************
 * 
 ************************* Informed search algorithms ******************************
 * 
 *************************************************************************************/

/* Greedy-Best-First-Search, described in Section 3.5.1 */
object GreedyBestFirstSearch {
  def apply[S,A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      GreedyBestFirstHeuristic(that,problem).compare(GreedyBestFirstHeuristic(node,problem))
      }))

  private def GreedyBestFirstHeuristic[S,A](node: Node[S,A],problem: Problem[S,A]) =
    problem.estimatedCostToGoal(node.state) //h(n)
}

/* A* Search, described in section 3.5.2 */
object AStarSearch {
  def apply[S, A](problem: Problem[S,A]) =
    GraphSearch(problem, new PriorityQueue[Node[S,A]](
      (node) => new Ordered[Node[S,A]] {
                    def compare(that: Node[S,A]) =
                      AStarHeuristic(that,problem).compare(AStarHeuristic(node,problem))
      }))

  private def AStarHeuristic[S,A](node: Node[S,A],problem: Problem[S,A]) =
    node.pathCost + problem.estimatedCostToGoal(node.state) //f(n) = g(n) + h(n)
}

/* Recursive-Best-First-Search, described in Fig 3.26 */
object RecursiveBestFirstSearch {

  def apply[S, A](problem: Problem[S,A]): SearchResult[List[A]] =
    RBFS(problem,RBFSNode(problem,problem.initialState),Infinity) match {
      case Success(x) => Success(x)
      case _ => Failure()
    }

  private val Infinity = Math.MAX_DOUBLE
  private final case class RBFSFailure[A](fVal: Double) extends Failure[A]
  
  //RBFS-Node is a special purpose node that stores node's fVal in a variable 
  //along with what is stored in a regular Node
  private final class RBFSNode[S,A](state: S, parent: Option[Node[S,A]], action: Option[A], depth: Int, pathCost: Double, var fVal: Double)
        extends Node[S,A](state,parent,action,depth,pathCost) {
          override def toString = "(" + state.toString + " , " + fVal.toString + ")"
        } 
  private object RBFSNode {
    def apply[S,A](problem: Problem[S,A],state: S) =
      new RBFSNode[S,A](state,None,None,0,0,problem.estimatedCostToGoal(state))

    def childNode[S,A](problem: Problem[S,A],parent: RBFSNode[S,A],action: A) = {
      val state = problem.result(parent.state,action)
      val pathCost = parent.pathCost + problem.stepCost(parent.state,action,state)
      val fVal = pathCost + problem.estimatedCostToGoal(state)
      new RBFSNode[S,A](state,Some(parent),Some(action),parent.depth+1,pathCost,fVal)
    }
  }

  /* RBFS, described in Fig 3.26
   * returns Success or RBFSFailure AND a new f-cost limit
   */
  private def RBFS[S, A](problem: Problem[S,A], node: RBFSNode[S,A], fLimit: Double): SearchResult[List[A]] = {
    if(problem.goalTest(node.state)) Success(node.solution)
    else {
      val successors = problem.actions(node.state).map((a:A)=>RBFSNode.childNode(problem,node,a))
      if(successors.length == 0) RBFSFailure(Infinity)
      else {
        //update fValue of any node from previous search
        successors.foreach((n) => n.fVal = Math.max(n.fVal,node.fVal))

        //storing the nodes in a PriorityQueue in increasing order
        //of fVal for easy access of best and next best nodes
        val successorsPq = new PriorityQueue[RBFSNode[S,A]](
          (arg) => {
            new Ordered[RBFSNode[S,A]] { def compare(that: RBFSNode[S,A]) = that.fVal.compare(arg.fVal) }
          })
        successorsPq.insertAll(successors)

        def loop: SearchResult[List[A]] = {
          val Some(bestNode) = successorsPq.nth(0)
          if(bestNode.fVal > fLimit) RBFSFailure(bestNode.fVal)
          else {
            val alternative_fVal =
              successorsPq.nth(1) match {
                case Some(n) => n.fVal
                case None => bestNode.fVal
              }

            RBFS(problem,bestNode,Math.min(fLimit, alternative_fVal)) match {
              case Success(a) => Success(a)
              case RBFSFailure(f) =>
                //remove the best node, reset its fVal and re-insert it to
                //the PriorityQueue, removal-insertion is necessary so
                //that at insert this node is placed according to its new
                //fVal in the PriorityQueue
                val Some(currentBest) = successorsPq.removeFirst
                currentBest.fVal = f
                successorsPq.insert(currentBest)
                loop
              case _ => throw new IllegalStateException("This can not happen.")
            }
          }
        }
        loop
      }
    }
  }
}
