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


sealed abstract class SearchResult
case final class Success[A <: Action](actions: List[A]) extends SearchResult
case final class Failure extends SearchResult
case final class CutOff extends SearchResult

//Uninformed search algorithms
object Uninformed {

  /**
   * Search Result --
   * Type: Either[Boolean,List[A <: Action]]
   *   Left(true) = FAILURE
   *   Left(false) = CUT-OFF
   *   Right(x) = success, x is List of Actions to be executed
   * */
  

  //Tree Search
  private def TreeSearch[S <: State,A <: Action](problem: Problem[S,A], fringe: Fringe[Node[S,A]]) = {

    def loop(fringe:Fringe[Node[S,A]]): Either[Boolean, List[A]] =
      fringe.removeFirst match {
        case None => Left(true)
        case Some(node) if problem.goalTest(node.state) => 
          println(node.state.toString()) //print the state when goal is reached
          Right(node.solution)
        case Some(node) => loop(fringe.insertAll(expand(node,problem)))
      }
    
    loop(fringe.insert(Node(problem.initialState)))
  }

  //TODO: correct evaluation of path cost
  private def expand[S <: State, A <: Action](node: Node[S,A], problem: Problem[S,A]) =
    problem.successorFn(node.state).map(
      (t: Tuple2[A,S]) => Node(t._2, Some(node), Some(t._1), node.depth+1, 0))


  def BreadthFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) = TreeSearch(problem, new FifoFringe[Node[S,A]]())

  def DepthFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) = TreeSearch(problem, new LifoFringe[Node[S,A]]())

  def DepthLimitedSearch[S <: State, A <: Action](problem: Problem[S,A], limit: Int) =
    recursiveDLS(Node[S,A](problem.initialState),problem,limit) match {
      case Left(x) => Left(x)
      case Right(node) =>
        println(node.state.toString()) //print the state when goal is reached
        Right(node.solution)
    }

  //TODO: does not seem to be correct
  private def recursiveDLS[S <: State, A <: Action](node: Node[S,A], problem: Problem[S,A], limit: Int): Either[Boolean,Node[S,A]] = {
    if (problem.goalTest(node.state)) Right(node) //success
    else {
      if (node.depth == limit) Left(false) //cut-off limit reached
      else {
        def loop(nodes: List[Node[S,A]]): Either[Boolean, Node[S,A]] = 
          nodes match {
            case Nil => Left(true) //failure
            case n :: rest => 
              recursiveDLS(n,problem,limit) match {
                case Left(true) => Left(true)
                case Left(false) => loop(rest)
                case Right(n) => Right(n)
              }
          }
        loop(expand(node,problem))
      }
    }
  }

  //actually IterativeDeepeningSearch needs to differentiate for cutoff, or
  //else it'll keep on trying indefinitely
/*  def IterativeDeepeningSearch[A <: State](problem: Problem[A]) = {
    def loop(depth: Int) =
      DepthLimitedSearch(depth) match {
        case None => loop(depth + 1) //cut-off
        case FAILURE => None //failed
        case Some(sol) => Some(sol) //solution found
      }
    loop(0)
  }
  */    
  //others to come
}
