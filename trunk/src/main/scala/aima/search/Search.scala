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
final case class Failure extends SearchResult[Nothing]
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

  //TODO: correct evaluation of path cost
  private def expand[S <: State, A <: Action](node: Node[S,A], problem: Problem[S,A]) =
    problem.successorFn(node.state).map(
      (t: Tuple2[A,S]) => Node(t._2, Some(node), Some(t._1), node.depth+1, 0))


  def BreadthFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) = TreeSearch(problem, new FifoFringe[Node[S,A]]())

  def DepthFirstSearch[S <: State, A <: Action](problem: Problem[S,A]) = TreeSearch(problem, new LifoFringe[Node[S,A]]())

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

   private def GraphSearch[S <: State,A <: Action](problem: Problem[S,A], fringe: Fringe[Node[S,A]]) = {

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

  def BreadthFirstGraphSearch[S <: State, A <: Action](problem: Problem[S,A]) = GraphSearch(problem, new FifoFringe[Node[S,A]]())

  def DepthFirstGraphSearch[S <: State, A <: Action](problem: Problem[S,A]) = GraphSearch(problem, new LifoFringe[Node[S,A]]())
  //others to come
}
