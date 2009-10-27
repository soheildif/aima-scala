package aima.search

//Node :  a node in the search tree
class Node[A <: State](val state: A, val parent: Option[Node[A]], 
           val action: Option[Action], val depth: Int, val pathCost: Double) {
  
  def solution: List[Action] = {
    def loop(node: Node[A], a: List[Action]): List[Action] =
      node.parent match {
        case None => a
        case Some(x) => loop(x, node.action.get :: a)
      }
    loop(this, Nil)
  }
}

object Node {
  def apply[A <: State](state:A) = new Node[A](state,None,None,0,0.0)

  def apply[A <: State](state: A, parent: Option[Node[A]], action: Option[Action], depth: Int) =
    new Node[A](state,parent,action,depth,0.0)

  def apply[A <: State](state: A, parent: Option[Node[A]], action: Option[Action], depth: Int, pathCost: Double) =
    new Node[A](state,parent,action,depth,pathCost)
}


//Uninformed search algorithms
object Uninformed {

  //Tree Search
  def TreeSearch[A <: State](problem: Problem[A], fringe: Fringe[Node[A]]) = {

    def loop(fringe:Fringe[Node[A]]): Option[List[Action]] =
      fringe.removeFirst match {
        case None => None
        case Some(node) if problem.goalTest(node.state) => 
          println(node.state.toString()) //print the state when goal is reached
          Some(node.solution)
        case Some(node) => loop(fringe.insertAll(expand(node,problem)))
      }
    
    loop(fringe.insert(Node(problem.initialState)))
  }

  //TODO: correct evaluation of path cost
  private def expand[A <: State](node: Node[A], problem: Problem[A]) =
    problem.successorFn(node.state).map(
      (t: Tuple2[Action,A]) => Node(t._2, Some(node), Some(t._1), node.depth+1, 0))


  def BreadthFirstSearch[A <: State](problem: Problem[A]) = TreeSearch(problem, new FifoFringe[Node[A]]())

  def DepthFirstSearch[A <: State](problem: Problem[A]) = TreeSearch(problem, new LifoFringe[Node[A]]())

  //others to come
}
