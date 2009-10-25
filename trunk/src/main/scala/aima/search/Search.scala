package aima.search

//renaming String to Action for readability purposes
import java.lang.{String => Action}

//State
abstract class State

//Problem
abstract class Problem(initState: State, goalState: State){
  
  def initialState: State = initState
  def goalTest(s: State): Boolean
  def successorFn(s: State): List[(Action,State)]
}


//Node :  a node in the search tree
class Node(val state: State, val parent: Option[Node], 
           val action: Option[Action], val depth: Int, val pathCost: Double) {
  
  def solution: List[Action] = {
    def loop(node: Node, a: List[Action]): List[Action] =
      node.parent match {
        case None => a.reverse
        case Some(x) => loop(x, node.action.get :: a)
      }
    loop(this, Nil)
  }
}

object Node {
  def apply(state:State) = new Node(state,None,None,0,0.0)

  def apply(state: State, parent: Option[Node], action: Option[Action], depth: Int) =
    new Node(state,parent,action,depth,0.0)

  def apply(state: State, parent: Option[Node], action: Option[Action], depth: Int, pathCost: Double) =
    new Node(state,parent,action,depth,pathCost)
}

//Uninformed search algorithms
object Uninformed {

  //Tree Search
  def TreeSearch(problem: Problem, fringe: Fringe[Node]) = {

    def loop(fringe:Fringe[Node]): Option[List[Action]] =
      fringe.removeFirst match {
        case None => None
        case Some(node) if problem.goalTest(node.state) => Some(node.solution)
        case Some(node) => loop(fringe.insertAll(expand(node,problem)))
      }
    
    loop(fringe.insert(Node(problem.initialState)))
  }

  //TODO: correct evaluation of path cost
  private def expand(node: Node, problem: Problem) =
    problem.successorFn(node.state).map(
      (t: Tuple2[Action,State]) => Node(t._2, Some(node), Some(t._1), node.depth+1, 0))

  def BreadthFirstSearch(problem: Problem) = TreeSearch(problem, new LifoFringe[Node]())

  def DepthFirstSearch(problem: Problem) = TreeSearch(problem, new FifoFringe[Node]())

  //others to come
}
