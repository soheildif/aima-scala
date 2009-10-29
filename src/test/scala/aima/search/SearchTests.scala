package aima.search

//Tests
import org.scalatest.Suite


class NQueensProblemTest {

  def testP() {
    var problem = new NQueensProblem(8)
    val x = problem.successorFn(problem.initialState)
    println(x)
    x match {
      case Nil => println("Fatal: shouldn't have happened")
      case y :: rest => println(problem.successorFn((y._2)))
    }
  }

}


class SearchTest extends Suite {

  def testBFS() {
    val problem = new NQueensProblem(8)
    
    Uninformed.BreadthFirstSearch(problem) match {
      case Left(_) => assert(false)
      case Right(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
    }
  }

  def testDFS() {
    val problem = new NQueensProblem(8)
    
    Uninformed.DepthFirstSearch(problem) match {
      case Left(_) => assert(false)
      case Right(x) => assert(x == List(Put(1), Put(5), Put(8), Put(6), Put(3), Put(7), Put(2), Put(4)))
    }
  }

  def testDLS() {
    Uninformed.DepthLimitedSearch(new NQueensProblem(8),7) match {
      case Left(_) => assert(false)
      case Right(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
    }
  }

}
