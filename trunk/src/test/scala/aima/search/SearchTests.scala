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
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case CutOff() | Failure() => assert(false) 
    }
  }

  def testDFS() {
    val problem = new NQueensProblem(8)
    
    Uninformed.DepthFirstSearch(problem) match {
      case Success(x) => assert(x == List(Put(1), Put(5), Put(8), Put(6), Put(3), Put(7), Put(2), Put(4)))
      case CutOff() | Failure() => assert(false)
    }
  }

  def testDLS() {
    Uninformed.DepthLimitedSearch(new NQueensProblem(8),8) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case CutOff() | Failure() => assert(false)
    }

    Uninformed.DepthLimitedSearch(new NQueensProblem(8),7) match {
      case CutOff() => assert(true)
      case _ => assert(false)
    }
  }

  def testIDS() {
    Uninformed.IterativeDeepeningSearch(new NQueensProblem(8)) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case CutOff() | Failure() => assert(false)
    }
  }

  def testBFGS() {
    val problem = new NQueensProblem(8)
    
    Uninformed.BreadthFirstGraphSearch(problem) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case CutOff() | Failure() => assert(false) 
    }
  }

  def testDFGS() {
    val problem = new NQueensProblem(8)
    
    Uninformed.DepthFirstGraphSearch(problem) match {
      case Success(x) => assert(x == List(Put(1), Put(5), Put(8), Put(6), Put(3), Put(7), Put(2), Put(4)))
      case CutOff() | Failure() => assert(false)
    }
  }

}

// BreadthFirstGraphSearch Test
class BFGStest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    Success(List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
    Uninformed.BreadthFirstGraphSearch(p) match {
      case Success(x) => assert(x == List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
      case CutOff() | Failure() => assert(false)
    }
  }
}

// DepthFirstGraphSearch Test
class DFGStest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    Uninformed.DepthFirstGraphSearch(p) match {
      case Success(x) => assert(x.last == Go('Bucharest)) //it'll find different path in different execution
      case CutOff() | Failure() => assert(false)
    }
  }
}

// UniformCostSearch Test
class UniformCostSearchTest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    Success(List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
    Uninformed.UniformCostSearch(p) match {
      case Success(x) => assert(x == List(Go('Sibiu), Go('Rimnicu_Vilcea), Go('Pitesti), Go('Bucharest)))
      case CutOff() | Failure() => assert(false)
    }
  }
}

// GreedyBestFirstSearch Test
class GreedyBestFirstSearchTest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    Informed.GreedyBestFirstSearch(p) match {
      case Success(x) => assert(x == List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.Fagaras), Go(RomaniaMapFactory.Bucharest)))
      case CutOff() | Failure() => assert(false)
    }
  }
}

// AStarSearch Test
class AStarSearchTest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    Informed.AStarSearch(p) match {
      case Success(x) => assert(x == List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.RimnicuVilcea), Go(RomaniaMapFactory.Pitesti), Go(RomaniaMapFactory.Bucharest)))
      case CutOff() | Failure() => assert(false)
    }
  }
}

// RecursiveBestFirstSearch Test
/* class RecursiveBestFirstSearchTest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    Informed.RecursiveBestFirstSearch(p) match {
      case Success(x) => println(x); assert(x == List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.RimnicuVilcea), Go(RomaniaMapFactory.Pitesti), Go(RomaniaMapFactory.Bucharest)))
      case CutOff() | Failure() => println("no result found"); assert(false)
    }
  } 
}*/

//Online DFS Tests
class OnlineDFSTest extends Suite {

  def testAIMA2eFig4_18() {
    var result:List[Go[String]] = List()
    val map = new LocationMap[String]()
    map.addPath("1,1", "1,2", 1.0);
    map.addPath("1,1", "2,1", 1.0);
    map.addPath("2,1", "3,1", 1.0);
    map.addPath("2,1", "2,2", 1.0);
    map.addPath("3,1", "3,2", 1.0);
    map.addPath("2,2", "2,3", 1.0);
    map.addPath("3,2", "3,3", 1.0);
    map.addPath("2,3", "1,3", 1.0);

    val problem = new OnlineSearchMapProblem[String](map,In("3,3"))
    val agent = new OnlineDFSMapAgent[String](problem,In("1,1"))

    val env = new MapEnvironment[OnlineDFSMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.stepUntilNoOp()
    assert(result.reverse == List(Go(1,2), Go(1,1), Go(2,1), Go(3,1), Go(3,2), Go(3,1), Go(2,1), Go(2,2), Go(2,3), Go(2,2), Go(2,1), Go(1,1), Go(2,1), Go(2,2), Go(2,3), Go(1,3), Go(2,3), Go(1,3), Go(2,3), Go(2,2), Go(2,1), Go(3,1), Go(3,2), Go(3,3)))
  }
}
