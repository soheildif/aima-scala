package aima.search

//Tests
import org.scalatest.Suite
import org.scalatest.ImpSuite

class BreadthFirstTreeSearchTest {

  def test8QueensProblem() { //successful search test
    BreadthFirstTreeSearch(new NQueensProblem(8)) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assert(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    BreadthFirstTreeSearch(new NQueensProblem(3)) match {
      case Failure() => assert(true)
      case _ => assert(false)
    }
  }
}

class BreadthFirstGraphSearchTest extends Suite {

  def test8QueensProblem() { //successful search test
    BreadthFirstGraphSearch(new NQueensProblem(8)) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assert(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    BreadthFirstGraphSearch(new NQueensProblem(3)) match {
      case Failure() => assert(true)
      case _ => assert(false)
    }
  }

  def testRomaniaMapAradToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    Success(List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
    BreadthFirstGraphSearch(p) match {
      case Success(x) => assert(x == List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
      case _ => assert(false)
    }
  }
}

class DepthFirstTreeSearchTest {

  def test8QueensProblem() { //successful search test
    DepthFirstTreeSearch(new NQueensProblem(8)) match {
      case Success(x) => assert(x == List(Put(1), Put(5), Put(8), Put(6), Put(3), Put(7), Put(2), Put(4)))
      case _ => assert(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    DepthFirstTreeSearch(new NQueensProblem(3)) match {
      case Failure() => assert(true)
      case _ => assert(false)
    }
  }
}

class DepthFirstGraphSearchTest extends Suite {

  def test8QueensProblem() { //successful search test
    DepthFirstGraphSearch(new NQueensProblem(8)) match {
      case Success(x) => assert(x == List(Put(1), Put(5), Put(8), Put(6), Put(3), Put(7), Put(2), Put(4)))
      case _ => assert(false) 
    }
  }

  def test3QueensProblem() { //unsuccessful search test
    DepthFirstGraphSearch(new NQueensProblem(3)) match {
      case Failure() => assert(true)
      case _ => assert(false)
    }
  }

  def testRomaniaMapAradToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    DepthFirstGraphSearch(p) match {
      case Success(x) => assert(x.last == Go('Bucharest)) //it'll find different path in different execution
      case _  => assert(false)
    }
  }
}

class UniformCostSearchTest extends Suite {

  def testRomaniaMapAradToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    //Success(List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
    UniformCostSearch(p) match {
      case Success(x) => assert(x == List(Go('Sibiu), Go('Rimnicu_Vilcea), Go('Pitesti), Go('Bucharest)))
      case _  => assert(false)
    }
  }

  //test described in Fig 3.15
  def testRomaniaMapSibiuToBucharest() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Sibiu), In(RomaniaMapFactory.Bucharest))
    UniformCostSearch(p) match {
      case Success(x) => assert(x == List(Go('Rimnicu_Vilcea), Go('Pitesti), Go('Bucharest)))
      case _  => assert(false)
    }
  }
}

class DepthLimitedSearchTest extends Suite {

  def testSuccessful() {
    DepthLimitedSearch(new NQueensProblem(8),8) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assert(false)
    }
  }

  def testCutoff() {
    DepthLimitedSearch(new NQueensProblem(8),7) match {
      case CutOff() => assert(true)
      case _ => assert(false)
    }
  }

  def testFailure() {
    DepthLimitedSearch(new NQueensProblem(3),5) match {
      case Failure() => assert(true)
      case _ => assert(false)
    }
  }
}

class IterativeDeepeningSearchTest extends Suite {

  def testIt() {
    IterativeDeepeningSearch(new NQueensProblem(8)) match {
      case Success(x) => assert(x == List(Put(8), Put(4), Put(1), Put(3), Put(6), Put(2), Put(7), Put(5)))
      case _ => assert(false)
    }
  }
}


class GreedyBestFirstSearchTest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    GreedyBestFirstSearch(p) match {
      case Success(x) => assert(x == List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.Fagaras), Go(RomaniaMapFactory.Bucharest)))
      case CutOff() | Failure() => assert(false)
    }
  }
}

// AStarSearch Test
class AStarSearchTest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    AStarSearch(p) match {
      case Success(x) => assert(x == List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.RimnicuVilcea), Go(RomaniaMapFactory.Pitesti), Go(RomaniaMapFactory.Bucharest)))
      case CutOff() | Failure() => assert(false)
    }
  }
}

// RecursiveBestFirstSearch Test
class RecursiveBestFirstSearchTest extends Suite {

  def testRomaniaMap() {
    val p = new MapProblem(RomaniaMapFactory.createRomaniaMap(), In(RomaniaMapFactory.Arad), In(RomaniaMapFactory.Bucharest))
    RecursiveBestFirstSearch(p) match {
      case Success(x) => println(x); assert(x == List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.RimnicuVilcea), Go(RomaniaMapFactory.Pitesti), Go(RomaniaMapFactory.Bucharest)))
      case _ => assert(false)
    }
  } 
}

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
    assert(result.reverse == List(Go("1,2"), Go("1,1"), Go("2,1"), Go("3,1"), Go("3,2"), Go("3,1"), Go("2,1"), Go("2,2"), Go("2,3"), Go("2,2"), Go("2,1"), Go("1,1"), Go("2,1"), Go("2,2"), Go("2,3"), Go("1,3"), Go("2,3"), Go("1,3"), Go("2,3"), Go("2,2"), Go("2,1"), Go("3,1"), Go("3,2"), Go("3,3")))
  }
}

class LRTAStarTest extends Suite {

  private def map = {
    val aMap = new LocationMap[String]()
    aMap.addPath("A", "B", 4.0);
    aMap.addPath("B", "C", 4.0);
    aMap.addPath("C", "D", 4.0);
    aMap.addPath("D", "E", 4.0);
    aMap.addPath("E", "F", 4.0);

    aMap.addStraightLineDistance("F","F",0);
    aMap.addStraightLineDistance("F","A",1);
    aMap.addStraightLineDistance("F","B",1);
    aMap.addStraightLineDistance("F","C",1);
    aMap.addStraightLineDistance("F","D",1);
    aMap.addStraightLineDistance("F","E",1);
    aMap
  }

  def testAlreadyAtgoal() {
    var result:List[Go[String]] = List()

    val problem = new OnlineSearchMapProblem[String](map,In("A"))
    val agent = new LRTAStarMapAgent[String](problem,In("A"))

    val env = new MapEnvironment[LRTAStarMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.stepUntilNoOp()
    assert(result == List())
  }

  def testNormalSearch() {
    var result:List[Go[String]] = List()

    val problem = new OnlineSearchMapProblem[String](map,In("F"))
    val agent = new LRTAStarMapAgent[String](problem,In("A"))

    val env = new MapEnvironment[LRTAStarMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.stepUntilNoOp()
    assert(result.reverse == List(Go("B"), Go("A"), Go("B"), Go("C"), Go("D"), Go("C"), Go("B"), Go("A"), Go("B"), Go("C"), Go("D"), Go("E"), Go("D"), Go("E"), Go("F")))
  }

  def testNoPath() {
    val aMap = map
    aMap.addStraightLineDistance("G","G",0);
    aMap.addStraightLineDistance("G","A",1);
    aMap.addStraightLineDistance("G","B",1);
    aMap.addStraightLineDistance("G","C",1);
    aMap.addStraightLineDistance("G","D",1);
    aMap.addStraightLineDistance("G","E",1);
    aMap.addStraightLineDistance("G","F",1);
    
    var result:List[Go[String]] = List()

    val problem = new OnlineSearchMapProblem[String](aMap,In("G"))
    val agent = new LRTAStarMapAgent[String](problem,In("A"))

    val env = new MapEnvironment[LRTAStarMapAgent[String],String]()

    env.registerView( _ match {
                          case Some(a) => result = a :: result
                          case None => ;})
    env.addAgent(agent)
    env.step(14) //or else it'll run forever

    assert(result.reverse == List(Go("B"), Go("A"), Go("B"), Go("C"), Go("D"), Go("C"), Go("B"), Go("A"), Go("B"), Go("C"), Go("D"), Go("E"), Go("D"), Go("E")))
  }
}

class CSPTest extends Suite {

  def testBacktrackingSearchForAustraliaMapColorCSP() {
    import AustraliaMapColorCSP._

    CSPSolver.BacktrackingSearch(csp) match {
      case None => assert(false)
      case Some(assignment) => {
        assert(assignment.getOrElse(Wa,-1) == Blue)
        assert(assignment.getOrElse(Nt,-1) == Red)
        assert(assignment.getOrElse(Q,-1) == Blue)
        assert(assignment.getOrElse(Sa,-1) == Green)
        assert(assignment.getOrElse(Nsw,-1) == Red)
        assert(assignment.getOrElse(V,-1) == Blue)
        assert(assignment.getOrElse(T,-1) == Red ||
               assignment.getOrElse(T,-1) == Blue ||
               assignment.getOrElse(T,-1) == Green)
      }
    }
  }

  def testBacktrackingSearchForNQueensCSP() {
    
    val csp = NQueensCSP.csp(8)
    CSPSolver.BacktrackingSearch(csp) match {
      case None => assert(false)
      case Some(assignment) =>
        println("Nqueeens solution found: " + csp.toString(assignment))        
        assert(true)
    }
  }

  def testMinConflictsForAustraliaMapColorCSP() {
    CSPSolver.MinConflicts(AustraliaMapColorCSP.csp,100) 
  }
}
