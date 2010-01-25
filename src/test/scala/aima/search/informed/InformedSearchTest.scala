package aima.search.informed

import org.scalatest.Suite
import org.scalatest.ImpSuite

import aima.search._

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
      case Success(x) => assert(x == List(Go(RomaniaMapFactory.Sibiu), Go(RomaniaMapFactory.RimnicuVilcea), Go(RomaniaMapFactory.Pitesti), Go(RomaniaMapFactory.Bucharest)))
      case _ => assert(false)
    }
  } 
}
