package aima.search.uninformed

import org.scalatest.Suite
import org.scalatest.ImpSuite

import aima.search._

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
//    Success(List(Go('Sibiu), Go('Fagaras), Go('Bucharest)))
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
