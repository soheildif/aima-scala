package aima.search

//State
abstract class State

//Action
abstract class Action


//Problem
abstract class Problem[S <: State, A <: Action](initState: S){
  def initialState: S = initState
  def goalTest(s: S): Boolean
  def successorFn(s: S): List[(A,S)]
}

//******************************************************************
//************************ Example Problems ************************
//******************************************************************

// ** N-Queens Problem **
case class Put(y: Int) extends Action

/*
 * NQueensState represents state of the board containing 0 or more queens
 * on the board.
 * 
 * size: Size of the board
 * co-ordinates x,y go from 1 to size, both start at bottom left corner of the board
 * 
 * queens: current state of queens on the board, queens = List(5,4,3) means
 * 3 queens at following positions
 * X: 3 2 1
 * Y: 5 4 3
 *  
 * */
class NQueensState private (size: Int, queens: List[Int]) extends State {

  def numQueens = queens.length

  //add a queen in next available column
  def executeAction(a: Put) = {
    if (numQueens == size)
      throw new IllegalStateException("Can't place one more queen. Board is full.")
    else
      a match {
        case Put(y) => new NQueensState(size, y :: queens)
      }
  }

  // Checks if the action is safe
  def isSafe(action: Put): Boolean = {
    //check if two positions are attacking each other
    def isAttacking(x1: Int, y1: Int, x2: Int, y2: Int) =
      !(x1 == x2 && y1 == y2) && ( x1 == x2 || y1 == y2 || Math.abs(x1-x2) == Math.abs(y1-y2))

    val len = numQueens
    val newX = len + 1
    val Put(newY) = action

    def loop(me: List[Int], x: Int): Boolean =
      me match {
        case Nil => true
        case y :: rest if isAttacking(x,y,newX,newY) => false
        case _ :: rest => loop(rest, x-1)
      }

    loop(queens,len)
  }

  //printable board representation
  override def toString() = {
    var result = ""
    val tmp = queens.reverse
    val len = numQueens

    for(y <- 1 to size) {
      for(x <- 1 to size) {
        if(x <= len && tmp(x-1) == (size+1-y))
          result = result + "X "
        else
          result = result + "- "
      }
      result = result + "\n"
    }
    result
  } 
}
object NQueensState {
  def apply(size: Int) = new NQueensState(size,List())
}

class NQueensProblem(size: Int) extends Problem[NQueensState,Put](NQueensState(size)) {

  override def goalTest(s: NQueensState) = (s.numQueens == size)

  override def successorFn(s: NQueensState): List[(Put,NQueensState)] = {

    def loop(i:Int, successors: List[(Put,NQueensState)]): List[(Put,NQueensState)] = {
      if (i > size) successors
      else {
        if (s.isSafe(Put(i)))
          loop(i+1, (Put(i),s.executeAction(Put(i))) :: successors)
        else loop(i+1, successors)
      }
    }
    loop(1,Nil)
  }
}

// ** Route finding Map based problem **
class LocationMap {

  import scala.collection.mutable.Map

  private val map: Map[Symbol,Map[Symbol,Double]] = Map()
  
  def addLocation(location: Symbol) {
    if(!map.contains(location))
      map + Tuple(location, Map())
  }
  
  def addPath(from: Symbol, to: Symbol, cost: Double, bidirectional: Boolean) {
    if(map.contains(from) && map.contains(to)) {
      map.getOrElse(from,null) + Tuple(to,cost)
      if(bidirectional) map.getOrElse(to,null) + Tuple(from,cost)
    }
    else
      throw new IllegalArgumentException(from + " and " + to + " must be added.")
  }
  
  //Add bi-directional path -- will be removed once scala-2.8 is in 
  //Scala-2.8 has concept of providing defaul value
  //to arguments, then we will not need this overloaded method
  //instead the above method signature will become
  //def addPath(from: Symbol, to: Symbol, cost: Double, bidirectional: Boolean = true)
  def addPath(from: Symbol, to: Symbol, cost: Double) { addPath(from,to,cost,true) }

  def getLocationsReachableFrom(from: Symbol): List[Symbol] =
    map.get(from) match {
      case Some(x) => x.keys.toList
      case None => throw new IllegalStateException(from + " is not in the map.")
    }
}


case class In(val location: Symbol) extends State
case class Go(val location: Symbol) extends Action

class MapProblem(locationMap: LocationMap, initState: In, goalState: In) extends Problem[In,Go](initState){

  override def goalTest(s: In): Boolean = s == goalState

  override def successorFn(s: In): List[(Go,In)] =
    s match {
      case In(x) =>
        locationMap.getLocationsReachableFrom(x).map(
          (s:Symbol) => (Go(s),In(s)))
    }
}

object ExampleMapFactory {
  
  def createRomaniaMap() = {
    val result = new LocationMap()

    //add all Romania cities
    result.addLocation('Oradea);
    result.addLocation('Zerind);
    result.addLocation('Arad);
    result.addLocation('Timisoara);
    result.addLocation('Lugoj);
    result.addLocation('Mehadia);
    result.addLocation('Dobreta);
    result.addLocation('Sibiu);
    result.addLocation('Rimnicu_Vilcea);
    result.addLocation('Craiova);
    result.addLocation('Fagaras);
    result.addLocation('Pitesti);
    result.addLocation('Bucharest);
    result.addLocation('Giurgiu);
    result.addLocation('Neamt);
    result.addLocation('Iasi);
    result.addLocation('Vaslui);
    result.addLocation('Urziceni);
    result.addLocation('Hirsova);
    result.addLocation('Eforie);
    
    //add various paths
    result.addPath('Oradea, 'Zerind, 71);
    result.addPath('Arad, 'Zerind, 75);
    result.addPath('Arad, 'Timisoara, 118);
    result.addPath('Timisoara, 'Lugoj, 111);
    result.addPath('Lugoj, 'Mehadia, 70);
    result.addPath('Mehadia, 'Dobreta, 75);
    result.addPath('Oradea, 'Sibiu, 151);
    result.addPath('Arad, 'Sibiu, 140);
    result.addPath('Dobreta, 'Craiova, 120);
    result.addPath('Sibiu, 'Fagaras, 99);
    result.addPath('Sibiu, 'Rimnicu_Vilcea, 80);
    result.addPath('Rimnicu_Vilcea, 'Craiova, 146);
    result.addPath('Rimnicu_Vilcea, 'Pitesti, 97);
    result.addPath('Craiova, 'Pitesti, 138);
    result.addPath('Fagaras, 'Bucharest, 211);
    result.addPath('Pitesti, 'Bucharest, 101);
    result.addPath('Bucharest, 'Giurgiu, 90);
    result.addPath('Bucharest, 'Urziceni, 85);
    result.addPath('Neamt, 'Iasi, 87);
    result.addPath('Iasi, 'Vaslui, 92);
    result.addPath('Vaslui, 'Urziceni, 142);
    result.addPath('Urziceni, 'Hirsova, 98);
    result.addPath('Hirsova, 'Eforie, 86);

    result
  }
}


