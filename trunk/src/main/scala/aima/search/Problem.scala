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
  
  //provided one can go from TO to in single step, what is the
  //cost
  def stepCost(from: S, to: S): Double

  //estimated cost to reach goal from
  //given state ( h(n) )
  def estimatedCostToGoal(from: S): Double
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

  //to be implemented properly
  override def stepCost(from: NQueensState, to: NQueensState): Double = 0.0

  //to be implemented properly
  override def estimatedCostToGoal(from: NQueensState): Double = 0.0
}

// ** Route finding Map based problem **
class LocationMap[A] {

  import scala.collection.mutable.Map

  private val map: Map[A,Map[A,Double]] = Map()

  //straight line distance between various pair of locations
  private val stDist : Map[(A,A),Double] = Map()
  
  def addLocation(location: A) {
    if(!map.contains(location))
      map += Tuple(location, Map[A,Double]())
  }
  
  def addPath(from: A, to: A, cost: Double, bidirectional: Boolean) {
    //add locations if they are not there already
    addLocation(from)
    addLocation(to)
    
    map.getOrElse(from,null) += Tuple(to,cost)
    if(bidirectional) map.getOrElse(to,null) += Tuple(from,cost)
  }
  
  //Add bi-directional path -- will be removed once scala-2.8 is in 
  //Scala-2.8 has concept of providing defaul value
  //to arguments, then we will not need this overloaded method
  //instead the above method signature will become
  //def addPath(from: Symbol, to: Symbol, cost: Double, bidirectional: Boolean = true)
  def addPath(from: A, to: A, cost: Double) { addPath(from,to,cost,true) }

  def addStraightLineDistance(loc1: A, loc2: A, dist: Double) {
    //add locations if not there already
    addLocation(loc1)
    addLocation(loc2)
    
    stDist.contains((loc1,loc2)) match {
      case true => stDist += Tuple((loc1,loc2),dist)
      case false => stDist += Tuple((loc2,loc1),dist)
    }
  }

  def getLocationsReachableFrom(from: A): List[A] =
    map.get(from) match {
      case Some(x) => x.keys.toList
      case None => throw new IllegalStateException(from + " is not in the map.")
    }

  //path distance from To to, provided they are adjacent
  def distance(from: A, to: A): Double = {
    map.get(from) match {
      case Some(tmp) => tmp.get(to) match {
                          case Some(x) => x
                          case None => throw new IllegalStateException(from + " and " + to + " are not adjacent."); }
      case None => throw new IllegalStateException(from + " is not a location.");
    }
  }

  //straight line distance between from AND to
  def straightLineDistance(from: A, to: A): Double =
    stDist.contains((from,to)) match {
      case true => stDist.getOrElse((from,to),0)
      case false if stDist.contains((to,from)) => stDist.getOrElse((to,from),0)
      case _ => throw new IllegalStateException("straight line distance between " + from + " and " + to + " is not available.")
    } 
}


case class In[A](val location: A) extends State
case class Go[A](val location: A) extends Action

class MapProblem(locationMap: LocationMap[Symbol], initState: In[Symbol], goalState: In[Symbol]) extends Problem[In[Symbol],Go[Symbol]](initState){

  override def goalTest(s: In[Symbol]): Boolean = s == goalState

  override def successorFn(s: In[Symbol]): List[(Go[Symbol],In[Symbol])] =
    s match {
      case In(x) =>
        locationMap.getLocationsReachableFrom(x).map(
          (s:Symbol) => (Go(s),In(s)))
    }

  override def stepCost(from: In[Symbol], to: In[Symbol]): Double =
    (from,to) match {
      case (In(f),In(t)) => locationMap.distance(f,t) }

  override def estimatedCostToGoal(from: In[Symbol]): Double =
    (from,goalState) match {
      case (In(f),In(g)) => locationMap.straightLineDistance(f,g)}
}


object RomaniaMapFactory {

  val Oradea = 'Oradea
  val Zerind = 'Zerind
  val Arad = 'Arad
  val Timisoara = 'Timisoara
  val Lugoj = 'Lugoj
  val Mehadia = 'Mehadia
  val Dobreta = 'Dobreta
  val Sibiu = 'Sibiu
  val RimnicuVilcea = 'Rimnicu_Vilcea
  val Craiova = 'Craiova
  val Fagaras = 'Fagaras
  val Pitesti = 'Pitesti
  val Bucharest = 'Bucharest
  val Giurgiu = 'Giurgiu
  val Neamt = 'Neamt
  val Iasi = 'Iasi
  val Vaslui = 'Vaslui
  val Urziceni = 'Urziceni
  val Hirsova = 'Hirsova
  val Eforie = 'Eforie
  
  
  def createRomaniaMap() = {
    val result = new LocationMap[Symbol]()

    //add various paths
    result.addPath(Oradea, Zerind, 71);
    result.addPath(Arad, Zerind, 75);
    result.addPath(Arad, Timisoara, 118);
    result.addPath(Timisoara, Lugoj, 111);
    result.addPath(Lugoj, Mehadia, 70);
    result.addPath(Mehadia, Dobreta, 75);
    result.addPath(Oradea, Sibiu, 151);
    result.addPath(Arad, Sibiu, 140);
    result.addPath(Dobreta, Craiova, 120);
    result.addPath(Sibiu, Fagaras, 99);
    result.addPath(Sibiu,RimnicuVilcea, 80);
    result.addPath(RimnicuVilcea, Craiova, 146);
    result.addPath(RimnicuVilcea, Pitesti, 97);
    result.addPath(Craiova, Pitesti, 138);
    result.addPath(Fagaras, Bucharest, 211);
    result.addPath(Pitesti, Bucharest, 101);
    result.addPath(Bucharest, Giurgiu, 90);
    result.addPath(Bucharest, Urziceni, 85);
    result.addPath(Neamt, Iasi, 87);
    result.addPath(Iasi, Vaslui, 92);
    result.addPath(Vaslui, Urziceni, 142);
    result.addPath(Urziceni, Hirsova, 98);
    result.addPath(Hirsova, Eforie, 86);

    //add straight line distances
    result.addStraightLineDistance(Bucharest,Bucharest,0);
    result.addStraightLineDistance(Bucharest,Arad,366);
    result.addStraightLineDistance(Bucharest,Craiova,160);
    result.addStraightLineDistance(Bucharest,Dobreta,242);
    result.addStraightLineDistance(Bucharest,Eforie,161);
    result.addStraightLineDistance(Bucharest,Fagaras,176);
    result.addStraightLineDistance(Bucharest,Giurgiu,77);
    result.addStraightLineDistance(Bucharest,Hirsova,151);
    result.addStraightLineDistance(Bucharest,Iasi,226);
    result.addStraightLineDistance(Bucharest,Lugoj,244);
    result.addStraightLineDistance(Bucharest,Mehadia,241);
    result.addStraightLineDistance(Bucharest,Neamt,234);
    result.addStraightLineDistance(Bucharest,Oradea,380);
    result.addStraightLineDistance(Bucharest,Pitesti,100);
    result.addStraightLineDistance(Bucharest,RimnicuVilcea,193);
    result.addStraightLineDistance(Bucharest,Sibiu,253);
    result.addStraightLineDistance(Bucharest,Timisoara,329);
    result.addStraightLineDistance(Bucharest,Urziceni,80);
    result.addStraightLineDistance(Bucharest,Vaslui,199);
    result.addStraightLineDistance(Bucharest,Zerind,374);
    result
  }
}


