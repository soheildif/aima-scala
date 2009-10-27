package aima.search

//State
abstract class State

//Action
abstract class Action


//Problem
abstract class Problem[A <: State](initState: A){
  type B <: Action
  def initialState: A = initState
  def goalTest(s: A): Boolean
  def successorFn(s: A): List[(B,A)]
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
 * there are 3 queens on the board at positions (1,3), (2,4), (3,5) 
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

class NQueensProblem(size: Int) extends Problem[NQueensState](NQueensState(size)) {

  type B = Put

  override def goalTest(s: NQueensState) = (s.numQueens == size)

  override def successorFn(s: NQueensState): List[(B,NQueensState)] = {

    def loop(i:Int, successors: List[(B,NQueensState)]): List[(B,NQueensState)] = {
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

// ** Romania map problem **
/*case class In(val location: Symbol) extends State
case class Go(val location: Symbol) extends Action

class RomaniaMapProblem(initState: In, goalState: In) extends Problem(initState) {

  override def goalState(s: In): Boolean =
    (s,goalState) match {
      case (In(x),In(y)) if x==y  => true
      case _ => false
    }

  override def successorFn(s: In): List[(Go,In)] =
    locationMap(s.location).map((x: (Symbol,Int)) => (Go(x._1),In(x._1)))


  //adjacency list represenation of the map of romania
  import scala.collection.immutable.Map
  
  private val oradea = 'Oradea
  private val zerind = 'Zerind
  private val arad = 'Arad
  private val timisoara = 'Timisoara
  private val lugoj = 'Lugoj
  private val mehadia = 'Mehadia
  private val dobreta = 'Dobreta
  private val sibiu = 'Sibiu
  private val rimnicuVilcea = 'RimnicuVilcea
  private val craiova = 'Craiova
  private val fagaras = 'Fagaras
  private val pitesti = 'Pitesti
  private val bucharest = 'Bucharest
  private val giurgiu = 'Giurgiu
  private val neamt = 'Neamt
  private val iasi = 'Iasi
  private val vaslui = 'Vaslui
  private val urziceni = 'Urziceni
  private val hirsova = 'Hirsova
  private val eforie = 'Eforie

  private val locationMap = Map(oradea -> List((zerind,71), (sibiu,151)),
                              zerind -> List((oradea,71), (arad,75)),
                              arad -> List((zerind,75), (sibiu,140), (timisoara,118)),
                              timisoara -> List((arad,118), (lugoj,111)),
                              lugoj -> List((timisoara,111), (mehadia,70)),
                              mehadia -> List((lugoj,70), (dobreta,75)),
                              dobreta -> List((mehadia,75), (craiova,120)),
                              sibiu -> List((oradea,151),(arad,140),(fagaras,99),(rimnicuVilcea,80)),
                              rimnicuVilcea -> List((sibiu,80),(pitesti,97),(craiova,146)),
                              craiova -> List((dobreta,120),(rimnicuVilcea,146),(pitesti,138)),
                              fagaras -> List((sibiu,99),(bucharest,211)),
                              pitesti -> List((rimnicuVilcea,97),(craiova,138),(bucharest,101)),
                              bucharest -> List((fagaras,211),(pitesti,101),(giurgiu,90),(urziceni,85)),
                              giurgiu -> List((bucharest,90)),
                              neamt -> List((iasi, 87)),
                              iasi -> List((neamt,87), (vaslui,92)),
                              vaslui -> List((iasi, 92), (urziceni, 142)),
                              urziceni -> List((bucharest, 85), (vaslui, 142), (hirsova, 98)),
                              hirsova -> List((urziceni, 98), (eforie, 86)),
                              eforie -> List((hirsova, 86)))
}
*/
