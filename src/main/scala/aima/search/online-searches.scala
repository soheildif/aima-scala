package aima.search

trait OnlineDFS[P,A] {

  val problem: OnlineSearchProblem[P,A]

  import scala.collection.mutable.{Map,Stack}

  //map of (from,to) -> action
  private val Result = Map[(P,P),A]()
  private val Unexplored = Map[P,Stack[A]]()
  private val Unbacktracked = Map[P,Stack[P]]()

  private var s: Option[P] = None //previous state
  private var a: Option[A] = None //previous action

  def search(sPrime: P): Option[A] = {
    if (problem.goalTest(sPrime)) a = None
    else {
      if (!Unexplored.contains(sPrime)) Unexplored += (sPrime -> problem.actions(sPrime))

      //s and a are previous percept and action respectively
      (s,a) match {
        case (Some(x),Some(y)) => {
          //If we've already seen it, don't put it again on Unbacktracked
          if(!Result.contains(x,sPrime)) {
            Result += ((x,sPrime) -> y)
            Unbacktracked.getOrElseUpdate(sPrime, new Stack[P]()).push(x)
          }
        }
        case _ => ;
      }

      Unexplored.get(sPrime) match {
        case Some(x) if x.isEmpty => {
          Unbacktracked.get(sPrime) match {
            case None => a = None
            case Some(y) if y.isEmpty => a = None
            case Some(y) => a = Some(action(sPrime,y.pop))
          }
        }
        case Some(x) => a = Some(x.pop)
        case None => throw new IllegalStateException("key should exist")
      }
    }

    s = Some(sPrime)
    a
  }

  // Returns the action that takes agent from "from" to "to"
  // provided such an action exists and has been explored
  private def action(from: P, to: P): A =
    Result.get((from,to)) match {
      case Some(a) => a
      case None => throw new IllegalStateException(from + "to" + to + "has not been explored yet or no such direct path exists.")
    }
    
  private def reset {
    s = None
    a = None
    Result.clear
    Unexplored.clear
    Unbacktracked.clear
  }
}   
  
abstract class OnlineSearchProblem[P,A] {
  
  import scala.collection.mutable.Stack

  def actions(s: P): Stack[A]
  def goalTest(s: P): Boolean
}


import aima.basic.{Environment,Agent}

//Agent and Environment
abstract class MapAgent[S] extends Agent[In[S],Go[S]] {
  var currentState:In[S]
}

class MapEnvironment[T <: MapAgent[S],S] extends Environment[T,In[S],Go[S]] {

  override def executeAction(agent: T, action: Option[Go[S]]) {
    action match {
      case Some(Go(s)) => agent currentState_= In(s)
      case None => agent.die()
    }
  }

  override def getPerceptSeenBy(agent: T) = agent.currentState
}


//OnlineDFS + Map Problem
class OnlineDFSMapAgent[S](prob: OnlineSearchMapProblem[S],initState: In[S]) extends MapAgent[S] with OnlineDFS[In[S],Go[S]] {
  var currentState = initState
  val problem = prob
  override def agentProgram(percept: In[S]) = search(percept)
}

class OnlineSearchMapProblem[S](locationMap: LocationMap[S], goalState: In[S]) 
extends OnlineSearchProblem[In[S],Go[S]] {

  import scala.collection.mutable.Stack

  def actions(s: In[S]) = {
    val In(p) = s
    val result = new Stack[Go[S]]()
    locationMap.getLocationsReachableFrom(p).foreach((a:S) => result.push(Go(a)))
    result
  }

  def goalTest(s: In[S]) = s == goalState
}
