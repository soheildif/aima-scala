package aima.search

/** This file contains code for Queue data structure
 * as described in Chapter 3, AIMA-3e
 *
 * @author Himanshu Gupta
 */

abstract class Queue[A] {
  
  def isEmpty: Boolean //EMPTY?
  def insert(elm: A): Queue[A] //INSERT
  def removeFirst: Option[A] //POP
  
  def insertAll(s: List[A]) = {
    def loop(s: List[A]): Queue[A] = 
      s match {
        case Nil => this
        case x :: rest => insert(x); loop(rest)
      }
    loop(s)
  }
}

class LifoQueue[A] extends Queue[A] {
  private val lifoQ = new scala.collection.mutable.Stack[A]()

  def isEmpty = lifoQ.isEmpty

  def insert(elm:A) = {
    lifoQ.push(elm)
    this
  }

  def removeFirst: Option[A] = 
    if(isEmpty) None else Some(lifoQ.pop)
}

class FifoQueue[A] extends Queue[A] {

  private val fifoQ = new scala.collection.mutable.Queue[A]()

  def isEmpty = fifoQ.isEmpty

  def insert(elm:A) = {
    fifoQ.enqueue(elm)
    this
  }

  def removeFirst : Option[A] =
    if (isEmpty) None else Some(fifoQ.dequeue)
}

class PriorityQueue[A](orderer: (A)=>Ordered[A]) extends Queue[A] {

  private val pq = new scala.collection.mutable.PriorityQueue[A]()(orderer)

  def isEmpty = pq.isEmpty

  def insert(elm: A) = {
    pq += elm
    this
  }

  def removeFirst : Option[A] =
    if (pq.isEmpty) None else Some(pq.dequeue)
}
