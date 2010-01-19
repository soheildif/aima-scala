package aima.search

import org.scalatest.Suite

class FifoQueueTest {
  def testIt() {
    val f = new FifoQueue[Int]
    assert(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    f.insertAll(List(4,5))
    assert(f.removeFirst == Some(1))
    assert(f.removeFirst == Some(2))
    assert(f.removeFirst == Some(3))
    assert(f.removeFirst == Some(4))
    assert(f.removeFirst == Some(5))
    assert(f.isEmpty)
  }
}

class LifoQueueTest {
  def testIt() {
    val f = new LifoQueue[Int]
    assert(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    f.insertAll(List(4,5))
    assert(f.removeFirst == Some(5))
    assert(f.removeFirst == Some(4))
    assert(f.removeFirst == Some(3))
    assert(f.removeFirst == Some(2))
    assert(f.removeFirst == Some(1))
    assert(f.isEmpty)
  }
}

class PriorityQueueTest {
  def testIt() {
    val f = new PriorityQueue[Int](
      (a) => new Ordered[Int] {
                def compare(that: Int) = that - a })
                    
    assert(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    f.insertAll(List(4,5))
    assert(f.removeFirst == Some(1))
    assert(f.removeFirst == Some(2))
    assert(f.removeFirst == Some(3))
    assert(f.removeFirst == Some(4))
    assert(f.removeFirst == Some(5))
    assert(f.isEmpty)
  }
}
