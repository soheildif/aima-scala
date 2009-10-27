package aima.search

import org.scalatest.Suite

class FifoFringeTest {
  def testIt() {
    val f = new FifoFringe[Int]
    assert(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    assert(f.removeFirst == Some(1))
    assert(f.removeFirst == Some(2))
    assert(f.removeFirst == Some(3))
    assert(f.isEmpty)
  }
}

class LifoFringeTest {
  def testIt() {
    val f = new LifoFringe[Int]
    assert(f.isEmpty)
    f.insert(1)
    f.insertAll(List(2,3))
    assert(f.removeFirst == Some(3))
    assert(f.removeFirst == Some(2))
    assert(f.removeFirst == Some(1))
    assert(f.isEmpty)
  }
}

