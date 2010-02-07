package aima.logic.propositional

import org.scalatest.Suite
import scala.collection.immutable.{Set}

/* This file contains tests for entailment algorithms
 * described in chapter 7.
 *
 * @author Himanshu Gupta
 */


/** Simple KB described in Fig 7.4.3 */
object KB {
  private val PLP = PropositionalLogicParser

  def apply: Conjunction = {
    val R1 = PLP.parse("~P11")
    val R2 = PLP.parse("B11 <=> (P12 \\/ P21)")
    val R3 = PLP.parse("B21 <=> (P11 \\/ P22 \\/ P31)")
    val R4 = PLP.parse("~B11")
    val R5 =PLP.parse( "B21")

    Sentence.createKB(R1,R2,R3,R4,R5)
  }
}
    
class TTEntailsTest extends Suite {

  def test1() {
    val KB = new TTEntailsBasedKB("A/\\B")
    assert(KB.ask("A"))
  }

  def test2() {
    val KB = new TTEntailsBasedKB("A\\/B")
    assert(!KB.ask("A"))
  }

  def test3() {
    val KB = new TTEntailsBasedKB("(A => B) /\\ B")
    assert(!KB.ask("A"))
  }

  def test4() {
    val KB = new TTEntailsBasedKB("A")
    assert(!KB.ask("~ A"))
  }

  def test5() {
    val KB = new TTEntailsBasedKB("(A => B) /\\ B")
    assert(!KB.ask("X"))
  }

  def test6() {
    val KB = new TTEntailsBasedKB("~A")
    assert(!KB.ask("A"))
  }

  def test7() {
    val KB = new TTEntailsBasedKB()
    KB.tell("(B12 <=> (P11 \\/ (P13 \\/ (P22 \\/ P02))))")
    KB.tell("(B21 <=> (P20 \\/ (P22 \\/ (P31 \\/ P11))))")
    KB.tell("(B01 <=> (P00 \\/ (P02 \\/ P11)))")
    KB.tell("(B10 <=> (P11 \\/ (P20 \\/ P00)))")
    KB.tell("(~ B21)")
    KB.tell("(~B12)")
    KB.tell("(B10)")
    KB.tell("(B01)")
    assert(KB.ask("P00"))
    assert(!KB.ask("~P00"))
  }

  /**
   * Test based on simple KB described in Section - 7.4.3
   */
  def testSec7_4_3() {
    val R1 = "~P11"
    val R2 = "B11 <=> (P12 \\/ P21)"
    val R3 = "B21 <=> (P11 \\/ P22 \\/ P31)"
    val R4 = "~B11"
    val R5 = "B21"

    val alpha = "~P12"

    assert(new TTEntailsBasedKB(R1,R2,R3,R4,R5).ask(alpha))
  } 
}

class PLResolutionTest extends Suite {

  def test1() {
    val KB = new PLResolutionBasedKB("A/\\B")
    assert(KB.ask("A"))
  }

  def test2() {
    val KB = new PLResolutionBasedKB("A\\/B")
    assert(!KB.ask("A"))
  }

  def test3() {
    val KB = new PLResolutionBasedKB("(A => B) /\\ B")
    assert(!KB.ask("A"))
  }

  def test4() {
    val KB = new PLResolutionBasedKB("A")
    assert(!KB.ask("~ A"))
  }

  def test5() {
    val KB = new PLResolutionBasedKB("(A => B) /\\ B")
    assert(!KB.ask("X"))
  }

  def test6() {
    val KB = new PLResolutionBasedKB("~A")
    assert(!KB.ask("A"))
  }

/*  def test7() { //TODO: Fix it
    val KB = new PLResolutionBasedKB()
    KB.tell("(B12 <=> (P11 \\/ (P13 \\/ (P22 \\/ P02))))")
    KB.tell("(B21 <=> (P20 \\/ (P22 \\/ (P31 \\/ P11))))")
    KB.tell("(B01 <=> (P00 \\/ (P02 \\/ P11)))")
    KB.tell("(B10 <=> (P11 \\/ (P20 \\/ P00)))")
    KB.tell("(~ B21)")
    KB.tell("(~B12)")
    KB.tell("(B10)")
    KB.tell("(B01)")
    assert(KB.ask("P00"))
    assert(!KB.ask("~P00"))
  }*/

  /**
   * Test based on simple KB described in Section - 7.4.3
   */
  def testSec7_4_3() {
    val R1 = "~P11"
    val R2 = "B11 <=> (P12 \\/ P21)"
    val R3 = "B21 <=> (P11 \\/ P22 \\/ P31)"
    val R4 = "~B11"
    val R5 = "B21"

    val alpha = "~P12"

    assert(new PLResolutionBasedKB(R1,R2,R3,R4,R5).ask(alpha))
  } 
}

class PLFCEntailsTest extends Suite {

  /** This test is based on the problem shown
   * in Fig 7.16
   */
  def testFig7_16() {
    val P = PropositionSymbol("P")
    val Q = PropositionSymbol("Q")
    val L = PropositionSymbol("L")
    val M = PropositionSymbol("M")
    val A = PropositionSymbol("A")
    val B = PropositionSymbol("B")

    val KB = Set(new DefiniteClause(Set(P),Q), // P => Q
                     new DefiniteClause(Set(L,M),P), // L/\M => P
                     new DefiniteClause(Set(B,L),M), // B/\L => M
                     new DefiniteClause(Set(A,P),L), // A/\P => L
                     new DefiniteClause(Set(A,B),L)) // A/\B => L

    assert(PLFCEntails(KB,Q,List(A,B)))
  }
}

class DPLLSatisfiableTest extends Suite {
  
  private val PLP = PropositionalLogicParser

  /**
   * Test whether KB /\ ~alpha is unsatisfiable, where alpha is ~P12?
   * It should return true.
   */
  def testSec7_4_3() {
    val alpha = PLP.parse("~~P12")
    assert(!DPLLSatisfiable(Sentence.addToKB(KB.apply,alpha)))
  }
}
