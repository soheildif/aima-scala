package aima.logic.propositional

import org.scalatest.Suite
import scala.collection.immutable.{Set,ListSet}

/* This file contains tests for entailment algorithms
 * described in chapter 7.
 *
 * @author Himanshu Gupta
 */


class TTEntailsTest extends Suite {
  
  private val PLP = PropositionalLogicParser

  /* This test is based on the simple knowledge base described in
   * section 7.4.3
   * KB = R1 /\ R2 /\ R3 /\ R4 /\ R5
   * Test whether KB |= alpha, where alpha is ~P12?
   * It should return true.
   */
  def testSec7_4_3() {
    //create the KB
    val R1 = "~P11"
    val R2 = "(B11 <=> (P12 \\/ P21))"
    val R3 = "(B21 <=> (P11 \\/ P22 \\/ P31))"
    val R4 = "~B11"
    val R5 = "B21"

    val KB = PLP.parse(R1 + "/\\" + R2 + "/\\" + R3 + "/\\" + R4 + "/\\" + R5)
    val alpha = PLP.parse("~P12")

    assert(TTEntails(KB,alpha))
  }
}
    
