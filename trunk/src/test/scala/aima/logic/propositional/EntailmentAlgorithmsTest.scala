package aima.logic.propositional

import org.scalatest.Suite
import scala.collection.immutable.{Set,ListSet}

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
  
  private val PLP = PropositionalLogicParser

  /**
   * Test whether KB |= alpha, where alpha is ~P12?
   * It should return true.
   */
  def testSec7_4_3() {
    val alpha = PLP.parse("~P12")
    assert(TTEntails(KB.apply,alpha))
  }
}

class PLResolutionTest extends Suite {
  
  private val PLP = PropositionalLogicParser

  /**
   * Test whether KB |= alpha, where alpha is ~P12?
   * It should return true.
   */
  def testSec7_4_3() {
    val alpha = PLP.parse("~P12")
    assert(PLResolution(KB.apply,alpha))
  }
}
