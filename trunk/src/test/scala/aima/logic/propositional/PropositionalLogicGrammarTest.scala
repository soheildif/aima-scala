package aima.logic.propositional

import org.scalatest.Suite
import scala.collection.immutable.{Set,ListSet}

/** Tests for Propositional Logic Grammar Parser
 *
 * @ Himanshu Gupta
 */
class PropositionalLogicParserTest extends Suite {

  private val PLP = PropositionalLogicParser

  def testParser() {

    assert(PLP.parse("A").toString == "A")
    assert(PLP.parse("AIMA").toString == "AIMA")
    assert(PLP.parse("True").toString == "True")
    assert(PLP.parse("False").toString == "False")
    assert(PLP.parse("~ A12").toString == "~A12")
    assert(PLP.parse("A /\\ B /\\ C").toString == "(A /\\ B /\\ C)")
    assert(PLP.parse("A \\/ B \\/ C").toString == "(A \\/ B \\/ C)")
    assert(PLP.parse("A \\/ B /\\ C").toString == "(A \\/ (B /\\ C))")
    assert(PLP.parse("A \\/ ~ B /\\ C").toString == "(A \\/ (~B /\\ C))")
    assert(PLP.parse("A /\\ B => C").toString == "((A /\\ B) => C)")
    assert(PLP.parse("~A <=> (B /\\ C)").toString == "(~A <=> (B /\\ C))")
    assert(PLP.parse("~A <=>   B /\\ C").toString == "(~A <=> (B /\\ C))")
    assert(PLP.parse("~ A /\\ B => S /\\ T").toString == "((~A /\\ B) => (S /\\ T))")
    assert(PLP.parse("~ A \\/ B => S /\\ T").toString == "((~A \\/ B) => (S /\\ T))")
    assert(PLP.parse("~ A \\/ (B => C) /\\ D").toString == "(~A \\/ ((B => C) /\\ D))")
    assert(PLP.parse("( norvig \\/ aima \\/ lisp ) /\\ (lisp => cool)").toString == 
                    "((norvig \\/ aima \\/ lisp) /\\ (lisp => cool))")

  }
}
