package aima.logic.propositional

import org.scalatest.Suite
import scala.collection.immutable.{Set,ListSet}

/* This file contains tests for CNF sentence Grammar
 * described in Fig 7.14
 *
 * @author Himanshu Gupta
 */

class CNFGrammarTest extends Suite {
  
  private val PLP = PropositionalLogicParser

  def testIt() {

    assert(SentenceToCNF(PLP.parse("A")).toString == "((A))")
    assert(SentenceToCNF(PLP.parse("AIMA")).toString == "((AIMA))")
    assert(SentenceToCNF(PLP.parse("True")).toString == "((True))")
    assert(SentenceToCNF(PLP.parse("False")).toString == "((False))")
    assert(SentenceToCNF(PLP.parse("~ A12")).toString == "((~A12))")
    assert(SentenceToCNF(PLP.parse("A /\\ B /\\ C")).toString == "((C) /\\ (B) /\\ (A))")
    assert(SentenceToCNF(PLP.parse("A \\/ B \\/ C")).toString == "((A \\/ B \\/ C))")
    assert(SentenceToCNF(PLP.parse("A \\/ B /\\ C")).toString == "((A \\/ C) /\\ (A \\/ B))")
    assert(SentenceToCNF(PLP.parse("A \\/ ~ B /\\ C")).toString == "((A \\/ C) /\\ (A \\/ ~B))")
    assert(SentenceToCNF(PLP.parse("A /\\ B => C")).toString == "((~A \\/ ~B \\/ C))")
    assert(SentenceToCNF(PLP.parse("~A <=> (B /\\ C)")).toString == 
      "((~A \\/ ~C \\/ ~B) /\\ (A \\/ B) /\\ (A \\/ C))")
    assert(SentenceToCNF(PLP.parse("~A <=>   B /\\ C")).toString == 
      "((~A \\/ ~C \\/ ~B) /\\ (A \\/ B) /\\ (A \\/ C))")
    assert(SentenceToCNF(PLP.parse("~ A /\\ B => S /\\ T")).toString == 
      "((A \\/ ~B \\/ T) /\\ (A \\/ ~B \\/ S))")
    assert(SentenceToCNF(PLP.parse("~ A \\/ B => S /\\ T")).toString == 
      "((A \\/ T) /\\ (A \\/ S) /\\ (~B \\/ T) /\\ (~B \\/ S))")
    assert(SentenceToCNF(PLP.parse("~ A \\/ (B => C) /\\ D")).toString == 
      "((~A \\/ D) /\\ (~A \\/ C \\/ ~B))")
    assert(SentenceToCNF(PLP.parse("( norvig \\/ aima \\/ lisp ) /\\ (lisp => cool)")).toString == 
      "((~lisp \\/ cool) /\\ (norvig \\/ aima \\/ lisp))")                    
  }

}
