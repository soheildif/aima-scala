package aima.logic.propositional

import org.scalatest.Suite
import scala.collection.immutable.{Set}

/* This file contains tests for CNF sentence Grammar
 * described in Fig 7.14
 *
 * @author Himanshu Gupta
 */

class CNFGrammarTest extends Suite {
  
  private val PLP = PropositionalLogicParser

  def testIt() {

    expect("((A))")(SentenceToCNF(PLP.parse("A")).toString)
    expect("((AIMA))")(SentenceToCNF(PLP.parse("AIMA")).toString)
    expect("((True))")(SentenceToCNF(PLP.parse("True")).toString)
    expect("((False))")(SentenceToCNF(PLP.parse("False")).toString)
    expect("((~A12))")(SentenceToCNF(PLP.parse("~ A12")).toString)
    expect("((A \\/ B))")(SentenceToCNF(PLP.parse("A\\/B")).toString)
    expect("((A) /\\ (B) /\\ (C))")(SentenceToCNF(PLP.parse("A /\\ B /\\ C")).toString)
    expect("((A \\/ B \\/ C))")(SentenceToCNF(PLP.parse("A \\/ B \\/ C")).toString)
    expect("((A \\/ B) /\\ (A \\/ C))")(SentenceToCNF(PLP.parse("A \\/ B /\\ C")).toString)
    expect("((A \\/ ~B) /\\ (A \\/ C))")(SentenceToCNF(PLP.parse("A \\/ ~ B /\\ C")).toString)
    expect("((~A \\/ ~B \\/ C))")(SentenceToCNF(PLP.parse("A /\\ B => C")).toString)
    expect("((A \\/ B) /\\ (A \\/ C) /\\ (~A \\/ ~B \\/ ~C))")(SentenceToCNF(PLP.parse("~A <=> (B /\\ C)")).toString)
    expect("((A \\/ B) /\\ (A \\/ C) /\\ (~A \\/ ~B \\/ ~C))")(SentenceToCNF(PLP.parse("~A <=>   B /\\ C")).toString)
    expect("((A \\/ ~B \\/ S) /\\ (A \\/ ~B \\/ T))")(SentenceToCNF(PLP.parse("~ A /\\ B => S /\\ T")).toString)
    expect("((A \\/ S) /\\ (A \\/ T) /\\ (~B \\/ S) /\\ (~B \\/ T))")(SentenceToCNF(PLP.parse("~ A \\/ B => S /\\ T")).toString)
    expect("((~A \\/ ~B \\/ C) /\\ (~A \\/ D))")(SentenceToCNF(PLP.parse("~ A \\/ (B => C) /\\ D")).toString)
    expect("((norvig \\/ aima \\/ lisp) /\\ (~lisp \\/ cool))")(SentenceToCNF(PLP.parse("( norvig \\/ aima \\/ lisp ) /\\ (lisp => cool)")).toString)
    expect("""((B12 \/ ~P02) /\ (~B12 \/ P22 \/ P11 \/ P02 \/ P13) /\ (B12 \/ ~P22) /\ (B12 \/ ~P13) /\ (B12 \/ ~P11))""".mkString)(SentenceToCNF(PLP.parse("(B12 <=> (P11 \\/ (P13 \\/ (P22 \\/ P02))))")).toString)
  }
}
