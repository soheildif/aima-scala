package aima.logic.propositional

import org.scalatest.Suite
import scala.collection.immutable.{Set}

/** Tests for Propositional Logic Grammar Parser
 *
 * @ Himanshu Gupta
 */
class PropositionalLogicParserTest extends Suite {

  private val PLP = PropositionalLogicParser


  //some proposition symbols
  private val A = PropositionSymbol("A")
  private val B = PropositionSymbol("B")
  private val C = PropositionSymbol("C")
  private val D = PropositionSymbol("D")
  private val S = PropositionSymbol("S")
  private val T = PropositionSymbol("T")

  private val norvig = PropositionSymbol("norvig")
  private val aima = PropositionSymbol("aima")
  private val lisp = PropositionSymbol("lisp")
  private val cool = PropositionSymbol("cool")

  def testParser() {

    expect(A)(PLP.parse("A"))
    expect(PropositionSymbol("True"))(PLP.parse("True"))
    expect(PropositionSymbol("False"))(PLP.parse("False"))
    expect(new Negation(PropositionSymbol("A12")))(PLP.parse("~ A12"))
    expect(new Disjunction(A,B))(PLP.parse("A|B"))
    expect(new Conjunction(A,B,C))(PLP.parse("A & B & C"))
    expect(new Disjunction(A,B,C))(PLP.parse("A | B | C"))
    expect(new Disjunction(A,new Conjunction(B,C)))(PLP.parse("A | B & C"))
    expect(new Disjunction(A,new Conjunction(new Negation(B),C)))(PLP.parse("A | ~ B & C"))
    expect(new Conditional(new Conjunction(A,B),C))(PLP.parse("A & B => C"))
    expect(new BiConditional(new Negation(A),new Conjunction(B,C)))(PLP.parse("~A <=> (B & C)"))
    
    expect(
      new Conditional(
        new Conjunction(new Negation(A),B),
        new Conjunction(S,T))
      )(PLP.parse("~ A & B => S & T"))
    
    expect(
      new Conditional(
        new Disjunction(new Negation(A),B),
        new Conjunction(S,T))
      )(PLP.parse("~ A | B => S & T"))
    
    expect(
      new Disjunction(new Negation(A),
                      new Conjunction(new Conditional(B,C),D))
    )(PLP.parse("~ A | (B => C) & D"))
    
    expect(
      new Conjunction(
        new Disjunction(norvig,aima,lisp),
        new Conditional(lisp,cool))
    )(PLP.parse("( norvig | aima | lisp ) & (lisp => cool)"))
  }
}
