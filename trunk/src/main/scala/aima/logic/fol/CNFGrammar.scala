package aima.logic.fol

abstract class Literal(val sentence: AtomicSentence) {
  def isPositive = this.isInstanceOf[PositiveLiteral]
  def isNegative = !isPositive
}
class PositiveLiteral(s: AtomicSentence) extends Literal(s)
class NegativeLiteral(s: AtomicSentence) extends Literal(s)

class Clause(ls: Literal *) {
  val literals: Set[Literal] = Set(ls: _*)

  def isEmpty = literals.isEmpty

  //is convertible to definite clause
  //there should be only one positive literal
  def isDefiniteClause = literals.filter(_.isPositive).length == 1

  //convert to FOL definite clause
  //TODO: complete it
  def toDefiniteClause = {
    //find the positive literal
    literals.filter(_.isPositive) match {
      case pl :: Nil => //pl is the single positive literal in this clause
        //find set or negative literals, get their positive literal counter part
        val negativeLiterals = literals.filter(_.isNegative)
        if(negativeLiterals != Nil)
          new ImplicationDefiniteClause(
            new IDCPremise(negativeLiterals.map(_.sentence):_*),
            pl.sentence)
        else new SimpleDefiniteClause(pl.sentence)
      case _ => throw new IllegalStateException("Not a definite clause.")
    }
}


//FOL Definite Clause
abstract class FOLDefiniteClause
class SimpleDefiniteClause(val literal: AtomicSentence) extends FOLDefiniteClause
class ImplicationDefiniteClause(val premise: IDCPremise, val conclusion: AtomicSentence) extends FOLDefiniteClause

//IDCPremise is just a conjunction of AtomicSentence's
//Make sure there is atleast one part
class IDCPremise(cs: AtomicSentence *) {
  val parts = Set(cs:_*)
}
