package aima.logic.fol

abstract class Literal(val sentence: AtomicSentence)
class PositiveLiteral(s: AtomicSentence) extends Literal(s)
class NegativeLiteral(s: AtomicSentence) extends Literal(s)

class Clause(ls: Literal *) {
  val literals: Set[Literal] = Set(ls: _*)

  def isEmpty = literals.isEmpty

  //is convertible to definite clause
  //there should be only one positive literal
  def isDefiniteClause: Boolean = {
    literals.filter(_ match {
                      case x: PositiveLiteral => true
                      case x: NegativeLiteral => false
                    }).length == 1
  }

  //convert to FOL definite clause
  //TODO: complete it
  def toDefiniteClause = {
    //find the positive literal
    literals.filter(_ match {
      case x: PositiveLiteral => true
      case x: NegativeLiteral => false
    }) match {
      case pl :: Nil => //pl is the single positive literal in this clause
        //find set or negative literals, get their positive literal counter part
        literal
      case _ => throw new IllegalStateException("Not a definite clause.")
    }

    
}


//FOL Definite Clause
abstract class FOLDefiniteClause
class SimpleDefiniteClause(val literal: AtomicSentence) extends FOLDefiniteClause
class ImplicationDefiniteClause(val premise: IDCPremise, val conclusion: AtomicSentence) extends FOLDefiniteClause

//IDCPremise is just a conjunction of AtomicSentence's
class IDCPremise(cs: AtomicSentence *) {
  val parts = Set(cs:_*)
}
