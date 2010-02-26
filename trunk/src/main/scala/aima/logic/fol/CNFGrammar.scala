package aima.logic.fol

/**
 * CNF Form AST for FOL sentence
 *
 * @author Himanshu Gupta
 */
abstract class Literal(val sentence: AtomicSentence) {
  def isPositive = this.isInstanceOf[PositiveLiteral]
  def isNegative = this.isInstanceOf[NegativeLiteral]
}
case class PositiveLiteral(s: AtomicSentence) extends Literal(s) {
  override def toString = s.toString
}
case class NegativeLiteral(s: AtomicSentence) extends Literal(s) {
  override def toString = "~" + s.toString
}

class Clause(ls: Literal *) {
  val literals: Set[Literal] = Set(ls: _*)

  def isEmpty = literals.isEmpty

  //is convertible to definite clause
  //there should be only one positive literal
  def isDefiniteClause = literals.filter(_.isPositive).size == 1

  //convert to FOL definite clause
  def toDefiniteClause = {
    //find the positive literal
    literals.filter(_.isPositive).toList match {
      case pl :: Nil => //pl is the single positive literal in this clause
        //find set of negative literals, get their positive literal counter part
        val negativeLiterals = literals.filter(_.isNegative)
        if(negativeLiterals != Nil)
          new ImplicationDefiniteClause(
            negativeLiterals.map(_.sentence),pl.sentence)
        else new SimpleDefiniteClause(pl.sentence)
      case _ => throw new IllegalStateException("Not a definite clause.")
    }
  }

  override def toString = "(" + literals.map(_.toString).reduceLeft(_ + " \\/ " + _)  + ")"
}


//FOL Definite Clause
abstract class FOLDefiniteClause
class SimpleDefiniteClause(val literal: AtomicSentence) extends FOLDefiniteClause {
  override def toString = literal.toString
}

class ImplicationDefiniteClause(val premise: Set[AtomicSentence], 
                                val conclusion: AtomicSentence) extends FOLDefiniteClause {
  override def toString =
    "(" + premise.map(_.toString).reduceLeft(_ + " /\\ " + _)  + ") => " + conclusion.toString
} 
