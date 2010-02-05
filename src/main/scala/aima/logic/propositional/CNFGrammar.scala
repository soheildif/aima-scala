package aima.logic.propositional

import scala.collection.immutable.{Set,ListSet}

/** AST for the CNF grammar described in Fig 7.14
 *
 * @author Himanshu Gupta
 */
/*
class CNFSentence(cs: Clause *) {

  val clauses: Set[Clause] = ListSet(cs: _*)

  override def equals(that: Any) = {
    that match {
      case x: Clause if this.clauses == x.clauses
      case _ => false
    }
}

class Clause(ls: Literal *) {
  val literals: Set[Literal] = ListSet(ls: _*)

  def isEmpty = literals.isEmpty

  def isTrue(model: Map[PropositionSymbol,Boolean]) = {
    //even if a single one is true, it is true
    if(literals.exists(_.isTrue(model) == Some(true)))
      Some(true)
    else {
      if(literals.exists(_.isTrue(model) == None))
        None
      else Some(false)
    }
  }

  def symbols: Set[Symbol] = literals.flatMap(_.symbols) 

}

abstract class Literal
case class PositiveLiteral(val symbol: PropositionSymbol) extends Literal {
  def isTrue(model: Map[PropositionSymbol,Boolean]) =
    model.get(symbol) match {
      case Some(x) => Some(x)
      case None => None
    }

  def symbols: Set[Symbol] = ListSet(symbol)
}
case class NegativeLiteral(val symbol: PropositionSymbol) extends Literal {
  def isTrue(model: Map[PropositionSymbol,Boolean]) =
    model.get(symbol) match {
      case Some(x) => Some(!x)
      case None => None
    }

  def symbols: Set[PropositionSymbol] = ListSet(symbol)
}
*/
/*
class DefiniteClause  {
  val premise: Set[Symbol]
  val conclusion: Symbol
}
*/
