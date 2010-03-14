package aima.planning.classical

/** Propositional Planning Problems representation(ones with no variables),
 * that Planning Graphs work with, described in section 10.3, 3rd paragraph
 *
 * @author Himanshu Gupta
 */
class ClassicalPlanningProblem(initState: Set[Literal], goals: Set[Literal], actions: Action *)


object ClassicalPlanningProblem {

  def apply(initState: String, goals: String, actions: Action *)
}


class Action(val symbol: Atom, val preconditions: Set[Literal], val effects: Set[Literal]) {

  override def equals(that: Any) =
    that match {
      case x: Action => this.symbol == x.symbol
      case _ => false
    }

  override def hashcode = symbol.hashcode
  
  override def toString = symbol.toString
}


//Used to represent a predicate( e.g. Have(Cake))
//as well as an action name(e.g. Eat(Cake))
class Atom(val symbol: String, val args: List[String]) {

  override def equals(that: Any) =
    that match {
      case x: Atom =>
        (symbol == x.symbol) && (args == x.args)
      case _ => false
    }

  override def hashcode = toString.hashcode

  override def toString = {
    symbol +
    if(args.size == 0) "()" else "(" + args.reduceLeft(_ + "," + _) + ")"
  }
}

//Positive and Negative Literals
sealed class Literal(val sentence: Atom) {

  def isPositive(l: Literal) =
    l match {
      case _: PositiveLiteral => true
      case _: NegativeLiteral => false
    }

  def isNegative(l: Literal) = !isPositive(l)

  override def toString =
    (if(isNegative(this)) "~" else "") + sentence.toString
}
case class PositiveLiteral(s: Atom) extends Literal(s)
case class NegativeLiteral(s: Atom) extends Literal(s)
