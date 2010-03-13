package aima.planning.classical

class ClassicalPlanningProblem(initState: Set[Literal], goals: Set[Literal], actions: Action *)


object ClassicalPlanningProblem {

  def apply(initState: String, goals: String, actions: Action *)
}


class Action(val symbol: Atom, val preconditions: Set[Literal], val effects: Set[Literal])


//Used to represent a predicate( e.g. Have(Cake))
//as well as an action name(e.g. Eat(Cake))
class Atom(val symbol: String, val args: Set[String]) {

  override def equals(that: Any) =
    that match {
      case x: Atom =>
        (symbol == x.symbol) && (args == x.args)
      case _ => false
    }

  def toString = {
    symbol +
    if(args.size == 0) "()" else "(" + args.reduceLeft(_ + "," + _) + ")"
  }
}

//Positive and Negative Literals
class Literal(val sentence: Atom)
class PositiveLiteral(s: Atom) extends Literal(s)
class NegativeLiteral(s: Atom) extends Literal(s)
