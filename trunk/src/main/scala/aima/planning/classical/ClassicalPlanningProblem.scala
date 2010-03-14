package aima.planning.classical

/** Propositional Planning Problems representation(ones with no variables),
 * that Planning Graphs work with, described in section 10.3, 3rd paragraph
 *
 * @author Himanshu Gupta
 */
class ClassicalPlanningProblem(val initState: Set[Literal], val goals: Set[Literal],
                               val actions: Set[Action])
object ClassicalPlanningProblem {
  def apply(initState: String, goals: String, actions: Action *) =
    new ClassicalPlanningProblem(
      CPSentenceParser.parseSet(initState),
      CPSentenceParser.parseSet(goals),
      Set(actions:_*))
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
object Action {
  def apply(symbol: String, preconds: String, effects: String) =
    new Action(
      CPSentenceParser.parseAtom(symbol),
      CPSentenceParser.parseSet(preconds),
      CPSentenceParser.parseSet(effects))
}

//---------- AST for Classical Planning Variable Free Sentence ---------
abstract class Sentence

//Used to represent a predicate( e.g. Have(Cake))
//as well as an action name(e.g. Eat(Cake))
class Atom(val symbol: String, val args: List[String]) extends Sentence {

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

class Negation(val sentence: Atom) extends Sentence
class Conjunction(val conjuncts: Set[Sentence]) extends Sentence

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


//---------------- Parser ---------------------
object CPSentenceParser extends JavaTokenParsers {


  def sentence: Parser[Sentence] = negation~rep("&"~negation) ^^
                                      {case d~Nil => d
                                       case d~reps => new Conjunction(d :: reps.map(_ match { case _~disj => disj }))}
  

  def negation: Parser[Sentence] = opt("~")~atom ^^
                                    {case None~as => as
                                     case Some(_)~as => new Negation(as)}

  def atom: Parser[Atom] = (
    constant~"("~repsep(constant,",")~")" ^^
    {case s~"("~ts~")" => new Atom(s,ts)}
  )
                                         
  def constant = """[A-Z][a-z0-9A-Z]*""".r

  def parseSet(in: String): Set[Literal] =
    parseAll(sentence,in).get match {
      case x: Atom => Set(PositiveLiteral(x))
      case x: Negation => Set(NegatveLiteral(x.sentence))
      case x: Conjunction =>
        x.conjuncts.map(
          _ match {
            case y: Atom => Set(PositiveLiteral(y))
            case y: Negation => Set(NegativeLiteral(y.sentence))
          })
    }

  def parseAtom(in: String): Atom =
    parseAll(atom,in)
}
