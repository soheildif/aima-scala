package aima.logic.fol

import scala.util.parsing.combinator._

/** Grammar for FOL logic */

//AtomicSentence := Predicate(Term, Term, ...) | Term = Term

//4L stands for FOR-All
//3E stands for THERE EXISTS

//Term = Function(Term,Term,...)| Variable | Constant

object FOLParser extends JavaTokenParsers {

  def sentence: Parser[Sentence] = (
    "4L"~repsep(variable,",")~biconditional ^^
    { case _~vs~s =>
      vs.map(Variable(_)).foldRight(s)(new UniversalQuantifier(_,_)) } //Universal Quantifier
    | "3E"~repsep(variable,",")~biconditional ^^
    { case _~vs~s =>
      vs.map(Variable(_)).foldRight(s)(new ExistentialQuantifier(_,_)) } //Existential Quantifier
    | biconditional
  )
  def biconditional: Parser[Sentence] = conditional~opt("<=>"~conditional) ^^
                                      {case condition~None => condition
                                       case condition~Some(_~conclusion) => new BiConditional(condition,conclusion) }
  def conditional: Parser[Sentence] = disjunction~opt("=>"~disjunction) ^^
                                      {case premise~None => premise
                                       case premise~Some(_~conclusion) => new Conditional(premise,conclusion) }
  def disjunction: Parser[Sentence] = conjunction~rep("\\/"~conjunction) ^^
                                      {case c~Nil => c
                                       case c~reps => new Disjunction(c :: reps.map(_ match { case _~cond => cond }):_*)}
  def conjunction: Parser[Sentence] = negation~rep("/\\"~negation) ^^
                                      {case d~Nil => d
                                       case d~reps => new Conjunction(d :: reps.map(_ match { case _~disj => disj }):_*)}
  

  def negation: Parser[Sentence] = rep("~")~atomicSentence ^^
                                    {case Nil~as => as
                                     case list~as => list.foldLeft(as)((x,y)=> new Negation(x))}

  def atomicSentence: Parser[Sentence] = (
    term~"="~term ^^
    {case lt~"="~rt => new Equal(lt,rt)} //Equal
    | symbol~"("~repsep(term,",")~")" ^^
    {case s~"("~Nil~")" => new Predicate(s)
     case s~"("~ts~")" => new Predicate(s,ts:_*)} //Predicate
    | "(" ~> sentence <~ ")"
  )
                                         

  def term: Parser[Term] = (
    symbol~"("~repsep(term,",")~")" ^^
    {case s~"("~Nil~")" => new Function(s)
     case s~"("~ts~")" => new Function(s,ts:_*)}  //Function
    | symbol ^^ (Constant(_)) //Constant
    | variable ^^ (Variable(_)) //Variable
  )

  def variable = """[a-z][a-z0-9A-Z]*""".r //symbol for Variable
  def symbol = """[A-Z][a-z0-9A-Z]*""".r //symbol for Constant,Predicate,Function

  def parse(in: String) = parseAll(sentence,in).get

  //term parser, meant for Testing ONLY
  def parseTerm(in: String) = parseAll(term,in).get
}



//TODO:Sentence equality should work
abstract class Sentence

//Term
abstract class Term
case class Constant(val symbol: String) extends Term {
  override def toString = symbol
}

class Function(val symbol:String, as: Term *) extends Term {
  val args = List(as:_*)

  override def equals(that: Any) =
    that match {
      case x: Function =>
        (x.symbol == this.symbol) && (x.args == this.args)
      case _ => false
    }

  override def toString =
    if(args == Nil)
      symbol + "()"
    else
      symbol + "(" + args.map(_.toString).reduceLeft(_ + "," + _) + ")"
}

case class Variable(val symbol: String) extends Term {
  override def toString = symbol
}

//======== Atomic Sentence ============
abstract class AtomicSentence extends Sentence

class Predicate(val symbol: String, as: Term *) extends AtomicSentence {
  val args = List(as:_*)

  override def equals(that: Any) =
    that match {
      case x: Predicate =>
        (x.symbol == this.symbol) && (x.args == this.args)
      case _ => false
    }

  override def toString =
    if(args == Nil)
      symbol + "()"
    else
      symbol + "(" + args.map(_.toString).reduceLeft(_ + "," + _) + ")"
}

class Equal(val lTerm: Term, val rTerm: Term) extends AtomicSentence {
  override def equals(that: Any) =
    that match {
      case x: Equal => (x.lTerm == this.lTerm) && (x.rTerm == this.rTerm)
      case _ => false
    }

  override def toString = "(" + lTerm + " = " + rTerm + ")"
}

//========= Complex Sentence =========
class Negation(val sentence: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: Negation => x.sentence == this.sentence
      case _ => false
    }

  override def toString = "~" + sentence
}

class Conjunction(cs: Sentence *) extends Sentence {
  val conjuncts: Set[Sentence] = Set(cs: _*)

  override def equals(that: Any) =
    that match {
      case x: Conjunction => x.conjuncts == this.conjuncts
      case _ => false
    }

  override def toString = "(" + conjuncts.map(_.toString).reduceLeft(_ + " /\\ " + _)  + ")"
}

class Disjunction(ds: Sentence *) extends Sentence {
  val disjuncts: Set[Sentence] = Set(ds: _*)

  override def equals(that: Any) =
    that match {
      case x: Disjunction => x.disjuncts == this.disjuncts
    }

  override def toString = "(" + disjuncts.map(_.toString).reduceLeft(_ + " \\/ " + _)  + ")"
}

class Conditional(val premise: Sentence, val conclusion: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: Conditional => (x.premise == this.premise) && (x.conclusion == this.conclusion)
      case _ => false
    }

  override def toString = "(" + premise + " => " + conclusion + ")"
}

class BiConditional(val condition: Sentence, val conclusion: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: BiConditional => (x.condition == this.condition) && (x.conclusion == this.conclusion)
      case _ => false
    }

  override def toString = "(" + condition + " <=> " + conclusion + ")"
}

class UniversalQuantifier(val variable: Variable, val sentence: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: UniversalQuantifier =>
        (x.variable == this.variable) && (x.sentence == this.sentence)
      case _ => false
    }

  override def toString = "(4L " + variable + " " + sentence + ")"
}

class ExistentialQuantifier(val variable: Variable, val sentence: Sentence) extends Sentence {
  override def equals(that: Any) =
    that match {
      case x: ExistentialQuantifier =>
        (x.variable == this.variable) && (x.sentence == this.sentence)
      case _ => false
    }

  override def toString = "(3E " + variable + " " + sentence + ")"
}

/*
object FOLFCAsk {
  def apply(KB: Set[FOLDefiniteClause], alpha: AtomicSentence): Option[Map[Variable,Term]] = {

    //separate implication rules and just literal from the KB
    val rules = KB.filter(_.isRule)
    val atoms = KB.filter(!_.isRule)

    def loop: Option[Map[Variable,Term]] = {

      def rulesLoop(KB: Set[FOLDefiniteClause], rules: List[FOLDefiniteClause], shouldLoopContinue: Boolean): Option[Map[Variable,Term]] = {
        rules match {
          case rule :: rest =>
            val clause = standardizeVariables(rule) //TODO: define standardizeVariables
          
            def unifierLoop(unifiers: List[Map[Variable,Term]],neW: Set[PositiveLiteral]): Option[Map[Variable,Term]] =
              unifiers match {
                case unifier :: rest =>
                  val qPrime = Subst(Set(unifier:_*), clause.conclusion) //TODO: ??
                  if (Fetch(KB, qPrime).isEmpty && Fetch(neW, qPrime).isEmpty) { //TODO: Fetch?
                    //add qPrime to new
                    val phi = Unify(qPrime,alpha) //TODO: Unify for AtomicSentence
                    if(phi != None) Some(phi)
                    else unifierLoop(rest,neW + qPrime)
                  }
                  else unifierLoop(rest,neW)
                case Nil => rulesLoop(KB ++ neW,rest, !neW.isEmpty)
              }
            unifierLoop(Fetch(KB,clause.premise),Set[FOLDefiniteClause].empty) //TODO
          case Nil if shouldLoopContinue => loop
          case _ => None
        }
      }
      rulesLoop(KB,rules,false)
    }

    loop
  }
}
*/





/*
object FOLBCAsk {
}
*/
