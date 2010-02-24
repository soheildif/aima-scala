package aima.logic.fol

import scala.util.parsing.combinator._

/** Grammar for FOL logic */

//AtomicSentence := Predicate(Term, Term, ...) | Term = Term

//4L stands for FOR-All
//3E stands for THERE EXISTS

//Term = Function(Term,Term,...)| Variable | Constant

object FOLParser extends JavaTokenParsers {

  def sentence: Parser[Sentence] = (
    "4L"~variable~biconditional ^^
    { case _~v~s => new UniversalQuantifier(Variable(v),s) } //Universal Quantifier
    | "3E"~variable~","~biconditional ^^
    { case _~v~s => new ExistentialQuantifier(Variable(v),s) } //Existential Quantifier
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
}



//TODO:Sentence equality should work
abstract class Sentence

//Term
abstract class Term
case class Constant(val symbol: String) extends Term
class Function(val symbol:String, val args: Term *) extends Term
case class Variable(val symbol: String) extends Term

//Atomic Sentence
abstract class AtomicSentence extends Sentence
class Predicate(val symbol: String, val args: Term *) extends AtomicSentence
class Equal(val lTerm: Term, val rTerm: Term) extends AtomicSentence

//Complex Sentence
class Negation(val sentence: Sentence) extends Sentence
class Conjunction(cs: Sentence *) extends Sentence {
  val conjuncts: Set[Sentence] = Set(cs: _*)
}
class Disjunction(ds: Sentence *) extends Sentence {
  val disjuncts: Set[Sentence] = Set(ds: _*)
}
class Conditional(val premise: Sentence, val conclusion: Sentence) extends Sentence
class BiConditional(val condition: Sentence, val conclusion: Sentence) extends Sentence

class UniversalQuantifier(val variable: Variable, val sentence: Sentence) extends Sentence
class ExistentialQuantifier(val variable: Variable, val sentence: Sentence) extends Sentence


/**
 * Domain contains Ontology(vocabulary) that is Constant, Predicate and Function
 * symbols(sets of String).
 * It has to be defined upfront, because functions and predicates are syntactically the
 * same.
 * NOTE:
 *   To be precise, function and predicate arity should also be defined, but
 *   I'm relaxing that for now.
 *
 * @author Himanshu Gupta
 */
/*class Domain(val constants: Set[String], val functions: Set[String],
             val predicates: Set[String]) {
  def addConstants(cs: String *) =
    new Domain(constants ++ cs, functions,predicates)

  def addFunctions(fs: String *) =
    new Domain(constants, functions ++ fs, predicates)

  def addPredicates(ps: String *) =
    new Domain(constants, functions, predicates ++ ps)
}
*/

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
