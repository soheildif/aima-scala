package aima.logic.fol

/** Grammar for FOL logic */

//Sentence equality should work
abstract class Sentence

//Term
abstract class Term
class Constant extends Term
class Function(val symbol, val args: Term *) extends Term
class Variable(val key: String) extends Term

//Atomic Sentence
abstract class AtomicSentence extends Sentence
class Predicate(val symbol, val args: Term *) extends AtomicSentence
class Equal(val lTerm: Term, val rTerm: Term) extends AtomicSentence //or we could say = is just a predefined binary predicate

//Complex Sentence
abstract class ComplexSentence extends Sentence //do we need this??
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
 * Domain -> contains Ontology(vocabulary) that is Constant, Predicate and Function symbols(sets of String), has to be defined upfront
 * To be precise, function and predicate airity should also be defined upfront
 *
*/
class Domain {
  private var _predicates = Set[String]()
  private var _functions = Set[String]()
  private var _constants = Set[String]()
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
