/** Grammar for FOL logic */

//Sentence equality should work
abstract class Sentence

//Term
abstract class Term
class Constant extends Term
class Function(val symbol, args: Term *) extends Term
class Variable(val key: String) extends Term

//Atomic Sentence
abstract class AtomicSentence extends Sentence
class Predicate(val symbol, val args: Term *) extends AtomicSentence
class Equal(val lTerm: Term, val rTerm: Term) extends AtomicSentence //or we could say = is just a predefined binary predicate

//Complex Sentence
class Negation(val s: Sentence) extends Sentence
class Conjunction(cs: Sentence *) extends Sentence
class Disjunction(ds: Sentence *) extends Sentence
class Conditional(val premise: Sentence, val conclusion: Sentence) extends Sentence
class BiConditional(val condition: Sentence, val conclusion: Sentence) extends Sentence

class UniversalQuantification(val variable: Variable, val sentence: Sentence) extends Sentence
class ExistentialQuentification(val variable: Variable, val sentence: Sentence) extends Sentence


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

//Returns the sentence after applying the susbstitution
def Subst[T](theta: Map[Variable,Term], alpha: T): T

object Unify {

  def apply(x: Term, y: Term, theda: Map[Variable,Term]): Option[Map[Variable,Term]]
  def apply(x: List[Term], y: List[Term], theta: Map[Variable,Term]) : Option[Map[Variable,Term]]

}

object FOLFCAsk {
  def apply(KB, alpha: AtomicSentence): Option[Map[Variable,Term]]
}
