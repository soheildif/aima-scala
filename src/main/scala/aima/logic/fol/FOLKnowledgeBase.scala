package aima.logic.fol

/** FOL KnowledgeBase representation.
 *
 * @author Himanshu Gupta
 */
class FOLKnowledgeBase {

  //sentences told so far in original form
  private var originalSentences = Set[String]()

  //sentences in parsed Sentence form
  private var sentences = Set[Sentence]()

  //sentences in CNF form
  private var clauses = Set[Clause]()

  //indexes
  private var predicates = Map[String,Set[Predicate]]()
/*
  def tell(s: String) {
    originalSentences = originalSentences + s

    val sentence = parser.parse(domain,s)
    sentences = sentences + sentence

    indexIt(sentence) //index the just received sentence

    //convert to CNF form and store
    val cnfSentence = SentenceToCNF(sentence)
    cnfSentences = cnfSentences + cnfSentence
  }

  //FETCH - described in 1st paragraph, section 9.2.3
  //
  //Returns list of all unifiers that unifies input sentence
  //with some sentence(s) in the KB

  //has to be defined for
  //Set[FOLDefiniteClause],PositiveLiteral
  //Set[PositiveLiteral], PositiveLiteral
  //Set[FOLDefiniteClause],FOLDefiniteClausePremise

  def fetch(s: AtomicSentence): Set[Map[Variable,Term]] = {
    s match {
      case x: Predicate =>
        predicates.get(x.symbol) match {
          case None => Set[Map[Variable,Term]].empty
          case Some(ps) => fetch(s,ps)
        }
      case x: Equal => //TODO: do it
    }
  }

  def fetch(l: Literal): Set[Map[Variable,Term]] = fetch(l.sentence)
*/
  //Returns set of unifiers that unifies s with elements in set
/*  private def fetch[T](s: T, set: Set[T]): Set[Map[Variable,Term]] =
    set.map(unify(s,_) match {
              case None => null
              case Some(x) => x
            }).filter(_ != null)

  
  */

  //Generators for unique constant, Variable and Function
  //Not: These are not thread-safe
  private var seqConstant = 0
  private var seqVariable = 0
  private var seqFunction = 0

  def generateConstant = {
    seqConstant = seqConstant + 1
    Constant("C$$" + seqConstant)
  }

  def generateVariable: Variable = generateVariable("")
  def generateVariable(prefix: String) = {
    seqVariable = seqVariable + 1
    Variable(prefix + "$$" + seqVariable)
  }

  def generateFunction(args: List[Term]) = {
    seqFunction = seqFunction + 1
    new Function("F$$" + seqFunction, args:_*)
  }
}
