package aima.logic.fol

class FOLKnowledgeBase {

  //sentences told so far in original form
  private var originalSentences = Set[String].empty

  //sentences in parsed Sentence form
  private var sentences = Set[Sentence].empty

  //sentences in CNF form
  private var cnfSentences = Set[CNFSentence].empty

  //indexes
  private var predicates = Map[String,Set[Predicate]].empty
  private var negations = Set[Negation].empty
  private var conjunctions = Set[Conjunction].empty

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

  def fetch(s: Sentence): Set[Map[Variable,Term]] = {
    s match {
      case x: Predicate =>
        predicates.get(x.symbol) match {
          case None => Set[Map[Variable,Term]].empty
          case Some(ps) => fetch(s,ps)
        }
      case x: Equal => //TODO: do it
      case x: Negation => fetch(x,negations)
      case x: Conjunction => fetch(x,conjunctions)
      case x: Disjunction => fetch(x,disjunctions)
      case x: Conditional => fetch(x,conditionals)
      case x: BiConditional => fetch(x,biConditionals)
      case x: UniversalQuantifier => fetch(x,universalQuantifiers)
      case x: ExistentialQuantifier => fetch(x,existentialQuantifiers)
    }
  }

  def fetch(l: Literal): Set[Map[Variable,Term]] = fetch(l.sentence)

  //Returns set of unifiers that unifies s with elements in set
  private def fetch[T](s: T, set: Set[T]): Set[Map[Variable,Term]] =
    set.map(unify(s,_) match {
              case None => null
              case Some(x) => x
            }).filter(_ != null)

  
  
    
    
