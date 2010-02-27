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

  //all definite clauses
  private var definiteClauses = Set[FOLDefiniteClause]()

  //all Implication Definite Clauses
  private var implicationDefiniteClauses = Set[ImplicationDefiniteClause]()

  //simple definite clauses
  private var simpleDefiniteClauses = Set[AtomicSentence]()

  //all predicates
  private var predicates = Map[String,Set[Predicate]]()

  //all equals
  private var equals = Set[Equal]()


  //TELL a sentence to this KB
  def tell(s: String) {
    originalSentences = originalSentences + s
    store(FOLParser.parse(s))
  }

  //index the sentence
  def indexIt(s: Sentence) =
    s match {
      case x: Predicate =>
        predicates.get(x.symbol) match {
          case None => predicates = predicates + (x.symbol -> Set(x))
          case Some(ps) =>
            predicates = predicates + (x.symbol -> (ps + x))
        }
      case x: Equal =>
        equals = equals + x
      case _ => ; //do nothing
    }


  //STORE - described in 1st paragraph, section 9.2.3
  //
  //Stores a sentence in KB
  def store(sentence: Sentence) {
    //TODO: should we check if the sentence already in KB
    sentences = sentences + sentence

    indexIt(sentence) //index the just received sentence

    //convert to CNF form and store
    val newClauses = SentenceToCNF(sentence,this)
    clauses = clauses ++ newClauses
  }


  //FETCH - described in 1st paragraph, section 9.2.3
  //
  //Returns list of all unifiers that unifies input sentence
  //with some sentence(s) in the KB
  def fetch(s: AtomicSentence): Set[Map[Variable,Term]] = {
    s match {
      case x: Predicate =>
        predicates.get(x.symbol) match {
          case None => Set[Map[Variable,Term]]()
          case Some(ps) => fetch(x,ps)
        }
      case x: Equal => fetch(x,equals)
    }
  }

  //ss are conjuncts of some sentence 
  def fetch(ss: Set[AtomicSentence]): Set[Map[Variable,Term]] = {
      
    def converge(s1: Set[Map[Variable,Term]], s2: Set[Map[Variable,Term]]) =
      s1.flatMap(m =>
        s2.map(Unify.merge(_,m)).filter(_ == None)).map(_.get)

    ss.map(fetch(_)).reduceLeft(converge(_,_))    
  }

  //def fetch(l: Literal): Set[Map[Variable,Term]] = fetch(l.sentence)

  //Returns set of unifiers that unifies s with elements in set
  def fetch[T](s: T, set: Set[T]): Set[Map[Variable,Term]] =
    set.map(Unify(s,_,Some(Map[Variable,Term]())) match {
              case None => null
              case Some(x) => x
            }).filter(_ != null)

  

  //Generators for unique(for this KB) constant, 
  //Variable and Function
  private var seqConstant = 0
  private var seqVariable = 0
  private var seqFunction = 0

  def generateConstant =
    synchronized {
      seqConstant = seqConstant + 1
      Constant("C$$" + seqConstant)
    }

  def generateVariable: Variable = generateVariable("")
  def generateVariable(prefix: String) =
    synchronized {
      seqVariable = seqVariable + 1
      Variable(prefix + "$$" + seqVariable)
    }

  def generateFunction(args: List[Term]) =
    synchronized {
      seqFunction = seqFunction + 1
      new Function("F$$" + seqFunction, args:_*)
    }
}
