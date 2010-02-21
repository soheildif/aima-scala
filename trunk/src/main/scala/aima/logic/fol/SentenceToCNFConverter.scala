package aima.logic.fol

/** Converts a sentence to CNF Clause Set
 *
 * @author Himanshu Gupta
 */
object SentenceToCNF {
  def apply(s: Sentence, KB: FOLKnowledgeBase) : CNFSentence =

    //step-1, Implications out, eliminate all the occurences of
    //=> and <=> ops
    var result = removeImplications(s)

    //step-2, Negations In, negations are distributed over other
    //logical operators untill each negation applies only to
    //single Atomic Sentence
    result = negationsIn(result)

    //step-3, Standardize-Variables, rename variables so that each
    //quantifier has a unique variable
    result = standardizeQuantifierVariables(result,KB)

    //step-4, Existentials out,
    //case-1, if there are no free variables then replace quantifier var with
    //skolem constant, a brand new constant symbol
    //case-2, if there are free variables then replace quantifier var with
    //skolem function, with a band new function symbol, that has free variables 
    //as its arguments
    result = removeExistentials(result,KB)

    //step-5, Drop Universal Quatifiers
    result = removeUniversalQuantifiers(result)

    //step-6, distribute \/ and /\ so that sentence becomes conjunction of disjunctions
    result = toConjunctionOfDisjunctions(result)

    //step-7, Rename Variables so that no variable appears in more than one clause
    result = renameVariables(result,KB)

    //do any transformation to result and return it
    result
  }

  def removeImplications(s: Sentence): Sentence =
    s match {
      case x: AtomicSentence => x
      case x: Negation =>
        new Negation(removeImplications(x.sentence))
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(removeImplications(_)).toList:_*)
      case x: Disjunction =>
        new Conjunction(x.disjuncts.map(removeImplications(_)).toList:_*)
      case x: Conditional =>
        new Disjunction(new Negation(x.premise),conclusion)
      case x: BiConditional =>
        new Conjunction(
          new Disjunction(new Negation(x.condition),x.conclusion),
          new Disjunction(x.condition,new Negation(x.conclusion)))
      case x: UniversalQuantifier =>
        new UniversalQuantifier(x.variable,removeImplications(x.sentence))
      case x: ExistentialQuantifier =>
        new ExistentialQuantifier(x.variable,removeImplications(x.sentence))
    }

  def negationsIn(s: Sentence): Sentence = {

    def negationsIn(s:Sentence, negate: Boolean) =
      if(negate) negationsIn(negate(s),false)
      else {
        s match {
          case x: AtomicSentence => x
          case x: Negation =>
            x.sentence match {
              case _: AtomicSentence => x
              case _ => negationsIn(x.sentence,true)
            }
          case x: Conjunction =>
            new Conjunction(x.conjuncts.map(negationsIn(_,false)).toList:_*)
          case x: Disjunction =>
            new Disjunction(x.disjuncts.map(negationsIn(_,false)).toList:_*)
          case x: Conditional => //Implication-Out happens before, this should not occur
            throw new IllegalStateException("Conditional in negationsIn")
          case x: BiConditional => //Implication-Out happens before, this should not occur
            throw new IllegalStateException("BiConditional in negationsIn")
          case x: UniversalQuantifier =>
            new UniversalQuantifier(x.variable,negationsIn(x.sentence,false))
          case x: ExistentialQuantifier =>
            new ExistentialQuantifier(x.variable,negationsIn(x.sentence,false))
        }
      }

    negationsIn(s,false)
  }

  //TODO: Standardize Variables
  def standardizeQuantifierVariables(s: Sentence) =
    s match {
      case x: AtomicSentence => x
      case x: Negation if x.sentence.isInstanceOf[AtomicSentence] => x
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(standardizeQuantifierVariables(_)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.conjuncts.map(standardizeQuantifierVariables(_)).toList:_*)
      case x: UniversalQuantifier =>
        val newVar = KB.generateVariable
        new UniversalQuantifer(newVar, 
                               standardizeQuantifierVariables(Subst(Map(x.variable -> newVar),
                                                                    x.sentence)))
      case x: ExistentialQuantifier =>
        val newVar = StandardizeVariable(x.variable)
        new ExistentialQuantifer(newVar, 
                                 standardizeQuantifierVariables(Subst(Map(x.variable -> newVar),
                                                                      x.sentence)))
    }

  //TODO: remove Existentials
  def removeExistentialQuantifiers(s: Sentence) = {
    s match {
      case x: AtomicSentence => x
      case x: Negation if x.sentence.isInstanceOf[AtomicSentence] => x
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(removeExistentialQuantifiers(_)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(removeExistentialQuantifiers(_)).toList:_*)
      case x: UniversalQuantifier =>
        new UniversalQuantifier(x.variable,removeExistentialQuantifiers(x.sentence))
      case x: ExistentialQuantifier =>
        val freeVars = freeVariables(Set[Variable].empty,x)

        if(freeVars.isEmpty) //case1 -> free variables don't exist
          Subst(Map(x.variable -> KB.generateConstant),
                x.sentence)
        else //case2 -> free variables do exist
          Subst(Map(x.variable -> KB.generateFunction(freeVars)),
                x.sentence)
    }

    //TODO: AtomicSentence -> Equal
    //collect Free variables from a Sentence
    def freeVariables(vs: Set[Variable], s: Sentence): Set[Variable] =
      s match {
        case x: Predicate =>
          Set(x.args.flatMap(freeVariables(vs,_)):_*)
        case x: Equal => Set() //TODO
        case x: Negation =>
          freeVariables(vs,x.sentence)
        case x: Conjunction =>
          x.conjuncts.flatMap(freeVariables(vs,_))
        case x: Disjunction =>
          x.disjuncts.flatMap(freeVariables(vs,_))
        case x: UniversalQuantifier => //TODO: should we have an abstract Quantifier with same st
          freeVariables(vs + x.variable, x.sentence)
        case x: ExistentialQuantifier =>
          freeVariables(vs + x.variable, x.sentence)
      }

    //collect Free variables from a Term
    def freeVariables(vs: Set[Variable], t: Term): Set[Variable] =
      t match {
        case x: Constant => Set[Variable].empty
        case x: Variable =>
          if(vs.exists(x == _)) Set[Variable].empty else Set(x)
        case x: Function =>
          Set(args.flatMap(freeVariables(vs,_)):_*)
      }
  }
      

  def removeUniversalQuantifiers(s: Sentence) =>
    s match {
      case x: AtomicSentence => x
      case x: Negation =>
        x.sentence match {
          case _:AtomicSentence => x
          case _ => //negationsIn happened before, this should not happen
            throw new IllegalStateException("Negation of non AtomicSentence in removeUniversalQuantifiers")
        }
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(removeUniversalQuantifiers(_)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(removeUniversalQuantifiers(_)).toList:_*)
      case x: Conditional => //Implication-Out happens before, this should not occur
        throw new IllegalStateException("Conditional in removeUniversalQuantifiers")
      case x: BiConditional => //Implication-Out happens before, this should not occur
        throw new IllegalStateException("BiConditional in removeUniversalQuantifiers")
      case x: UniversalQuantifier => x.sentence //drop it
      case x: ExistentialQuantifier => //removeExistentials happens before, this should not occur
        throw new IllegalStateException("Existential quantifier in removeUniversalQuantifiers")
    }

  def makeCNFSentence(s: Sentence) = {
    
    def convert(s: Sentence): Set[Clause] =
      s match {
        case x: AtomicSentence => Set(new Clause(PositiveLiteral(x)))
        case x: Negation => Set(new Clause(NegativeLiteral(x.sentence.asInstanceOf[AtomicSentence])))
        case x: Conjunction =>
          x.conjuncts.flatMap(convert(_))
        case x: Disjunction =>
          x.disjuncts.map(convert(_).reduceLeft(unionOfTwoClauseSets(_,_)))
      }

    def unionOfTwoClauseSets(cs1: Set[Clause], cs2: Set[Clause]): Set[Clause] =
      for(ci <- cs1; cj <- cs2) yield new Clause(List((ci.literals ++ cj.literals).toList:_*).toList:_*)

    new CNFSentence(convert(s))
  }

  //Rename Variables, so that no two clauses have same variables
  
        

  //Returns negation of a sentence
  def negate(s: Sentence): Sentence =
    s match {
      case x: AtomicSentence => new Negation(x)
      case x: Negation => x.sentence
      case x: Conjunction =>
        new Disjunction(x.conjuncts.map(new Negation(_)).toList:_*)
      case x: Disjunction =>
        new Conjunction(x.disjuncts.map(new Negation(_)).toList:_*)
      case x: Conditional =>
        new Conjunction(x.premise,new Negation(x.conclusion))
      case x: BiConditional =>
        new Disjunction(
          new Conjunction(x.condition, new Negation(x.conclusion)),
          new Conjunction(new Negation(x.condition),x.conclusion))
      case x: UniversalQuantifier =>
        new ExistentialQuantifier(x.variable,new Negation(x.sentence))
      case x: ExistentialQuantifier =>
        new UniversalQuantifier(x.variable,new Negation(x.sentence))
    }
}
