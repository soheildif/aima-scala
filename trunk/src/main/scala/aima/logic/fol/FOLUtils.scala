package aima.logic.fol

/** Returns given Sentence/Clause/Term.. after making the
 * substitutions given in theta.
 *
 * @author Himanshu Gupta
 */
object Subst {

  //for Term
  def apply(theta: Map[Variable,Term], alpha: Term) =
    alpha match {
      case x: Constant => x
      case x: Variable =>
        if(theta.contains(x))
          theta(x)
        else x
      case x: Function => new Function(x.symbol, x.args.map(apply(theta,_)):_*)
    }

  //for FOL Sentence
  def apply(theta: Map[Variable,Term], alpha: Sentence) =
    alpha match {
      case x: Predicate => new Predicate(x.symbol, x.args.map(apply(theta,_)):_*)
      case x: Equal => new Equal(apply(theta,x.lTerm),apply(theta,x.rTerm))
      case x: Negation =>
        new Negation(apply(theta,x.sentence))
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(apply(theta,n.sentence)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(apply(theta,x.sentence)).toList:_*)
      case x: Conditional =>
        new Conditional(apply(theta,x.premise),apply(theta,x.conclusion))
      case x: BiConditional =>
        new BiConditional(apply(theta,x.condition),apply(theta,x.conclusion))
      case x: UniversalQuantifier =>
        if(theta.contains(x.variable))
          throw new IllegalStateException(x.variable + " should not occur in " + theta)
        else
          new UniversalQuantifier(x.variable,apply(theta,x.sentence))
      case x: ExistentialQuantification =>
        if(theta.contains(x.variable))
          throw new IllegalStateException(x.variable + " should not occur in " + theta)
        else
          new ExistentialQuantifier(x.variable,apply(theta,x.sentence))
    }

  //for Positive/Negative Literal
  def apply[T <: Literal](theta: Map[Variable,Term], alpha: T): T =
    alpha match {
      case p: PositiveLiteral => PositiveLiteral(apply(theta,alpha.sentence))
      case n: NegativeLiteral => NegativeLiteral(apply(theta,alpha.sentence))
    }

  //for Clause
  def apply(theta: Map[Variable,Term], alpha: Clause) =
    new Clause(alpha.literals.map(apply(theta,_)).toList:_*)

  //for Set[Clause]
  def apply(theta: Map[Variable,Term], alpha: Set[Clause]) = alpha.map(apply(_))
}

//TODO: complete it
//has to be defined for
//AtomicSentence, Term
/*
object Unify {

  def apply(x: Term, y: Term, theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    (theta,x,y) match {
      case (None,_,_) => None //failure
      case (_,_,_) if x == y => theta
      case (_, a: Variable,_) => UnifyVar(a,y,theta)
      case (_,_,a: Variable) => UnifyVar(a,x,theta)
      case (_,a:Function,b:Function) =>
          if(a.symbol == b.symbol)
            apply(a.args,b.args,theta)
          else None
      case _ => None     
  }

  def apply(x: List[Term], y: List[Term], theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    theta match {
      case None => None //failure
      case Some(m) =>
        (x,y) match {
          case (aX :: restX, aY :: restY) => apply(restX,restY,apply(aX,aY,theta))
          case (Nil, Nil) => theta
          case _ => None
        }
    }
    
  def UnifyVar(v: Variable, x: Term, theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    theta match {
      case None => None //failure
      case Some(m) if m.contains(v) => apply(m(v),x,theta)
      case Some(m) =>
        x match {
          case a: Variable if m.contains(a) => apply(v,m(a),theta)
          //TODO: case occur-check => failure
          case _ => Some(m + (v -> x))
        }
    }

    def apply(x: AtomicSentence, y: AtomicSentence, theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
      (x,y) match {
        case (a: Predicate, b: Predicate) =>
          if(a.symbol == b.symbol) apply(a.args,b.args,theta)
          else None
        case (a: Equal, b: Equal) => //TODO: do it
        case _ => None
      }
}
*/
