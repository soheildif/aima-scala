package aima.logic.fol

//TODO: check out what is the best way to impl visitor pattern in
//my case/scala

//object Store

//standardize variables

//Returns the sentence after applying the susbstitution
//has to be defined for
//PositiveLiteral
object Subst {

  //for Term
  def apply(theta: Map[Variable,Term], alpha: Term) =
    alpha match {
      case c: Constant => c
      case v: Variable =>
        if(theta.contains(v)) //TODO: take care of hashcode for variable
          theta(v)
        else v
      case f: Function => new Function(f.symbol, f.args.map(apply(theta,_)):_*)
    }

  //TODO: Build uniqueness of Variables at the time of compiling the
  //sentence
  //for AtomicSentence
  def apply(theta: Map[Variable,Term], alpha: Sentence) =
    alpha match {
      case p: Predicate => new Predicate(p.symbol, p.args.map(apply(theta,_)):_*)
      case e: Equal => new Equal(apply(theta,e.lTerm),apply(theta,e.rTerm))
      case n: Negation =>
        new Negation(apply(theta,n.sentence))
      case c: Conjunction =>
        new Conjunction(c.conjuncts.map(apply(theta,n.sentence)).toList:_*)
      case d: Disjunction =>
        new Disjunction(d.disjuncts.map(apply(theta,n.sentence)).toList:_*)
      case c: Conditional =>
        new Conditional(apply(theta,c.premise),apply(theta,c.conclution))
      case b: BiConditional =>
        new BiConditional(apply(theta,b.condition),apply(theta,d.conclution))
      case u: UniversalQuantifier => //TODO: rethink the correctness
        new UniversalQuantifier(u.variable,apply(theta,u.sentence))
      case e: ExistentialQuantification => //TODO: rethink the correctness
        new ExistentialQuantifier(e.variable,apply(theta,e.sentence))
    }

  //for Positive/Negative Literal
  def apply[T <: Literal](theta: Map[Variable,Term], alpha: T): T =
    alpha match {
      case p: PositiveLiteral => PositiveLiteral(apply(theta,alpha.sentence))
      case n: NegativeLiteral => NegativeLiteral(apply(theta,alpha.sentence))
    }
}

//TODO: complete it
//has to be defined for
//AtomicSentence, Term
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
