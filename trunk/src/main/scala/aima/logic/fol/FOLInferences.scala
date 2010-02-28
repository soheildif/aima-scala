package aima.logic.fol


object FOLFCAsk {
  def apply(KB: FOLKnowledgeBase, alpha: AtomicSentence): Option[Set[Map[Variable,Term]]] = {

    //TODO: check that KB contains Definite Clauses only

    //As explained in section 9.4.1, a query like Person(x) can be proved with
    //multiple substitutions like x -> John and x -> Richard
    //so the result is a Set of all substitutions possible
    var result = Set[Map[Variable,Term]]()

    //Add already alpha matching sentences from KB
    //to the result set
    result = result ++ KB.fetch(alpha)

    println("Alpha is: " + alpha)
    val rules = KB.implicationDefiniteClauses
    println("rules: " + rules)

    def loop: Option[Set[Map[Variable,Term]]] = {

      def rulesLoop(KB: FOLKnowledgeBase, rules: List[ImplicationDefiniteClause], shouldLoopContinue: Boolean): Option[Set[Map[Variable,Term]]] = {
        rules match {
          case rule :: rest =>
            val clause = standardizeVariables(rule,KB)
            println("rules are: " + rules)
            println("standardized rule is: " + clause)
          
            def unifierLoop(unifiers: List[Map[Variable,Term]],neW: Set[AtomicSentence]): Option[Set[Map[Variable,Term]]] = {
              println("Unifiers are: " + unifiers)
              println("new: " + neW)
              unifiers match {
                case unifier :: rest =>
                  val qPrime = Subst(unifier, clause.conclusion)
                  if (KB.fetch(qPrime).isEmpty && KB.fetch(qPrime,neW).isEmpty) {
                    val phi = Unify(qPrime,alpha)
                    if(phi != None)
                      result = result + phi.get
                    unifierLoop(rest,neW + qPrime)
                  }
                  else unifierLoop(rest,neW)
                case Nil => rulesLoop(KB.tell(neW),rest, !neW.isEmpty)
              }}
            unifierLoop(KB.fetch(clause.premise).toList,Set[AtomicSentence]())
          case Nil if shouldLoopContinue => loop
          case Nil if result.isEmpty => None
          case Nil => Some(result)
        }
      }
      rulesLoop(KB,rules.toList,false)
    }

    loop
  }

  def standardizeVariables(c: ImplicationDefiniteClause,KB: FOLKnowledgeBase) = {
    val theta = Map(CollectVariables(c).map(
      v => v -> KB.generateVariable(v.symbol)).toList:_*)
    new ImplicationDefiniteClause(
      Subst(theta,c.premise), Subst(theta,c.conclusion))
  }
}


object FOLBCAsk {
  
  private type Result = Set[Map[Variable,Term]]
  
  def apply(KB: FOLKnowledgeBase, query: AtomicSentence): Option[Result] =
    FOLBCOr(KB,query,empty)

  def FOLBCOr(KB: FOLKnowledgeBase, goal: AtomicSentence, theta: Option[Result]): Option[Result] = {
    
    var result = Set[Map[Variable,Term]]()

    val rules = FetchRulesForGoal(KB,goal)
    for(rule <- rules) {
      val stRule = standardizeVariables(rule);
      val (lhs,rhs) = stRule match {
        case x: AtomicSentence => (Nil,x)
        case x: ImplicationDefiniteClause => (x.premise.toList,x.conclusion)
      }

      FOLBCAnd(KB,lhs,Unify(rhs,goal,theta)) match {
        case Some(s) => result = result ++ s
        case None => ; //do nothing
      }
    }

    if(result.isEmpty) None
    else Some(result)
  }

  def FOLBCAnd(KB: FOLKnowledgeBase, goals: List[AtomicSentence], theta: Option[Map[Variable,Term]]) =
    if(theta == None) None
    else {
      if(goals.isEmpty) theta
      else {
        var result = Set[Map[Variable,Term]]()
        val first :: rest = goals

        FOLBCOr(KB, first, theta) match {
          case None => None
          case Some(s) =>
            def iter(s: List[Map[Variable,Term]], result: Set[Map[Variable,Term]]) =
              s match {
                case m :: restM =>
                  FOLBCAnd(KB, rest, m) match {
                    case None => None
                    case Some(x) =>
                      iter(restM,x.map(Unify.merge(_,m)).filter(_ != None) ++ result)
                  }
                case Nil => Some(result)
              }
          iter(s,Set())
      }
    }
  }

  def FetchRulesForGoal(KB: FOLKnowledgeBase,goal: AtomicSentence): Set[FOLDefiniteClause] =
    KB.definiteClauses.filter(
      _ match {
        case x: AtomicSentence => Unify(x,goal) != None
        case x: ImplicationDefiniteClause => Unify(x.conclusion,goal) != None
      })

  def standardizeVariables(c: FOLDefiniteClause, KB: FOLKnowledgeBase): FOLDefiniteClause =
    c match {
      case x: AtomicSentence =>
        Subst(Map(CollectVariables(x).map(
          v => v -> KB.generateVariable(v.symbol)).toList:_*),x)
      case x: ImplicationDefiniteClause =>
        val theta = Map(CollectVariables(c).map(
          v => v -> KB.generateVariable(v.symbol)).toList:_*)
        new ImplicationDefiniteClause(
        Subst(theta,c.premise), Subst(theta,c.conclusion))
    }
}

