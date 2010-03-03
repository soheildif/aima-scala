package aima.logic.fol


object FOLFCAsk {
  //Empty set returned means failure
  def apply(KB: FOLKnowledgeBase, alpha: AtomicSentence): Set[Map[Variable,Term]] = {

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

    def loop: Set[Map[Variable,Term]] = {

      def rulesLoop(KB: FOLKnowledgeBase, rules: List[ImplicationDefiniteClause], shouldLoopContinue: Boolean): Set[Map[Variable,Term]] = {
        rules match {
          case rule :: rest =>
            val clause = standardizeVariables(rule,KB)
            println("rules are: " + rules)
            println("standardized rule is: " + clause)
          
            def unifierLoop(unifiers: List[Map[Variable,Term]],neW: Set[AtomicSentence]): Set[Map[Variable,Term]] = {
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
          case Nil => result
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
  
  //Empty set returned means failure
  def apply(KB: FOLKnowledgeBase, query: AtomicSentence): Set[Map[Variable,Term]] =
    FOLBCOr(KB,query,Some(Map()))

  def FOLBCOr(KB: FOLKnowledgeBase, goal: AtomicSentence, theta: Option[Map[Variable,Term]]): Set[Map[Variable,Term]] = {    
    var result = Set[Map[Variable,Term]]()

    val rules = FetchRulesForGoal(KB,goal)
    for(rule <- rules) {
      val stRule = standardizeVariables(rule,KB)
      val (lhs,rhs) = stRule match {
        case x: AtomicSentence => (Nil,x)
        case x: ImplicationDefiniteClause => (x.premise.toList,x.conclusion)
      }
      result = result ++ FOLBCAnd(KB,lhs,Unify(rhs,goal,theta))
    }
    result
  }

  def FOLBCAnd(KB: FOLKnowledgeBase, goals: List[AtomicSentence], theta: Option[Map[Variable,Term]]): Set[Map[Variable,Term]] =
    if(theta == None) Set()
    else {
      if(goals.isEmpty) Set(theta.get)
      else {
        val first :: rest = goals

        val firstResults = FOLBCOr(KB, first, theta)
        val restResults = FOLBCAnd(KB, rest, theta)

        firstResults.flatMap(m =>
          restResults.map(Unify.merge(_,m)).filter(_ != None)).map(_.get)
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
        val theta = Map(CollectVariables(x).map(
          v => v -> KB.generateVariable(v.symbol)).toList:_*)
        new ImplicationDefiniteClause(
        Subst(theta,x.premise), Subst(theta,x.conclusion))
    }
}

