package aima.logic.fol


object FOLFCAsk {
  def apply(KB: FOLKnowledgeBase, alpha: AtomicSentence): Option[Map[Variable,Term]] = {

    //TODO: check that KB contains Definite Clauses only

    val rules = KB.implicationDefiniteClauses

    def loop: Option[Map[Variable,Term]] = {

      def rulesLoop(KB: FOLKnowledgeBase, rules: List[ImplicationDefiniteClause], shouldLoopContinue: Boolean): Option[Map[Variable,Term]] = {
        rules match {
          case rule :: rest =>
            val clause = standardizeVariables(rule,KB)
          
            def unifierLoop(unifiers: List[Map[Variable,Term]],neW: Set[AtomicSentence]): Option[Map[Variable,Term]] =
              unifiers match {
                case unifier :: rest =>
                  val qPrime = Subst(unifier, clause.conclusion)
                  if (KB.fetch(qPrime).isEmpty && KB.fetch(qPrime,neW).isEmpty) {
                    val phi = Unify(qPrime,alpha)
                    if(phi != None) phi
                    else unifierLoop(rest,neW + qPrime)
                  }
                  else unifierLoop(rest,neW)
                case Nil => rulesLoop(KB.tell(neW),rest, !neW.isEmpty)
              }
            unifierLoop(KB.fetch(clause.premise).toList,Set[AtomicSentence]())
          case Nil if shouldLoopContinue => loop
          case _ => None
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

/*
object FOLBCAsk {
}
*/
