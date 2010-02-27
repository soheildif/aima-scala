package aima.logic.fol

/*
object FOLFCAsk {
  def apply(KB: FOLKnowledgeBase, alpha: AtomicSentence): Option[Map[Variable,Term]] = {

    //TODO: check that KB contains Definite Clauses only

    //separate implication rules and just literal from the KB
    val rules = KB.filter(_ match {
      case _: ImplicationDefiniteClause => true
      case _: AtomicSentence => false}).map(_.asInstanceOf[ImplicationDefiniteClause])
    val atoms = KB.filter(_ match {
      case _: ImplicationDefiniteClause => false
      case _: AtomicSentence => true})

    def loop: Option[Map[Variable,Term]] = {

      def rulesLoop(rules: List[ImplicationDefiniteClause], shouldLoopContinue: Boolean): Option[Map[Variable,Term]] = {
        rules match {
          case rule :: rest =>
            val clause = standardizeVariables(rule)
          
            def unifierLoop(unifiers: List[Map[Variable,Term]],neW: Set[AtomicSentence]): Option[Map[Variable,Term]] =
              unifiers match {
                case unifier :: rest =>
                  val qPrime = Subst(unifier, clause.conclusion)
                  if (KB.fetch(qPrime).isEmpty && KB.fetch(qPrime,neW).isEmpty) {
                    val phi = Unify(qPrime,alpha)
                    if(phi != None) Some(phi)
                    else unifierLoop(rest,neW + qPrime)
                  }
                  else unifierLoop(rest,neW)
                case Nil => rulesLoop(KB ++ neW,rest, !neW.isEmpty) //TODO: add atomic sentences set to KB
              }
            unifierLoop(KB.fetch(clause.premise),Set[FOLDefiniteClause]())
          case Nil if shouldLoopContinue => loop
          case _ => None
        }
      }
      rulesLoop(KB,rules,false)
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
*/
/*
object FOLBCAsk {
}
*/
