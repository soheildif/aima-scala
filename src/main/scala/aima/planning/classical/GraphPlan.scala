package aima.planning.classical

/** GRAPHPLAN algorithm, described in Fig 10.9
 *
 * @author Himanshu Gupta
 */
object GraphPlan {
  def apply(problem: ClassicalPlanningProblem): Option[Sol] = {
    val graph = new PlanningGraph(problem.initState, problem.actions)
    val goals = problem.goals
    val noGoods = Map()
    
    def loop(tl: Int, graph: PlanningGraph): Option[Sol] = {
      if goals.subsetOf(graph.stateLevel(tl).freeLiterals) {
        val sol = extractSolution(graph,goals,nogoods)
        if(sol != None) sol
        
      }

      if (graph.isLeveledOff && nogoods.isLeveledOff) None
      else loop(tl+1, expandGraph(graph))
    }

    loop(0,initPlanningGraph(problem))
  }

  def extractSolution(graph: PlanningGraph, goals: Set[Literal], n: Int, nogoods: Map) =
    BacktrackingSearch(createCSP(graph)) match {
      case None => None
      case Some(m) => //convert to list of actions, return it
  }

  def createCSP(graph: PlanningGraph) = {
    val domain = List(true,false)
    val prob = new CSP[Action,Boolean]()

    for(aLevel <- graph.actionLevels) {
      for(action <- aLevel.actions)
        prob.addVariables((action.toString,0), domain)
      //add all the mutex constraints(mutex is a pair of actions)
      prob.addConstraints(
      aLevel.mutexes.map(_ match {
        case (x,y) => new AllDiffConstraint(x.toString,y.toString)
      }):_*)
    }

    prob
  }
}
