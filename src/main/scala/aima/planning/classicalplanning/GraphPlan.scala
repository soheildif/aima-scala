/** GRAPHPLAN algorithm, described in Fig 10.9
 *
 * @author Himanshu Gupta
 */
object GraphPlan {
  def apply(problem: ClassicalPlanningProblem): Option[Sol] = {
    val graph = initPlanningGraph(problem)
    val goals = problem.goal.conjuncts
    val noGoods = Map()
    
    def loop(tl: Int, graph: PlanningGraph): Option[Sol] = {
      if goals.subsetOf(graph.sLevel(tl).nonMutexes) {
        val sol = extractSolution(graph,goals,graph.numLevels,nogoods)
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

class PlanningGraph(initState: Set[Literal], goal: Set[Literal], actionSchema: Set[Action]) {

  private currLevel = -1

  var stateLevels: List[StateLevel]
  var actionLevels: List[ActionLevel]

  def init {
    //create Level S0
    val allPositives = collectPositiveLiterals(actionSchema)
    val literals = initState ++ (allPositives - initState).map(_.makeNegative) :: statelevels
    val mutexes = getMutexes(literals, prevLevel: Option[ActionLevel])
    stateLevels = new StateLevel(literals,mutexes) :: stateLevels
  }

  def createNextLevel {
    actionLevels = getNextActionLevel() :: actionLevels
    stateLevels = getNextStateLevel
  }

  def getNextStateLevel(prevActionLevel: ActionLevel): StateLevel
  def getNextActionLevel(prevStateLevel: StateLevel) : ActionLevel

  def getLiteralMutexes(literals: Set[Literal], prevLevel: Option[ActionLevel]): Set[(Literal,Literal)]
  def getActionMutexes(actions: Set[Action], prevLevel: StateLevel): Set[(Action,Action)]
}

abstract class Level[A] {
  val items: Set[A]
  val mutexes: Set[(A,A)]
}
class StateLevel extends Level[Literal]  //Si
class ActionLevel extends Level[Action]  //Ai
  
        
        
