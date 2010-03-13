package aima.planning.classical

/** PLANNING-GRAPH, described in section 10.3
 *
 * @author Himanshu Gupta
 */
class PlanningGraph(initState: Set[Literal], actionSchema: Set[Action]) {

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

class Level[A](val items: Set[A], val mutexes: Set[(A,A)])
class StateLevel(literals: Set[Literal], ms: Set[(Literal,Literal)]) extends Level[Literal](literals,ms)
class ActionLevel(actions: Set[Action], ms: Set[(Action,Action)]) extends Level[Action](actions,ms)
  
