package aima.planning.classical

/** PLANNING-GRAPH, described in section 10.3
 *
 * @author Himanshu Gupta
 */
class PlanningGraph(problem: ClassicalPlanningProblem) {

  private var _currStateLevel = 0 //max current state level up

  var stateLevels = Map[Int,StateLevel]()
  var actionLevels = Map[Int,ActionLevel]()

  def init {
    //create Level S0
    val allPositives = collectPositiveLiterals(actionSchema)
    val literals = problem.initState ++ (allPositives - problem.initState).map(_.makeNegative)
    val mutexes = getLiteralMutexes(literals, None)
    stateLevels = Map(0 -> new StateLevel(literals,mutexes))
  }

  def expandGraph = {
    //generate next action level
    val actions = problem.actions.filter(_.preconditions.subsetOf(stateLevels(_currStateLevel)))
    actionLevels = actionLevels + (_currStateLevel -> new ActionLevel(actions,getActionMutexes(actions,stateLevels(_currStateLevel))))

    //generate next state level
    val literals = stateLevels(_currStateLevel) ++ actions.flatMap(_.effects)
    stateLevels = stateLevels + (_currStateLevel+1 ->
                                   new StateLevel(literals, getLiteralMutexes(literals,actionLevels(_currStateLevel))))

    //increment currStateLevel
    _currStateLevel = _currStateLevel + 1
    
    this
  }
    

  def getLiteralMutexes(literals: Set[Literal], prevLevel: Option[ActionLevel]): Set[(Literal,Literal)]
  def getActionMutexes(actions: Set[Action], prevLevel: StateLevel): Set[(Action,Action)]
}

class Level[A](val items: Set[A], val mutexes: Set[(A,A)]) {

  //Returns the ones which are not part of any mutex pair
  def freeItems: Set[A] = {
    items.filter( //TODO
}
class StateLevel(literals: Set[Literal], ms: Set[(Literal,Literal)]) extends Level[Literal](literals,ms)
class ActionLevel(actions: Set[Action], ms: Set[(Action,Action)]) extends Level[Action](actions,ms)

