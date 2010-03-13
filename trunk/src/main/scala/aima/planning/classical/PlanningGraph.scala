package aima.planning.classical

/** PLANNING-GRAPH, described in section 10.3
 *
 * @author Himanshu Gupta
 */
class PlanningGraph(problem: ClassicalPlanningProblem) {

  private var _currStateLevel = 0 //max current state level up
  init //create S0 level

  private var stateLevels = Map[Int,StateLevel]()
  private var actionLevels = Map[Int,ActionLevel]()

  private def init {
    //create Level S0
    val allPositives = collectPositiveLiterals(problem.actions)
    val literals = problem.initState ++ (allPositives - problem.initState).map(_.makeNegative)
    val mutexes = getLiteralMutexes(literals, None)
    stateLevels = Map(0 -> new StateLevel(literals,mutexes))
  }

  def expandGraph = {
    //generate next action level
    val actions = problem.actions.filter(_.preconditions.subsetOf(stateLevels(_currStateLevel))) ++
      stateLevels(_currStateLevel).items.map(getNoOp(_)) //No-Ops
    actionLevels = actionLevels + (_currStateLevel -> new ActionLevel(actions,getActionMutexes(actions,stateLevels(_currStateLevel))))

    //generate next state level
    val literals = stateLevels(_currStateLevel) ++ actions.flatMap(_.effects)
    stateLevels = stateLevels + (_currStateLevel+1 ->
                                   new StateLevel(literals, getLiteralMutexes(literals,actionLevels(_currStateLevel))))

    //increment currStateLevel
    _currStateLevel = _currStateLevel + 1
    
    this
  }
    
  def stateLevel(n: Int) = stateLevels(n)
  def actionLevel(n: Int) = actionLevels(n)

  def isLeveledOff: Boolean =
    if(_currStateLevel == 0) false
    else stateLevels(_currStateLevel) == stateLevels(_currStateLevel - 1)

  private def getLiteralMutexes(literals: Set[Literal], prevLevel: Option[ActionLevel]): Set[(Literal,Literal)]
  private def getActionMutexes(actions: Set[Action], prevLevel: StateLevel): Set[(Action,Action)]
  private def getNoOp(literal: Literal): Action
  private def collectPositiveLiterals(Set[Action]) : Set[Literal]
  private def makeNegative(l: Literal): NegativeLiteral
}

class Level[A](val items: Set[A], val mutexes: Set[(A,A)]) {

  //Returns the ones which are not part of any mutex pair
  def freeItems: Set[A] = {
    items.filter( //TODO
}
class StateLevel(literals: Set[Literal], ms: Set[(Literal,Literal)]) extends Level[Literal](literals,ms)
class ActionLevel(actions: Set[Action], ms: Set[(Action,Action)]) extends Level[Action](actions,ms)

