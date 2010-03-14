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

  def expandGraph = {
    //generate next action level
    val actions = problem.actions.filter(_.preconditions.subsetOf(stateLevels(_currStateLevel).items)) ++
      stateLevels(_currStateLevel).items.map(getNoOp(_)) //No-Ops
    actionLevels = actionLevels + (_currStateLevel -> new ActionLevel(actions,getActionMutexes(actions)))

    //generate next state level
    val literals = stateLevels(_currStateLevel).items ++ actions.flatMap(_.effects)
    stateLevels = stateLevels + (_currStateLevel+1 ->
                                   new StateLevel(literals, getLiteralMutexes(literals,Some(actionLevels(_currStateLevel)))))

    //increment currStateLevel
    _currStateLevel = _currStateLevel + 1
    
    this
  }
    
  def stateLevel(n: Int) = stateLevels(n)
  def actionLevel(n: Int) = actionLevels(n)

  def isLeveledOff: Boolean =
    if(_currStateLevel == 0) false
    else stateLevels(_currStateLevel) == stateLevels(_currStateLevel - 1)

  private def init {
    //create Level S0
    val allPositives = collectPositiveLiterals(problem.actions)
    val literals = problem.initState ++ (allPositives -- problem.initState).map(makeNegative(_))
    val mutexes = getLiteralMutexes(literals, None)
    stateLevels = Map(0 -> new StateLevel(literals,mutexes))
  }

  private def getLiteralMutexes(literals: Set[Literal], prevLevel: Option[ActionLevel]): Set[(Literal,Literal)] = {

    def isMutex(x: Literal, y: Literal): Boolean =
      (x,y) match {
        //If the two are complementary
        case (_:PositiveLiteral,_:NegativeLiteral) if x.sentence == y.sentence => true
        case (_:NegativeLiteral,_:PositiveLiteral) if x.sentence == y.sentence => true
        //else
        case _ =>
          //if each pair of actions(from prev action level) achieving them is mutex
          prevLevel match {
            case None => false
            case Some(alevel) =>
              //find all pair of actions that achieve x and y
              def doAchieveXY(p: Action, q: Action): Boolean = {
                (p.effects.exists(_ == x) && q.effects.exists(_ == y)) ||
                (p.effects.exists(_ == y) && q.effects.exists(_ == x))
              }

              val ps = getPairsSatisfyingPredicate(alevel.items,doAchieveXY)
              //see if all pairs above are mutex
              ps.filter(
                _ match {
                  case (a1,a2) =>
                    !alevel.mutexes.exists(
                      _ match {
                        case (m1,m2) =>
                          ((a1 == m1) && (a2 == m2)) || ((a1 == m2) && (a2 == m1))
                      })
                }).isEmpty
          }
      }

    getPairsSatisfyingPredicate(literals,isMutex)
  }

  private def getActionMutexes(actions: Set[Action]): Set[(Action,Action)] = {
    
    def isMutex(x: Action, y: Action): Boolean = {
      isInconsistent(x.effects,y.effects) ||             //Inconsistent effects
      isInconsistent(x.preconditions,y.effects) ||       //Interference
      isInconsistent(x.effects,y.preconditions) ||       //Interference
      isInconsistent(x.preconditions,y.preconditions)    //Competing Needs
    }

    def isInconsistent(xs: Set[Literal], ys: Set[Literal]): Boolean =
      xs.exists(x =>
        ys.exists(y =>
          (x,y) match {
            case (_:PositiveLiteral,_:NegativeLiteral) => x.sentence == y.sentence
            case (_:NegativeLiteral,_:PositiveLiteral) => x.sentence == y.sentence
            case _ => false
          }))

    getPairsSatisfyingPredicate(actions,isMutex)
  }


  //Returns all pairs from elements in "items" that satisfy
  //the give "pred" condition
  private def getPairsSatisfyingPredicate[A](items: Set[A], pred: (A,A)=>Boolean): Set[(A,A)] = {
    
    def loop(items: List[A], result: Set[(A,A)]): Set[(A,A)] =
      items match {
        case x :: rest =>
          var tmp = Set[(A,A)]()
          for(y <- rest) {
            if(pred(x,y)) tmp = tmp + ((x,y))
          }
          loop(rest,result ++ tmp)
        case Nil => result
      }

    loop(items.toList,Set.empty)
  }


  private def getNoOp(literal: Literal): Action =
    new Action(new Atom("$NoOp:" + literal.toString + "$",Nil),
               Set(literal),Set(literal))

  private def collectPositiveLiterals(actions: Set[Action]) : Set[Literal] =
    actions.flatMap(a =>
      a.preconditions.filter(_.isPositive) ++ a.effects.filter(_.isPositive))

  private def makeNegative(l: Literal): NegativeLiteral =
    l match {
      case x: PositiveLiteral => NegativeLiteral(x.sentence)
      case x: NegativeLiteral => x
    }
}

class Level[A](val items: Set[A], val mutexes: Set[(A,A)]) {

  //Returns the ones which are not part of any mutex pair
  def freeItems: Set[A] =
    items.filter(
      a => !mutexes.exists(
        _ match {
          case (x,y) => (x == a) || (y == a)
        }))
}
class StateLevel(literals: Set[Literal], ms: Set[(Literal,Literal)]) extends Level[Literal](literals,ms)
class ActionLevel(actions: Set[Action], ms: Set[(Action,Action)]) extends Level[Action](actions,ms)

