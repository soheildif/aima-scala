package aima.planning.classical

import aima.search.uninformed.DepthFirstTreeSearch
import aima.search.Success

/** GRAPHPLAN algorithm, described in Fig 10.9
 *
 * @author Himanshu Gupta
 */
object GraphPlan {
  def apply(problem: ClassicalPlanningProblem) = {
    
    def loop(tl: Int, graph: PlanningGraph): Option[List[Set[Action]]] =
      if (goals.subsetOf(graph.stateLevel(tl).freeItems)) {
        val sol = extractSolution(graph,problem)
        if(sol != None) sol
        else {
          if (graph.isLeveledOff) None
          else loop(tl+1, graph.expandGraph)
        }
      }
      else {
        if (graph.isLeveledOff) None
        else loop(tl+1, graph.expandGraph)
      }

    loop(0,new PlanningGraph(problem))
  }

  def extractSolution(graph: PlanningGraph, cpp: ClassicalPlanningProblem, n: Int) = {
    //Formulate as Search Problem
    val sp = new SearchProblem(graph,cpp,n)
    DepthFirstTreeSearch(sp) match {
      case Success(result) => Some(result)
      case _ => None
    }
  }
}


class SearchProblem(planningGraph: PlanningGraph, planningProblem: ClassicalPlanningProblem, lastLevelNum: Int)
extends aima.search.Problem[(Set[Literal],Int),Set[Action]] {

  override def initialState = (planningProblem.goals,lastLevelNum)

  override def goalTest(s: (Set[Literal],Int)) =
    s._1.filter(_.isPositive).subsetOf(planningProblem.initState)

  override def actions(s: (Set[Literal],Int)) =
    s match {
      case (literals,n) if n > 0 =>
        //Find Set of Actions from An-1
        val actionLevel = planningGraph.actionLevel(n-1)
        //Find non-conflicting set of actions
        var as = actionLevel.freeItems
        //Find the subset, whose effects cover the literals
        //take the only actions whose effects contribute to
        //literals
        as = as.filter(!(_.effects ** literals).isEmpty)

        //find all subsets of above and filter the ones those
        //not satisfying the literals
        getAllSubsetsOf(as).filter( //TODO
          ts =>
            if(ts.isEmpty) false
            else ts.flatMap(_.effects) == literals
        ).toList
        
      case _ => Nil
  }

  override def result(s: (Set[Literal],Int), a: Set[Action]) = (a.flatMap(_.preconditions),s._2 - 1)
  
}
  
  
  
