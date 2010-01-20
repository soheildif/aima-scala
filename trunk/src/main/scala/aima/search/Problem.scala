package aima.search

/** A generic representation for Problem as described in section-3.1.1
 *
 * @author Himanshu Gupta
 */
abstract class Problem[S, A](initState: S){
  def initialState: S = initState
  def goalTest(s: S): Boolean

  //TODO: needs to be removed
  def successorFn(s: S): List[(A,S)]

  def actions(s: S): List[A]
  def result(s: S,a: A): S
  
  //provided one can go from TO to in single step, what is the
  //cost
  def stepCost(from: S, to: S): Double
  //def stepCost(from: S, action: A, to: S): Double

  //estimated cost to reach goal from
  //given state, heuristic function : h(n)
  def estimatedCostToGoal(from: S): Double
}
