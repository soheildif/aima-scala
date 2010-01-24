package aima.search

/** A generic representation for Problem as described in section-3.1.1
 *
 * @author Himanshu Gupta
 */
abstract class Problem[S, A](initState: S){
  def initialState: S = initState
  def goalTest(s: S): Boolean

  def actions(s: S): List[A]
  def result(s: S,a: A): S
 
  //provided one can go from TO to in single step, what is the
  //cost; c(s,a,s')
  def stepCost(from: S, action: A, to: S): Double

  //estimated cost to reach goal from
  //given state, heuristic function : h(n)
  def estimatedCostToGoal(from: S): Double
}

/** A generic representation for Non-Deterministic Problem as described
 * in section 4.3.1
 *
 * @author Himanshu Gupta
 */
abstract class NonDeterministicProblem[S,A](initState: S) extends Problem[S,A](initState) {

  def results(state: S, action: A): List[S]

  override def result(state: S, action: A):S =
    throw new UnsupportedOperationException("result does not exist for non-deterministic problem.")
}

/** A generic representation for Online-Search-Problem,
 * described in section 4.5.1
 *
 * @author Himanshu Gupta
 */
abstract class OnlineSearchProblem[P,A] {
  
  import scala.collection.mutable.Stack

  def actions(s: P): Stack[A]
  def goalTest(s: P): Boolean
  //heuristic function, estimated cost to reach
  //goal from state "s"
  def h(s: P): Double
  //cost to reach "to" from "from" in one step by
  //taking "action"
  def stepCost(from: P, action: A, to: P): Double
}
