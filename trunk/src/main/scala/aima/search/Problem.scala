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

/** A generic representation for Non-Deterministic Problem as described
 * in section 4.3.1
 *
 * @author Himanshu Gupta
 */
abstract class NonDeterministicProblem[S,A](initState: S) extends Problem[S,A](initState) {

  def results(state: S, action: A): List[S]

  override def result(state: S, action: A):S =
    throw new UnsupportedOperationException("result does not exist for non-deterministic problem.")

  override def successorFn(state: S) =
    throw new UnsupportedOperationException("result does not exist.")
}
