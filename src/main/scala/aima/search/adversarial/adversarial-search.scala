package aima.search

object AdversarialSearch {

  def MinimaxDecision[A](state: GameState[A]): A = {

    def MaxValue(state: GameState[A]): Int =
      if (state.terminalTest) state.utility
      else
        state.successors.foldLeft(Math.MIN_INT)( (v,s) => {
                                                    val tmp = MinValue(s)
                                                    if(v > tmp) v else tmp } )

    def MinValue(state: GameState[A]): Int =
      if (state.terminalTest) state.utility
      else
        state.successors.foldLeft(Math.MAX_INT)( (v,s) => {
                                                    val tmp = MaxValue(s)
                                                    if(v < tmp) v else tmp } )
        
    state.action(MaxValue(state))
  }

  def AlphaBetaSearch[A](state: GameState[A]): A = {

    def MaxValue(state: GameState[A],alpha: Int,beta: Int): Int =
      if (state.terminalTest) state.utility
      else {
        def loop(states: List[GameState[A]], v: Int, alpha: Int): Int =
          states match {
            case s :: rest => {
              val tmp = Math.max(v,MinValue(s,alpha,beta))
              if(tmp >= beta) tmp
              else loop(rest, tmp, Math.max(alpha,tmp))
            }
            case Nil => v
          }

        loop(state.successors,Math.MIN_INT,alpha)
      }

    def MinValue(state: GameState[A],alpha: Int, beta: Int): Int =
      if (state.terminalTest) state.utility
      else {
        def loop(states: List[GameState[A]], v: Int, beta: Int): Int =
          states match {
            case s :: rest => {
              val tmp = Math.min(v,MaxValue(s,alpha,beta))
              if(tmp <= alpha) tmp
              else loop(rest, tmp, Math.min(beta,tmp))
            }
            case Nil => v
          }

        loop(state.successors,Math.MAX_INT,beta)
      }

    state.action(MaxValue(state, Math.MIN_INT, Math.MAX_INT))
  }
}

// ------------- Abstract Game State Representation ----------------------
abstract class GameState[A] {
  def successors: List[GameState[A]]
  def terminalTest: Boolean
  def utility: Int

  //Returns the action in Successors of state with value "v"
  def action(v: Int): A
}


