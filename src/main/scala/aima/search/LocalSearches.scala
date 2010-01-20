package aima.search

object Local {

  //returns state that maximizes the value
  def HillClimbingSearch[S, A](problem: Problem[S,A], value: (S)=>Double) = {

    def getHighestValuedSuccessor(s: S): Option[(Double,S)] = {
      val successors = problem.successorFn(s)
      if(!successors.isEmpty) {
        val tmp = successors.map((st) => (value(st._2),st._2))
        Some(tmp.foldLeft(tmp.head)((a,b)=>if(a._1 < b._1) b else a))
      }
      else None
    }
    def loop(current: S): S = {
      getHighestValuedSuccessor(current) match {
        case Some((v,n)) =>
          if(v <= value(current)) current
          else loop(n)
        case None => current
      }
    }
    loop(problem.initialState)
  }

  def SimulatedAnnealingSearch[S, A](problem: Problem[S,A],value: (S)=>Double, schedule: (Int)=>Double) = {

    val random = new scala.util.Random(new java.util.Random)

    def randomSuccessor(s: S): Option[S] = {
      val successors = problem.successorFn(s)
      if(successors.isEmpty) None
      else {
        Some(successors(random.nextInt(successors.length))._2)
      }
    }
 
    def loop(current: S, t: Int): S = {
      val T = schedule(t)
      if(T == 0.0) current
      else {
        randomSuccessor(current) match {
          case Some(next) => {
            val dE = value(next) - value(current)
            if(dE > 0.0) loop(next,t+1)
            else {
              if(Math.random < Math.exp(dE/T)) loop(next,t+1)
              else loop(current,t+1)
            }}
          case None => current
        }
      }
    }
    loop(problem.initialState,1)
  }
}
