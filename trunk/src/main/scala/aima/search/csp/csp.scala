package aima.search.csp 

// Algorithms to Solve Constraint Satisfaction Problems with Binary Constraints



/** Implementation of AC-3 and MAC (Maintaining Arc Consistancy)
 * inferencing algorithms
 * @author Himanshu Gupta
 */
object Inference {

  //************************ ARC ************************
  //
  //One binary constraint leads to two arcs(or variable pairs)
  //in the queue, we also keep the constraint between those
  //variables in the queue, so queue is a list of triplets
  //For example a binary constraint, c with 2 variables x1
  //and x2 results in two triplets in the queue
  //(x1,x2,c) and (x2,x1,c)


  /** AC-3 algorithm, described in Fig 6.3
   * 
   * A slight variation is that instead of returning
   * true/false it returns a CSP with inconsistant
   * values removed from domain of various variables
   * or None if domain of one of the variables becomes
   * empty.
   *
   * @author Himanshu Gupta
   */
  def AC3[K,V](csp: CSP[K,V]): Option[CSP[K,V]] = {

    //Prepare the queue containing all the arcs
    var queue = List[(K,K,Constraint[K,V])]()
    csp.constraints.foreach(c => {
                              val (x,y) = c.variables
                              queue = (x,y,c) :: (y,x,c) :: queue })

    makeArcConsistant(csp,queue)
  }

  //TODO: MAC is similar stuff
//  def MAC[K,V](csp: CSP[K,V]): Option[CSP[K,V]] = {    

  //Returns None if current input assignment is not possible
  //Or else returns a new CSP with inconsistant values of various
  //variables from their domain removed
  private def makeArcConsistant[K,V](csp: CSP[K,V], queue: List[(K,K,Constraint[K,V])]): Option[CSP[K,V]] = {
      queue match {
        case (x,y,c) :: rest => {
          revise(csp,x,y,c) match {
            case None =>  //No change to the domain of x
              makeArcConsistant(csp,rest)
            case Some(domainX) => //domain of x reduced
              if (domainX.isEmpty) None
              else {
                val neighbours = csp.neighbours(x) //returns a list of pair (neighbour,constraint)
                var newQ = rest
                neighbours.foreach(nbr => if(nbr._1 != y) newQ = (nbr._1,x,nbr._2) :: newQ)
                makeArcConsistant(csp.clone(x -> domainX),newQ)
              }
          }
        }
        case Nil => Some(csp)
      }
  }

  //Returns new domain of x if changed or None
  private def revise[K,V](csp: CSP[K,V], x: K, y: K, constraint: Constraint[K,V]): Option[List[V]] = {
    val domainXi = csp.domain(x)
    val domainYi = csp.domain(y)
    val consistantDomainXi = csp.domain(x).filter( xi =>
                                        domainYi.exists( yi => constraint.isSatisfied(Map(x -> xi, y -> yi))))
    if(consistantDomainXi.length != domainXi.length)
      Some(consistantDomainXi)
    else None
  }
}

/** Backtracking-Search, described in Fig 6.5
 *
 * @author Himanshu Gupta
 */
object BacktrackingSearch {
  //BacktrackingSearch without any inferencing
  def apply[K,V](csp: CSP[K,V]) =
    backtrack(Map[K,V](),csp,identityInference)


  //BacktrackingSearch with given inference algorithm
  def apply[K,V](csp: CSP[K,V],inference: (Map[K,V],CSP[K,V])=>Option[CSP[K,V]]) =
    backtrack(Map[K,V](),csp,inference)

  def backtrack[K,V](assignment: Map[K,V],csp: CSP[K,V], inference: (Map[K,V],CSP[K,V])=>Option[CSP[K,V]]): Option[Map[K,V]] = {
    if (csp.isComplete(assignment)) Some(assignment)
    else {
      var variable = selectUnassignedVariable(csp.variables,assignment,csp) match {
        case Some(s) => s
        case None => throw new IllegalStateException("No unassigned variable found.")}

      val values = orderDomainValues(variable,assignment,csp)
      
      def loop(values: List[V]): Option[Map[K,V]] =
        values match {
          case value :: rest => {
            val newAssignment = assignment + (variable -> value)
            if (csp.isAssignmentOk(newAssignment)) {
              inference(newAssignment,csp) match {
                case None => loop(rest)
                case Some(newCsp) =>
                  val result = backtrack(newAssignment,newCsp,inference)
                  if (result != None) result else loop(rest)
              }
            }
            else loop(rest)
          }
          case Nil => None
        }
      loop(values)
    }
  }

  private def selectUnassignedVariable[K,V](variables: List[K],assignment: Map[K,V],csp: CSP[K,V]) =
    variables.find(!assignment.contains(_))

  private def orderDomainValues[K,V](variable: K, assignment: Map[K,V], csp: CSP[K,V]) =
    csp.domain(variable)

  //An inference that does no inferencing
  private def identityInference[K,V](assignment: Map[K,V], csp: CSP[K,V]) = Some(csp)
}


/** Min-Conflicts, described in Fig 6.8
 *
 * @author Himanshu Gupta
 */
object MinConflicts {
  def apply[K,V](csp: CSP[K,V],maxSteps: Int) = {
    
    def loop(current: Map[K,V],count: Int): Option[Map[K,V]] = {
      if(count < maxSteps) {
        if (csp.isComplete(current)) Some(current)
        else {
          var variable = randomlyChoseVariableInConflict(csp,current)
          val value = valueThatMinimizesConflicts(variable,current,csp)
          loop(current + (variable -> value),count+1)
        }
      }
      else None }

    loop(randomFullAssignment(csp),0)
  }

  //Returns a randomly generated full assignment that may or
  //may not satisfy all the constraints
  private def randomFullAssignment[K,V](csp: CSP[K,V]) = {
    
    val random = new scala.util.Random(new java.util.Random)
    def randomValue(variable: K) = {
      val domain = csp.domain(variable)
      if(domain.length > 0)
        domain(random.nextInt(domain.length))
      else
        throw new IllegalStateException("domain for " + variable + " is empty.")
    }

    Map[K,V]() ++ (csp.variables.map((x) => (x, randomValue(x))))
  }

  private def valueThatMinimizesConflicts[K,V](variable: K, assignment: Map[K,V], csp: CSP[K,V]) = {
    val domain = csp.domain(variable)
    domain.foldLeft(domain.head)( (p,q) => if (csp.constraintsInConflict(assignment + (variable -> p)).length <
                                                                  csp.constraintsInConflict(assignment + (variable -> q)).length)
                                                            p
                                                            else q )
  }

  //returns a variable(randomly chosen,higher probability for higher number of conflicts) in conflict, 
  //provided given assignment is not partial and atleast one constraint is broken
  private def randomlyChoseVariableInConflict[K,V](csp: CSP[K,V], assignment: Map[K,V]) = {
    val random = new scala.util.Random(new java.util.Random)
    def loop(constraints: List[Constraint[K,V]], variablesInConflict: List[K]): List[K] =
      constraints match {
        case c :: rest => {
          if (!c.isSatisfied(assignment)) {
            val (x,y) = c.variables
            (assignment.contains(x),assignment.contains(y)) match {
              case (true,true) => loop(rest, x :: y :: variablesInConflict)
              case (true,false) => loop(rest,x :: variablesInConflict)
              case (false,true) => loop(rest, y :: variablesInConflict)
              case (false,false) => loop(rest, variablesInConflict)
            }
          }
          else loop(rest, variablesInConflict) }
        case Nil => variablesInConflict
      }
    loop(csp.constraints,Nil) match {
      case Nil => throw new IllegalStateException("assignment is either complete or partial.")
      case x => x(random.nextInt(x.length)) }
    
  }
}

/*
object CSPSolver {
  
  //------------ Backtracking Search ----------------
  def BacktrackingSearch[K,V](csp: CSP[K,V]) = RecursiveBacktracking(Map[K,V](),csp,identityInference)

  def RecursiveBacktracking[K,V](assignment: Map[K,V],csp: CSP[K,V], inference: (Map[K,V],CSP[K,V])=>Option[CSP[K,V]]): Option[Map[K,V]] = {
    if (csp.isComplete(assignment)) Some(assignment)
    else {
      var variable = selectUnassignedVariable(csp.variables,assignment,csp) match {
        case Some(s) => s
        case None => throw new IllegalStateException("No unassigned variable found.")}

      val values = orderDomainValues(variable,assignment,csp)
      
      def loop(values: List[V]): Option[Map[K,V]] =
        values match {
          case value :: rest => {
            val newAssignment = assignment + (variable -> value)
            if (csp.isAssignmentOk(newAssignment)) {
              inference(newAssignment,csp) match {
                case None => loop(rest)
                case Some(newCsp) =>
                  val result = RecursiveBacktracking(newAssignment,newCsp,inference)
                  if (result != None) result else loop(rest)
              }
            }
            else loop(rest)
          }
          case Nil => None
        }
      loop(values)
    }
  }

  private def identityInference[K,V](assignment: Map[K,V],csp: CSP[K,V]) = Some(csp)

  private def selectUnassignedVariable[K,V](variables: List[K],assignment: Map[K,V],csp: CSP[K,V]) =
    variables.find(!assignment.contains(_))

  private def orderDomainValues[K,V](variable: K, assignment: Map[K,V], csp: CSP[K,V]) =
    csp.domain(variable)

  //----------- MIN-CONFLICTS ------------
  def MinConflicts[K,V](csp: CSP[K,V],maxSteps: Int) = {
    
    def loop(current: Map[K,V],count: Int): Option[Map[K,V]] = {
      if(count < maxSteps) {
        if (csp.isComplete(current)) Some(current)
        else {
          var variable = randomlyChoseVariableInConflict(csp,current)
          val value = valueThatMinimizesConflicts(variable,current,csp)
          loop(current + (variable -> value),count+1)
        }
      }
      else None }

    loop(randomFullAssignment(csp),0)
  }

  //Returns a randomly generated full assignment that may or
  //may not satisfy all the constraints
  private def randomFullAssignment[K,V](csp: CSP[K,V]) = {
    
    val random = new scala.util.Random(new java.util.Random)
    def randomValue(variable: K) = {
      val domain = csp.domain(variable)
      if(domain.length > 0)
        domain(random.nextInt(domain.length))
      else
        throw new IllegalStateException("domain for " + variable + " is empty.")
    }

    Map[K,V]() ++ (csp.variables.map((x) => (x, randomValue(x))))
  }

  private def valueThatMinimizesConflicts[K,V](variable: K, assignment: Map[K,V], csp: CSP[K,V]) = {
    val domain = csp.domain(variable)
    domain.foldLeft(domain.head)( (p,q) => if (csp.constraintsInConflict(assignment + (variable -> p)).length <
                                                                  csp.constraintsInConflict(assignment + (variable -> q)).length)
                                                            p
                                                            else q )
  }

  //returns a variable(randomly chosen,higher probability for higher number of conflicts) in conflict, 
  //provided given assignment is not partial and atleast one constraint is broken
  private def randomlyChoseVariableInConflict[K,V](csp: CSP[K,V], assignment: Map[K,V]) = {
    val random = new scala.util.Random(new java.util.Random)
    def loop(constraints: List[Constraint[K,V]], variablesInConflict: List[K]): List[K] =
      constraints match {
        case c :: rest => {
          if (!c.isSatisfied(assignment)) {
            val (x,y) = c.variables
            (assignment.contains(x),assignment.contains(y)) match {
              case (true,true) => loop(rest, x :: y :: variablesInConflict)
              case (true,false) => loop(rest,x :: variablesInConflict)
              case (false,true) => loop(rest, y :: variablesInConflict)
              case (false,false) => loop(rest, variablesInConflict)
            }
          }
          else loop(rest, variablesInConflict) }
        case Nil => variablesInConflict
      }
    loop(csp.constraints,Nil) match {
      case Nil => throw new IllegalStateException("assignment is either complete or partial.")
      case x => x(random.nextInt(x.length)) }
    
  }  
}*/

//TODO: write comments to describe the code and data structures
//Various consistancy check algorithms





