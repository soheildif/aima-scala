//@Author: Himanshu Gupta

package aima.search

//Assignment is of type Map[K,V]

object CSPSolver {
  
  //------------ Backtracking Search ----------------
  def BacktrackingSearch[K,V](csp: CSP[K,V]) = RecursiveBacktracking(Map[K,V](),csp)

  def RecursiveBacktracking[K,V](assignment: Map[K,V],csp: CSP[K,V]): Option[Map[K,V]] = {
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
              val result = RecursiveBacktracking(newAssignment,csp)
              if (result != None) result else loop(rest)
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

  //----------- MIN-CONFLICTS ------------
  /*def MinConflicts[K,V](csp,maxSteps: Int) = {
    
    def loop(current: Map[K,V],count: Int): Option[Map[K,V]] =
      if(count < maxSteps) {
        if (csp.isComplete(current)) Some(current)
        else {
          var variable = randomlyChosenConflictedVariable(csp,current)
          val value = valueThatMinimizesConflicts(dadadada)
          loop(current + (variable -> value),count+1)
        }
      }
      else None

    loop(randomCompleteAssignment(csp),0)
  }

  private def valueThatMinimizesConflicts[K,V](variable: K, assignment: Map[K,V], csp: CSP[K,V]) = {
    val domain = csp.domain(variable)
    domain.foldLeft(domain.head)( (p,q) => if (csp.constraintsInConflict(assignment + (variable -> p)).length <
                                                                  csp.constraintsInConflict(assignment + (variable -> q)).length)
                                                            p
                                                         else q )
  }*/

  
}


abstract class CSP[K,V] {

  def constraints: List[Constraint[K,V]]
  def variables: List[K]
  def domain(variable: K): List[V]

  def isComplete(assignment: Map[K,V]) =
    constraints.forall(_.isSatisfied(assignment)) &&
    variables.forall(assignment.contains(_))

  //checks if a partial assignment is not broken
  def isAssignmentOk(assignment: Map[K,V]) =
    constraints.forall(
      (constraint) => {
        !constraint.variables.forall(assignment.contains(_)) ||
        constraint.isSatisfied(assignment) })

  //returns List of Constraints which are in Conflict
  def constraintsInConflict(assignment: Map[K,V]) =
    constraints.filter(_.isSatisfied(assignment))
}

abstract class Constraint[K,V] {
  def variables: List[K] //variables involved in this constraint
  def isSatisfied(assignment: Map[K,V]): Boolean
}


//Map Coloring Constraint Satisfaction Problem
class MapColorCSP extends CSP[String,Int] {


  private var _variableMap = Map[String,List[Int]]()
  private var _constraints = List[MapColorConstraint]()

  def constraints = _constraints
  def variables = List.fromIterator(_variableMap.keys)
  def domain(variable: String) =
    _variableMap.get(variable) match {
      case Some(x) => x
      case None => throw new IllegalStateException("domain for " + variable + " not found.")
    }

  def addVariables(variableAndDomains: (String,List[Int]) *) {
    _variableMap = _variableMap ++ variableAndDomains
  }

  def addConstraints(constraints: MapColorConstraint *) {
    _constraints = _constraints ++ constraints
  }
}

class MapColorConstraint(x1: String, x2: String) extends Constraint[String,Int] {

  def variables = List(x1,x2)

  def isSatisfied(assignment: Map[String,Int]) = {
    val tmp = assignment.get(x1)
    (tmp != None) && (tmp != assignment.get(x2))
  }
}

//Factory for Australia Map color CSP

object AustraliaMapColorCSP {
  val Red = 0
  val Green = 1
  val Blue = 2

  private val domain = List(Red,Green,Blue)

  val Wa = "WA"
  val Nt = "NT"
  val Q = "Q"
  val Sa = "SA"
  val Nsw = "NSW"
  val V = "V"
  val T = "T"

  def csp = {
    
    val prob = new MapColorCSP()

    prob.addVariables((Wa,domain),
                      (Nt,domain),
                      (Q,domain),
                      (Sa,domain),
                      (Nsw,domain),
                      (V,domain),
                      (T,domain))                      
    prob.addConstraints(new MapColorConstraint(Wa,Nt),
                       new MapColorConstraint(Nt, Sa),
                       new MapColorConstraint(Wa, Sa),
                       new MapColorConstraint(Nt, Q),
                       new MapColorConstraint(Q, Sa),
                       new MapColorConstraint(Nsw, Sa),
                       new MapColorConstraint(Q, Nsw),
                       new MapColorConstraint(V, Sa),
                       new MapColorConstraint(V, Nsw))
    prob }
}
