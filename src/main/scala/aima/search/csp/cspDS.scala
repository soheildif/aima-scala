package aima.search.csp

//Various CSP data structures

/** Absract Representation for a *binary* Constraint
 *
 * @author Himanshu Gupta
 */
abstract class Constraint[K,V](x1: K, x2: K) {
  
  def variables = (x1,x2)

  //Returns whether given assignment is complete w.r.t
  //this constraint i.e. does given assignment assigns
  //values to x1 and x2 both
  def isComplete(assignment: Map[K,V]) =
    (assignment.get(x1),assignment.get(x2)) match {
      case (Some(_),Some(_)) => true
      case _ => false
    }

  //Returns whether given assignment is complete w.r.t
  //this constraint and values assigned are in agreement
  //with this constraint
  def isSatisfied(assignment: Map[K,V]): Boolean
}

/**A generic All-Diff Constraint, puts the constraint that
 * values to both variables should be different
 *
 * @author Himanshu Gupta
 */
class AllDiffConstraint[K,V](x1: K, x2: K) extends Constraint[K,V](x1,x2) {
  def isSatisfied(assignment: Map[K,V]) =
    (assignment.get(x1),assignment.get(x2)) match {
      case (Some(val1),Some(val2)) => val1 != val2
      case _ => false
    }
}



/** A Generic CSP representation
 *
 * @author Himanshu Gupta
 */
class CSP[K,V] {

  private var _variableMap = Map[K,List[V]]()
  private var _constraints = List[Constraint[K,V]]()

  def constraints = _constraints
  def variables = List.fromIterator(_variableMap.keys)
  def domain(variable: K) =
    _variableMap.get(variable) match {
      case Some(x) => x
      case None => throw new IllegalStateException("domain for " + variable + " not found.")
    }

  //Returns list of pair of (variable,constraint) for all variable
  //which are neighbour of input variable x
  def neighbours(x: K) = {
    
    def loop(constraints: List[Constraint[K,V]], neighbours: List[(K,Constraint[K,V])]): List[(K,Constraint[K,V])] =
      constraints match {
        case c :: rest =>
          val (x1,x2) = c.variables
          if(x1 == x)
            loop(rest,(x2,c) :: neighbours)
          else {
            if(x2 == x)
              loop(rest,(x1,c) :: neighbours)
            else loop(rest,neighbours)
          }
        case Nil => neighbours
      }

    loop(_constraints,Nil)
  }

  def addVariables(variableAndDomains: (K,List[V]) *) {
    _variableMap = _variableMap ++ variableAndDomains
  }

  def addConstraints(constraints: Constraint[K,V] *) {
    _constraints = _constraints ++ constraints
  }

  def isComplete(assignment: Map[K,V]) =
    constraints.forall(_.isSatisfied(assignment)) &&
    variables.forall(assignment.contains(_))

  //checks if a partial assignment is not broken
  def isAssignmentOk(assignment: Map[K,V]) =
    constraints.forall(
      (constraint) => {
        !constraint.isComplete(assignment) ||
        constraint.isSatisfied(assignment) })

  //returns List of Constraints which are in Conflict
  def constraintsInConflict(assignment: Map[K,V]) =
    constraints.filter(!_.isSatisfied(assignment))

  //returns a *new* CSP object with same constraints as *this*
  //with updated domain for given variables in the input
  def clone(variableAndDomains: (K,List[V]) *) = {
    val result = new CSP[K,V]()
    result.setConstraints(_constraints)
    result.setVariables(_variableMap ++ variableAndDomains)
    result
  }

  //Following two methods are convenience methods to be used in
  //"clone" and not be used by any client code
  def setConstraints(constraints: List[Constraint[K,V]]) {
    _constraints = constraints
  }
  def setVariables(variableMap: Map[K,List[V]]) {
    _variableMap = variableMap
  }

  //an implementation can return a sensible representation
  //of the current state of the CSP
  //For debugging purposes only.
  def toString(assignment: Map[K,V]): String = { toString() }
}
