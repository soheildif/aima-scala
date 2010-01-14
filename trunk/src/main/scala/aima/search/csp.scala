//@Author: Himanshu Gupta

package aima.search


//*********************** CSP solver algorithms ***************************************
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
            loop(rest,c.variables.filter(assignment.contains(_)) ++ variablesInConflict)
          }
          else loop(rest, variablesInConflict) }
        case Nil => variablesInConflict
      }
    loop(csp.constraints,Nil) match {
      case Nil => throw new IllegalStateException("assignment is either complete or partial.")
      case x => x(random.nextInt(x.length)) }
    
  }  
}

//*************************** Generic CSP representation ***************************
abstract class Constraint[K,V] {
  def variables: List[K] //variables involved in this constraint
  def isSatisfied(assignment: Map[K,V]): Boolean
}

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
        !constraint.variables.forall(assignment.contains(_)) ||
        constraint.isSatisfied(assignment) })

  //returns List of Constraints which are in Conflict
  def constraintsInConflict(assignment: Map[K,V]) =
    constraints.filter(!_.isSatisfied(assignment))

  //another implementation can return a sensible representation
  //of the current state of the CSP
  //For debugging purposes only.
  def toString(assignment: Map[K,V]): String = { toString() }
}


// *************************** Example CSPs ********************************



// -------------- Map Coloring Constraint Satisfaction Problem ---------------------
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
    
    val prob = new CSP[String,Int]()

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

// ------------------------------- N queens problem ------------------------------
case class Queen(n: Int)
class NQueensConstraint(x1: Queen, x2: Queen) extends Constraint[Queen,Int] {

  def variables = List(x1,x2)

  //check that no two queens are not atacking each other
  //in the given assignment
  def isSatisfied(assignment: Map[Queen,Int]) =
    (assignment.get(x1),assignment.get(x2)) match {
      case(None,_) => false
      case (_, None) => false
      case (Some(qy1),Some(qy2)) if qy1 == qy2 => false
      case (Some(qy1),Some(qy2)) => {
        (x1,x2) match {
          case (Queen(qx1),Queen(qx2)) => Math.abs(qy1 - qy2) != Math.abs(qx1 - qx2) }
      }
    }
}
  
//Factory for N-Queens CSP
object NQueensCSP {
  
  private def generateDomain(n: Int,result: List[Int]): List[Int] = 
    if(n == 0) result else generateDomain(n-1,n :: result)
  
  def csp(n: Int) = {
    val csp = new CSP[Queen,Int]() {
      //overriding toString to print a human-friendly version of
      //the assignment
      override def toString(assignment: Map[Queen,Int]) = {
        val n = assignment.size
        var result = "\n"
        for(y <- n until (0,-1)) {
          for(x <- 1 to n) {
            assignment.get(Queen(x)) match {
              case None => throw new IllegalStateException(Queen(x) + " is not assigned.")
              case Some(yPos) => 
                result = result + (if(y == yPos) "X " else "- ")
            }
          }
          result = result + "\n"
        }
        result
      }
    }

    val domain = generateDomain(n,Nil)
    
    //add n Queens
    for(i <- 1 to n) { csp.addVariables((Queen(i),domain)) }

    //add constraint for every pair of queen
    for(i <- 1 to n; j <- i+1 to n) { csp.addConstraints(new NQueensConstraint(Queen(i),Queen(j))) }

    csp //return the csp
  }
}
