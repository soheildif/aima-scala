package aima.search

/** A generic binary constraint implementation to model
 * map color problems like the one described in section 6.1.1
 * 
 * @author Himanshu Gupta
 */
class MapColorConstraint(x1: String, x2: String) extends Constraint[String,Int] {

  def variables = List(x1,x2)

  def isSatisfied(assignment: Map[String,Int]) = {
    val tmp = assignment.get(x1)
    (tmp != None) && (tmp != assignment.get(x2))
  }
}
