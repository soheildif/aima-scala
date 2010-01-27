package aima.search.csp

import org.scalatest.Suite
import org.scalatest.ImpSuite

class AC3Test extends Suite {

  /* This tests the AC3 algorithm with problem described
   * in section 6.2.2
   *
   * There are two variables x, y and constraint is y = x*x
   * we begin with all 0-9 in the domain of both the variables.
   * After AC3 is applied, domain of x should become {0,1,2,3}
   * and that of y should become {0,1,4,9}
   */
  def testAC3() {
    val X = "X"
    val Y = "Y"
    val constraint = new Constraint[String,Int](X,Y) {
      def isSatisfied(assignment: Map[String,Int]) =
        (assignment.get(X),assignment.get(Y)) match {
          case (Some(valX),Some(valY)) => valY == valX*valX
          case _ => false
        }
    }

    val domain = List(0,1,2,3,4,5,6,7,8,9)

    val csp = new CSP[String,Int]()
    csp.addConstraints(constraint)
    csp.addVariables((X,domain),(Y,domain))

    Inference.AC3(csp) match {
      case None => assert(false)
      case Some(csp) =>
        val domainX = csp.domain(X)
        val domainY = csp.domain(Y)

        assert(domainX.length == 4)
        assert(domainY.length == 4)
        
        val testDomainX = List(0,1,2,3)
        val testDomainY = List(0,1,4,9)

        assert(domainX.forall( x => testDomainX.exists(_ == x)))
        assert(domainY.forall( y => testDomainY.exists(_ == y)))
    }
  }
}

class BacktrackingSearchTest extends Suite {

  /*******************************************************/
  /**** Backtracking-Search Tests with NO Inferencing ****/
  /*******************************************************/

  def testBacktrackingSearchForAustraliaMapColorCSP() {
    import AustraliaMapColorCSP._

    BacktrackingSearch(csp) match {
      case None => assert(false)
      case Some(assignment) => {
        assert(assignment.getOrElse(Wa,-1) == Blue)
        assert(assignment.getOrElse(Nt,-1) == Red)
        assert(assignment.getOrElse(Q,-1) == Blue)
        assert(assignment.getOrElse(Sa,-1) == Green)
        assert(assignment.getOrElse(Nsw,-1) == Red)
        assert(assignment.getOrElse(V,-1) == Blue)
        assert(assignment.getOrElse(T,-1) == Red ||
               assignment.getOrElse(T,-1) == Blue ||
               assignment.getOrElse(T,-1) == Green)
      }
    }
  }

  def testBacktrackingSearchForNQueensCSP() {
    
    val csp = NQueensCSP.csp(8)
    BacktrackingSearch(csp) match {
      case None => assert(false)
      case Some(assignment) =>
        println("Nqueeens solution found: " + csp.toString(assignment))        
        assert(true)
    }
  }

  /*******************************************************/
  /**** Backtracking-Search Tests with MAC Inferencing ***/
  /*******************************************************/
}

class MinConflictsTest extends Suite {
  
  def testIt() {
    MinConflicts(AustraliaMapColorCSP.csp,100) 
  }
}
