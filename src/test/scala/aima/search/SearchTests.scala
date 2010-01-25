package aima.search

import org.scalatest.Suite
import org.scalatest.ImpSuite

class CSPTest extends Suite {

  def testBacktrackingSearchForAustraliaMapColorCSP() {
    import AustraliaMapColorCSP._

    CSPSolver.BacktrackingSearch(csp) match {
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
    CSPSolver.BacktrackingSearch(csp) match {
      case None => assert(false)
      case Some(assignment) =>
        println("Nqueeens solution found: " + csp.toString(assignment))        
        assert(true)
    }
  }

  def testMinConflictsForAustraliaMapColorCSP() {
    CSPSolver.MinConflicts(AustraliaMapColorCSP.csp,100) 
  }
}
