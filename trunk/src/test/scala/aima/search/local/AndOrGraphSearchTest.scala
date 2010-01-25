package aima.search.local

import org.scalatest.Suite
import org.scalatest.ImpSuite

//AndOrSearch Tests
class AndOrGraphSearchTest extends Suite {
  def testIt() {
    val problem = new VacuumWorldNonDeterministicProblem("A")
    AndOrGraphSearch(problem) match {
      case Success(x) => assert(x.toString() == "Suck IF (A,false,false) THEN [NoOp] ELSE  IF (A,false,true) THEN [Right Suck NoOp]")
      case _ => assert(false)
    }
  }
}
