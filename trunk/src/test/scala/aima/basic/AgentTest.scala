package aima.basic

//Tests
import org.scalatest.Suite
import VacuumWorld._

//TableDrivenVacuumAgent test
class TableDrivenVacuumAgentTest extends Suite {

  def testCleanClean() {
    template("Clean", "Clean","RightLeft")
  }

  def testCleanDirty() {
    template("Clean", "Dirty", "RightSuck")
  }

  def testDirtyClean() {
    template("Dirty", "Clean","SuckRight")
  }

  def testDirtyDirty() {
    template("Dirty", "Dirty", "SuckRight")
  }

  private def template(statusA:String, statusB:String, expectedResult:String) {
    var result = ""
    val tve = new TrivialVacuumEnvironment[TableDrivenVacuumAgent](statusA, statusB)
    tve.addAgent(new TableDrivenVacuumAgent("A"))
    tve.registerView((action:Option[String]) => 
                        action match {
                          case None => ; //nothing
                          case Some(x) => result += x })
    tve.stepUntilNoOp()
    assert(expectedResult == result)
  }
}

//ReflexVacuumAgent test
class ReflexVacuumAgentTest extends Suite {

  def testCleanClean() {
    template("Clean", "Clean","RightLeftRightLeftRightLeftRightLeft", 8)
  }

  def testCleanDirty() {
    template("Clean", "Dirty", "RightSuckLeftRightLeftRightLeftRight", 8)
  }

  def testDirtyClean() {
    template("Dirty", "Clean","SuckRightLeftRightLeftRightLeftRight", 8)
  }

  def testDirtyDirty() {
    template("Dirty", "Dirty", "SuckRightSuckLeftRightLeftRightLeft", 8)
  }

  private def template(statusA:String, statusB:String, expectedResult:String, steps:Int) {
    var result = ""
    val tve = new TrivialVacuumEnvironment[ReflexVacuumAgent](statusA, statusB)
    tve.addAgent(new ReflexVacuumAgent("A"))
    tve.registerView((action:Option[String]) => 
                        action match {
                          case None => ; //nothing
                          case Some(x) => result += x })
    tve.step(steps)
    assert(expectedResult == result)
  }
}

//SimpleReflexVacuumAgent test
class SimpleReflexVacuumAgentTest extends Suite {

  def testCleanClean() {
    template("Clean", "Clean","RightLeftRightLeftRightLeftRightLeft", 8)
  }

  def testCleanDirty() {
    template("Clean", "Dirty", "RightSuckLeftRightLeftRightLeftRight", 8)
  }

  def testDirtyClean() {
    template("Dirty", "Clean","SuckRightLeftRightLeftRightLeftRight", 8)
  }

  def testDirtyDirty() {
    template("Dirty", "Dirty", "SuckRightSuckLeftRightLeftRightLeft", 8)
  }

  private def template(statusA:String, statusB:String, expectedResult:String, steps:Int) {
    var result = ""
    val tve = new TrivialVacuumEnvironment[SimpleReflexVacuumAgent](statusA, statusB)
    tve.addAgent(new SimpleReflexVacuumAgent("A"))
    tve.registerView((action:Option[String]) => 
                        action match {
                          case None => ; //nothing
                          case Some(x) => result += x })    
    tve.step(steps)
    assert(expectedResult == result)
  }
}

//ReflexVacuumAgentWithState test
class ReflexVacuumAgentWithStateTest extends Suite {

  def testCleanClean() {
    template("Clean", "Clean","Right")
  }

  def testCleanDirty() {
    template("Clean", "Dirty", "RightSuck")
  }

  def testDirtyClean() {
    template("Dirty", "Clean","SuckRight")
  }

  def testDirtyDirty() {
    template("Dirty", "Dirty", "SuckRightSuck")
  }

  private def template(statusA:String, statusB:String, expectedResult:String) {
    var result = ""
    val tve = new TrivialVacuumEnvironment[ReflexVacuumAgentWithState](statusA, statusB)
    tve.addAgent(new ReflexVacuumAgentWithState("A"))
    tve.registerView((action:Option[String]) => 
                        action match {
                          case None => ; //nothing
                          case Some(x) => result += x })
    tve.stepUntilNoOp()
    assert(expectedResult == result, result)
  }
}
