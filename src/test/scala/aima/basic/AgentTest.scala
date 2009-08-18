package aima.basic

//Tests
import org.scalatest.Suite

//TableDrivenAgent test
class TableDriveAgentTest extends Suite{
  def testIt() {
    val ps1:List[Map[Any,Any]] = List(Map("key1" -> "val1"))
    val ps2:List[Map[Any,Any]] = List(Map("key1" -> "val1"),
                   Map("key1" -> "val2"))
    val ps3:List[Map[Any,Any]] = List(Map("key1" -> "val1"),
                   Map("key1" -> "val2"),
                   Map("key1" -> "val3"))
    val table = Map(ps1 -> "action1", ps2 -> "action2", ps3 -> "action3")
    val agent = new TableDrivenAgent(table)
    assert("action1" == agent.execute(Map[Any,Any]("key1" -> "val1")))
    assert("action2" == agent.execute(Map[Any,Any]("key1" -> "val2")))
    assert("action3" == agent.execute(Map[Any,Any]("key1" -> "val3")))
    assert("NoOp" == agent.execute(Map[Any,Any]("key1" -> "val4")))
  }
}

//TableDrivenVacuumAgent test
class TableDrivenVacuumAgentTest extends Suite {

  private var agent:TableDrivenVacuumAgent = null
  private var result:String = ""

  def setUp() {
    agent = new TableDrivenVacuumAgent()
    result = "";
  }

  def testCleanClean() {
    template("Clean", "Clean","RightLeftRightNoOp")
  }

  def testCleanDirty() {
    template("Clean", "Dirty", "RightSuckLeftNoOp")
  }

  def testDirtyClean() {
    template("Dirty", "Clean","SuckRightLeftNoOp")
  }

  def testDirtyDirty() {
    template("Dirty", "Dirty", "SuckRightSuckNoOp")
  }

  private def template(statusA:String, statusB:String, expectedResult:String) {
    setUp()
    val tve:TrivialVacuumEnvironment = new TrivialVacuumEnvironment(statusA, statusB)
    tve.addAgent(agent, "A")
    tve.registerView((cmd:String) => result += cmd)
    tve.stepUntilNoOp()
    assert(expectedResult == result)
  }
}

//ReflexVacuumAgent test
class ReflexVacuumAgentTest extends Suite {

  private var agent:ReflexVacuumAgent = null
  private var result:String = ""

  def setUp() {
    agent = new ReflexVacuumAgent()
    result = "";
  }

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
    setUp()
    val tve:TrivialVacuumEnvironment = new TrivialVacuumEnvironment(statusA, statusB)
    tve.addAgent(agent, "A")
    tve.registerView((cmd:String) => result += cmd)
    tve.step(8)
    assert(expectedResult == result)
  }
}

//SimpleReflexVacuumAgent test
class SimpleReflexVacuumAgentTest extends Suite {

  private var agent:SimpleReflexVacuumAgent = null
  private var result:String = ""

  def setUp() {
    agent = new SimpleReflexVacuumAgent()
    result = "";
  }

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
    setUp()
    val tve:TrivialVacuumEnvironment = new TrivialVacuumEnvironment(statusA, statusB)
    tve.addAgent(agent, "A")
    tve.registerView((cmd:String) => result += cmd)
    tve.step(8)
    assert(expectedResult == result)
  }
}

