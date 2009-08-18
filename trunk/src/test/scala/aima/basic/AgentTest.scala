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
  def testCleanClean() {
    var result = ""
    TrivialVacuumEnvironment tve = new TrivialVacuumEnviroenment("Clean", "Clean")
    tve.addAgent(new TableDrivenVacuumAgent(), "A")
    tve.registerView(_ => result += _)
    tve.stepUntilNoOp()
    assert("RightLeftRightNoOp" == result)
}
