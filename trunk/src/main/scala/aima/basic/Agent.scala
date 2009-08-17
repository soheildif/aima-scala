//I'm using immutable Map for Percept.

//Agent
object Agent {
  val NoOp = "NoOp" 
}

abstract class Agent {

  //the agent program
  def program(p:Map[Any, Any]):String

  var alive = true
  //val environmentSpecificAttributes: HashMap

  //methods 
  def execute(p:Map[Any, Any]) = this.program(p)

  def live() {alive = true}
  def die() {alive = false}

  def isAlive = alive
}


//TableDrivenAgent
class TableDrivenAgent(table:Map[List[Map[Any,Any]], String]) extends Agent {
  //a list of already received percepts
  var percepts:List[Map[Any,Any]] = List()

  override def program(p:Map[Any, Any]):String = {
    percepts = percepts ::: List(p)
    if(table.contains(percepts))
       table(percepts)
    else Agent.NoOp
  }
}

//TableDrivenVacuumAgent
object TableDrivenVacuumAgent {
  //percept sequence table for
  //this agent
  private val table = init

  private def init() ={
    var tmpTable:Map[List[Map[Any,Any]],String] = Map()

    //Level1: 4 states
    var ps:List[Map[Any,Any]] = List(Map("location" -> "A", "state" -> "Clean"))
    tmpTable += (ps -> "Right")
    ps = List(Map("location" -> "A", "state" -> "Dirty"))
    tmpTable += (ps -> "Suck")
    ps = List(Map("location" -> "B", "state" -> "Clean"))
    tmpTable += (ps -> "Left")
    ps = List(Map("location" -> "A", "state" -> "Dirty"))
    tmpTable += (ps -> "Suck")

    //Level2: 4x4 states
    ps = List(
      Map("location" -> "A" , "status" -> "Clean") , Map(
	"location" -> "A" , "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A" , "status" -> "Clean") , Map(
	"location" -> "A" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A" , "status" -> "Clean") , Map(
	"location" -> "B" , "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A" , "status" -> "Clean") , Map(
	"location" -> "B" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");

    ps = List(
      Map("location" -> "A" , "status" -> "Dirty") , Map(
	"location" -> "A" , "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A" , "status" -> "Dirty") , Map(
	"location" -> "A" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A" , "status" -> "Dirty") , Map(
	"location" -> "B" , "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A" , "status" -> "Dirty") , Map(
	"location" -> "B" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");

    ps = List(
      Map("location" -> "B" , "status" -> "Clean") , Map(
	"location" -> "A" , "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B" , "status" -> "Clean") , Map(
	"location" -> "A" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B" , "status" -> "Clean") , Map(
	"location" -> "B" , "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B" , "status" -> "Clean") , Map(
	"location" -> "B" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");

    ps = List(
      Map("location" -> "B" , "status" -> "Dirty") , Map(
	"location" -> "A" , "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B" , "status" -> "Dirty") , Map(
	"location" -> "A" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B" , "status" -> "Dirty") , Map(
	"location" -> "B" , "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B" , "status" -> "Dirty") , Map(
	"location" -> "B" , "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");

    //Level3: 4x4x4 states
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //		
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //	
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //		
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //	
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //	
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    //
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Clean"));
    tmpTable += (ps -> "Right");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "A", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Clean"));
    tmpTable += (ps -> "Left");
    ps = List(
      Map("location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty") , Map(
	"location" -> "B", "status" -> "Dirty"));
    tmpTable += (ps -> "Suck");
    
    tmpTable
  }
}
class TableDrivenVacuumAgent extends Agent {
  private var tableDrivenAgent = new TableDrivenAgent(TableDrivenVacuumAgent.table)

  override def program(p:Map[Any, Any]):String = 
    tableDrivenAgent.program(p)
}
//Tests
import org.scalatest.Suite

//TableDrivenAgent test
class TableDriveAgentTest extends Suite{
  def test() {
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
