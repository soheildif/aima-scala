package aima.basic

//I'm using immutable Map for Percept.

//Any object that can be placed in the Environment
class Object {
  import scala.collection.mutable.Map
  private val attributes:Map[Any,Any] = Map()
  
  def setAttribute(key:Any, value:Any) {
    attributes += (key -> value)
  }

  def getAttribute(key:Any) = attributes(key)
}

//Agent
object Agent {
  val NoOp = "NoOp" 
}

abstract class Agent extends Object{

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
   val table = init

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
