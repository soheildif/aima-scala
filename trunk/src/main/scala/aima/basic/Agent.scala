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

  def isAlive() = alive
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
    var ps:List[Map[Any,Any]] = List(Map("location" -> "A", "status" -> "Clean"))
    tmpTable += (ps -> "Right")
    ps = List(Map("location" -> "A", "status" -> "Dirty"))
    tmpTable += (ps -> "Suck")
    ps = List(Map("location" -> "B", "status" -> "Clean"))
    tmpTable += (ps -> "Left")
    ps = List(Map("location" -> "A", "status" -> "Dirty"))
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

//Reflex Vacuum Agent
class ReflexVacuumAgent extends Agent {
  override def program(p:Map[Any, Any]):String = {
    if(p("status") == "Dirty")
      "Suck"
    else if(p("location") == "A")
      "Right"
    else if(p("location") == "B")
      "Left"
    else Agent.NoOp
  }
}

//Simple Reflex Agent
import aima.basic.simplerule._
class SimpleReflexAgent(rules:List[Rule]) extends Agent {

  override def program(p:Map[Any, Any]):String = {
    val state = interpretInput(p)
    val rule = ruleMatch(state,rules)
    ruleAction(rule)
  }

  private def interpretInput(p:Map[Any,Any]):Map[Any,Any] = p

  private def ruleMatch(state:Map[Any,Any], rules:List[Rule]) =
    (rules.find(_.execute(state))).getOrElse(null)

  private def ruleAction(rule:Rule):String = {
    if(rule != null) rule.action else Agent.NoOp
  }
}
//Simple Reflex Vacuum Agent
class SimpleReflexVacuumAgent extends Agent {
  private var proxy:SimpleReflexAgent = null

  //primary constructor code
  private var rules:List[Rule] = List()
  rules += new Rule(new EqualCondition("status", "Dirty"), "Suck")
  rules += new Rule(new EqualCondition("location", "A"), "Right")
  rules += new Rule(new EqualCondition("location", "B"), "Left")
  proxy = new SimpleReflexAgent(rules)

  override def program(p:Map[Any,Any]):String = proxy.program(p)
}


//Reflex Agent With State
class ReflexAgentWithState(var state:Map[Any,Any],rules:List[Rule],
    updateState:(Map[Any,Any], String, Map[Any,Any]) => Map[Any,Any]) extends Agent {

  private var action:String = null //the most recent action

  override def program(percept:Map[Any, Any]):String = {
    state = updateState(state, action, percept)
    val rule = ruleMatch(state,rules)
    ruleAction(rule)
  }

  private def ruleMatch(state:Map[Any,Any], rules:List[Rule]) =
    (rules.find(_.execute(state))).getOrElse(null)

  private def ruleAction(rule:Rule):String = {
    if(rule != null) rule.action else Agent.NoOp
  }
}
//Reflex Vacuum Agent with State
class ReflexVacuumAgentWithState extends Agent {
  private var proxy:ReflexAgentWithState = null

  //primary constructor code
  private var rules:List[Rule] = List()
  rules += new Rule(new AndCondition(new EqualCondition("statusLocationA", "Clean"),
                                     new EqualCondition("statusLocationB", "Clean")),
                    Agent.NoOp)
  rules += new Rule(new EqualCondition("currentStatus", "Dirty"), "Suck")
  rules += new Rule(new EqualCondition("currentLocation", "A"), "Right")
  rules += new Rule(new EqualCondition("currentLocation", "B"), "Left")
  
  private val updateState = (state:Map[Any,Any], action:String, percept:Map[Any,Any]) =>
    {
      var resultState = state
      resultState += ("currentLocation" -> percept("location"))
      resultState += ("currentStatus" -> percept("status"))
      resultState += ("statusLocation" + percept("location") -> percept("status"))
      resultState
    }
  proxy = new ReflexAgentWithState(Map(),rules, updateState) 

  override def program(p:Map[Any,Any]):String = proxy.program(p)
}
