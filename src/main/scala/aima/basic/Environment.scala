package aima.basic

abstract class Environment {
  
  import scala.collection.immutable.Map
  var objects:List[Object] = List()
  var agents:List[Agent] = List()
  private var views:List[(String)=>Unit] = List()
  
  //abstract methods
  def executeAction(a:Agent, act:String):Unit
  def getPerceptSeenBy(a:Agent):Map[Any, Any]
  def addAgent(a:Agent):Unit
  
  def registerView(view: (String)=>Unit) {
    views = view :: views
  }

  def updateViews(cmd:String) {
    views.foreach(view => view(cmd))
  }

  def isDone() = !agents.exists(a => a.isAlive())

  def createExogenousChange() {}

  def step() {
    if(!this.isDone) {
      agents.foreach(
        agent => {
        val anAction = agent.execute(this.getPerceptSeenBy(agent))
        updateViews(anAction)
        this.executeAction(agent, anAction) })
      //this.createExogenousChange()
    }
  }

  def step(n:Int) {
    if(n>0) {
      step()
      step(n - 1)
    }
  }

  def stepUntilNoOp() {
    if(!this.isDone()) {
      step()
      stepUntilNoOp()
    }
  }

/*  def alreadyContains(o:EnvironmentObject) = objects.exists(_ == o)

  def alreadyContains(a:Agent) = agents.exists(_ == a)

  def addAgent(a:Agent, attributeName:String, attributeValue:Any) {
    if(!alreadyContains(a)) {
      a.setAttribute(attributeName, attributeValue)
      agents = a :: agents
    }
  }

  def addAgent(a:Agent) {
    if(!alreadyContains(a))
      agents = a :: agents
  }

  def addObject(o:EnvironmentObjet) {
    if(!alreadyContains(o))
      objects = o :: objects
  }

  def addObject(o:EnvironmentObject, aName:String, aValue:Any) {
    if(!alreadyContains(O)) {
      o.setAttribute(aName, aValue)
      objects = o :: objects
    }
  }
  
  def getAll = objects ::: agents */
  
}


import scala.collection.mutable.Map
import scala.util.Random
class TrivialVacuumEnvironment() extends Environment {
  private val locationA = "A"
  private val locationB = "B"
  private val status:Map[String,String] = Map()

  //code for primary constructor, generate random status for both locations
  private val rand = new Random()
  private val tmp1 = if(rand.nextBoolean) "Clean" else "Dirty"
  private val tmp2 = if(rand.nextBoolean) "Clean" else "Dirty"
  status += (locationA -> tmp1)
  status += (locationB -> tmp2)

  //auxiliary constructor
  def this(statusA:String,statusB:String) {
    this()
    status += (locationA -> statusA)
    status += (locationB -> statusB)
  }

  override def executeAction(agent:Agent, action:String) {
    val performance:Int = agent.getAttribute("performance").asInstanceOf[Int]
    if(action == "Right") {
      agent.setAttribute("location", locationB)
      agent.setAttribute("performance", performance - 1)
    }else if(action == "Left") {
      agent.setAttribute("location", locationA)
      agent.setAttribute("performance", performance - 1)
    }else if(action == "Suck") {
      val location:String = agent.getAttribute("location").asInstanceOf[String]
      if(this.status(location) == "Dirty") {
        this.status += (location -> "Clean")
        agent.setAttribute("performance", performance + 10)
      }
    }else if(action == Agent.NoOp) agent.die()
  }

  override def getPerceptSeenBy(a:Agent) = {
    scala.collection.immutable.Map("location" -> a.getAttribute("location"),
                                   "status" -> this.status(a.getAttribute("location").asInstanceOf[String]))
  }
 
  override def addAgent(a:Agent) { addAgent(a, "A") }

  def addAgent(a:Agent, loc:String) {
    a.setAttribute("location", loc)
    a.setAttribute("performance", 0)
    agents = a :: agents
  }
}

