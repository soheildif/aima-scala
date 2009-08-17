package aima.basic

abstract class Environment {
  var objects:List[EnvironmentObject] = List()
  var agents:List[Agent] = List()
  private var views:List[BasicEnvironmentView] = List()
  
  //abstract methods
  def executeAction(a:Agent, act:String):Unit
  def getPerceptSeenBy(a:Agent):Percept[Any, Any]
  
  def registerView(bev:BasicEnvironmentView) {
    objects = bev :: objects;
  }

  def updateViews(cmd:String) {
    views.foreach(view =>
      view.envChanged(cmd))
  }

  def isDone = !agents.exists(a => a.isAlive())

  def createExogenousChange() {}

  def step() {
    if(!this.isDone) {
      agents.foreach(agent =>
        val anAction = agent.execute(this.getPerceptSeenBy(agent))
        updateViews(anAction)
        this.executeAction(agent, anAction))
      this.createExogenousChange()
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

  def alreadyContains(o:EnvironmentObject) = objects.exists(_ == o)

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
  
  def getAll = objects ::: agents
  
}

//Environment class
//Any physical non-agent object that can appear inside an environment
abstract class EnvironmentObject

//BasicEnvironmentView
class BasicEnvironmentView {
  def envChanged(cmd:String) { println(cmd) }
}

