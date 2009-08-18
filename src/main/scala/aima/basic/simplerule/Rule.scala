package aima.basic.simplerule

class Rule(cond:Condition, act:String) {
  require(cond != null)
  require(act != null)
  val condition = cond
  val action = act

  def execute(state:Map[Any,Any]):Boolean = cond.execute(state)
}

abstract class Condition {
  def execute(state:Map[Any,Any]):Boolean
}

class EqualCondition(key:Any, value:Any) extends Condition {
  override def execute(state:Map[Any,Any]):Boolean = {
    if(state(key) == value) true else false
  }
}

class NotCondition(cond:Condition) extends Condition {
  override def execute(state:Map[Any,Any]):Boolean = !cond.execute(state)
}

class AndCondition(left:Condition, right:Condition) extends Condition {
  override def execute(state:Map[Any,Any]):Boolean = left.execute(state) && right.execute(state)
}

class OrCondition(left:Condition, right:Condition) extends Condition {
  override def execute(state:Map[Any,Any]):Boolean = left.execute(state) || right.execute(state)
}

