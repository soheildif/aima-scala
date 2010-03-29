package aima.uncertainity

/** Bayesian Network Representation, as described in Sec 14.1
 *
 * NOTE: Parents(or Causes) must be added *before* Children(or Effects)
 *
 * @author Himanshu Gupta
 */
class BayesNet {

  private var _variables:List[RandomVariable] = Nil
  private var _nodesMap = Map[RandomVariable,Node]()

  private def findNode(X: RandomVariable) =
    _nodesMap.get(X) match {
      case Some(n) => n
      case None => throw new IllegalArgumentException(X + " has never been added to this bayes network")
    }
  
  //Returns P(X=x|Parents(X))
  def getProbability(X: RandomVariable, x: String, conditions: Set[(RandomVariable,String)]) =
    findNode(X).cpt.get(conditions + (X->x)) match {
      case Some(p) => p
      case None =>
        throw new IllegalArgumentException("Could not find posterior probability for " + X + " = " + x + " with conditions " + conditions)
    }

  def parents(X: RandomVariable): Set[RandomVariable] = findNode(X).parents.map(_.variable)

  //topologically sorted list of variables
  def variables = _variables

  def add(variable: RandomVariable, parents: Set[RandomVariable], cpt: Map[Set[(RandomVariable,String)],Double]): BayesNet = {
    //make sure this variable is not already added
    if(!_nodesMap.contains(variable)) {
      val node = new Node(variable,parents.map(findNode(_)),cpt)
      _nodesMap = _nodesMap + (variable -> node)
      _variables = _variables ::: List(variable)
      this
    }
    else  throw new RuntimeException(variable + " has already been added to this Bayes net")
  }
}

//Representation for a node in the Bayes Network
class Node(val variable: RandomVariable, val parents: Set[Node],
           val cpt: Map[Set[(RandomVariable, String)],Double]) {

  //for all parents, add this node as child node
  parents.foreach(_.addChild(this))

  private var _children = Set[Node]()

  def children = _children

  def addChild(node: Node) { _children = _children + node }
}
