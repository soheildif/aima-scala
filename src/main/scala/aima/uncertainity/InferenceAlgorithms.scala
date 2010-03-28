package aima.uncertainity

/** ENUMERATION-ASK, described in Fig 14.9
 *
 * @author Himanshu Gupta
 */
object EnumerationAsk {
  def apply(X: RandomVariable, e: Map[RandomVariable,String], bn: BayesNet): Map[String,Double] = {

    val q = X.domain.map(x => (x -> enumerateAll(bn.variables, e + (X -> x),bn))).
      foldLeft(Map[String,Double]())(_ + _)
    val alpha = 1/q.values.reduceLeft(_ + _) //normalization constant
    q.transform((_,v) => alpha*v)
  }

  def enumerateAll(vars: List[RandomVariable], e: Map[RandomVariable,String], bn: BayesNet): Double =
    vars match {
      case Nil => 1.0
      //Parents(y) will always be present in the evidence e
      case y :: rest if e.contains(y) =>
        bn.getProbability(y,e(y),bn.parents(y).map(x => (x -> e(x)))) * enumerateAll(rest,e,bn)
      case y :: rest =>
        y.domain.map(v => bn.getProbability(y,v,bn.parents(y).map(x => (x -> e(x)))) * enumerateAll(rest,e + (y -> v),bn)).reduceLeft(_ + _)
    }
}


//Fig 14.11, VariableElimination Algorithm
object EnumerationAskWithVariableElimination {

  type Factor = (RandomVariable,Map[RandomVariable,String]) //(X,evidence)

  def apply(X: RandomVariable, e: Map[RandomVariable,String], bn: BayesNet): Map[String,Double] = {

    def hidden(x: RandomVariable) = x != X && !e.contains(x)

    var factors = collectFactors(e,bn)
//    for(x <- order(bn.variables)) {
//      factors = new Factor(x,e) :: factors
//      if(hidden(x))
//        factors = sumOut(x,factors)
//    }
    //get factors after summing 
    normalize(pointwiseProduct(factors))
  }

  private def order(variables: List[Variable]) = variables

  private def collectFactors(e: Map[RandomVariable,String], bn: BayesNet) =
    Set(Set(bn.variables.map(x => new AtomFactor(x,e)):_*))

  private def pointwiseProduct(f1: Factor, f2: Factor): Factor = {
    //find common set of variables
    val cmns = f1 ** f2
  }
    
  private def sumOut(x: RandomVariable, factors: Set[Factor]): Set[Factor] = {
    //take the relevant ones
    val relevants = factors.filter(_.variables.exists(x == _))

    val others = factors - relevants

    
    
    
}

class Factor(variables: Set[RandomVariable], ptable: Map[Set[(RandomVariable,String)],Double])




/*
//Fig 14.13
object PriorSample {
  //todo: how do we fix a topology
  def apply(bn: BayesNet): List[String] =
    bn.variables.map(x => randomSample(x,bn.getProbability(x,parents)))

  //Returns one value from domain of x, as per given probability
  def randomSample(x: RandomVariable, prob: List[Double]) = {
  }
}

//Fig 14.14
object RejectionSampling {
  def apply(query: RandomVariable, e: Map<RandomVariable,String>, bn: BayesNet, n: Int): Double = {
    
    def loop(n: Int, sample: List<Int>): List<Int> =
      if(n > 0) {
        val event = PriorSample(bn)
        if(isConsistent(event,e)){
          //find value of query variable in it and add to sample
          loop(n-1,sample)
        } 
      }
      else sample

    normalize(loop(n, list-of-zeros))
  }
}
*/
