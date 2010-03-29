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

 // type Factor = (RandomVariable,Map[RandomVariable,String]) //(X,evidence)

//  def apply(X: RandomVariable, e: Map[RandomVariable,String], bn: BayesNet): Map[String,Double] = {

//    def hidden(x: RandomVariable) = x != X && !e.contains(x)

//    var factors = collectFactors(e,bn)
//    for(x <- order(bn.variables)) {
//      factors = new Factor(x,e) :: factors
//      if(hidden(x))
//        factors = sumOut(x,factors)
//    }
    //get factors after summing 
//    normalize(pointwiseProduct(factors))
//  }

  private def order(variables: List[RandomVariable]) = variables

//  private def collectFactors(e: Map[RandomVariable,String], bn: BayesNet) =
//    Set(Set(bn.variables.map(x => new AtomFactor(x,e)):_*))

  def pointwiseProduct(f1: Factor, f2: Factor): Factor = {
    //find union of variables in f1 and f2
    val allVars = f1.variables ++ f2.variables
    val size = allVars.size

    val ptbl1 = f1.ptable
    val ptbl2 = f2.ptable

    val ptbl = ptbl1.keySet.foldLeft(Map[Set[(RandomVariable,String)],Double]())(
      (m,pk1) => {
        ptbl2.keySet.foldLeft(m)(
          (m,pk2) => {
            val k = pk1 ++ pk2
            if(k.size == size)
              m + (k -> ptbl1(pk1)*ptbl2(pk2))
            else m
          })})

    new Factor(allVars,ptbl)
  }
    
  private def sumOut(x: RandomVariable, factors: Set[Factor]): Set[Factor] = {
    //take the relevant ones
    val relevants = factors.filter(_.variables.exists(x == _))

    if(relevants.size > 0) {
      val factor = relevants.reduceLeft(pointwiseProduct(_,_))
      (factors -- relevants) + sumOutAFactor(x,factor)
    }
    else factors
  }

  //Sums out given Random variable from a Factor and returns the
  //new Factor
  def sumOutAFactor(x: RandomVariable, factor: Factor): Factor = {
    val otherVars = factor.variables - x
    val allKeys = allCombinations(otherVars)
    
    val oldPtbl = factor.ptable

    val newPtbl = allKeys.foldLeft(Map[Set[(RandomVariable,String)],Double]())(
      (m,a) => m + (a -> oldPtbl.keySet.foldLeft(0.0)(
        (p,k) =>
          if(a.subsetOf(k)) p+oldPtbl(k)
          else p)))

    new Factor(otherVars,newPtbl)
  }

  //Given a set of variables, it returns all possible combinations for probability
  //distribution
  //For example, if we give variables = Set(RandomVariable(A),RandomVariable(B))
  //It returns Set(Set((A,true), (B,true)), 
  //               Set((A,true), (B,false)),
  //               Set((A,false), (B,true)),
  //               Set((A,false), (B,false)))
  def allCombinations(variables: Set[RandomVariable]) = {
    def loop(variables: List[RandomVariable], result: Set[Set[(RandomVariable,String)]]): Set[Set[(RandomVariable,String)]] =
      variables match {
        case Nil => result
        case x :: rest =>
          loop(rest,result.flatMap(s => x.domain.map((v:String) => s + ((x,v)))))
      }

    loop(variables.toList,Set(Set.empty))
  } 
}

class Factor(val variables: Set[RandomVariable],
             val ptable: Map[Set[(RandomVariable,String)],Double])




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
