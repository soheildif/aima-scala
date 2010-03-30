package aima.uncertainity

import org.scalatest.Suite

class EnumerationAskTest extends Suite {

  def testEnumerationAskAimaExample() {
    val result = EnumerationAsk(RandomVariable("Burglary"),
                                Map(RandomVariable("JohnCalls") -> RandomVariable.True,
                                    RandomVariable("MaryCalls") -> RandomVariable.True),
                                ExampleBayesNet.burglaryNetwork)
    assert(result.size == 2)
    assert(0.284 < result(RandomVariable.True) && result(RandomVariable.True) < 0.285)
    assert(0.715 < result(RandomVariable.False) && result(RandomVariable.False) < 0.716)
  }

  def testEnumerationAllVariablesExcludingQueryKnown() {
    val result = EnumerationAsk(RandomVariable("Alarm"),
                                Map(RandomVariable("Burglary") -> RandomVariable.False,
                                    RandomVariable("EarthQuake") -> RandomVariable.False,
                                    RandomVariable("JohnCalls") -> RandomVariable.True,
                                    RandomVariable("MaryCalls") -> RandomVariable.True),
                                ExampleBayesNet.burglaryNetwork)

    assert(result.size == 2)
    assert(0.557 < result(RandomVariable.True) && result(RandomVariable.True) < 0.558)
    assert(0.442 < result(RandomVariable.False) && result(RandomVariable.False) < 0.443)
  }
}


class EnumerationAskWithVariableEliminationTest extends Suite {
  private val True = RandomVariable.True
  private val False = RandomVariable.False

  private val A = RandomVariable("A")
  private val B = RandomVariable("B")
  private val C = RandomVariable("C")

  private val f0 = new Factor(Set.empty,Map(Set.empty -> 1.0)) //no variables  
  private val fA = new Factor(Set(A),Map(Set((A,True)) -> 0.6,
                                         Set((A,False)) -> 0.4))
  private val fB = new Factor(Set(B),Map(Set((B,True)) -> 0.7,
                                         Set((B,False)) -> 0.3))
  private val fAB = new Factor(Set(A,B),Map(Set((A,True),(B,True)) -> 0.3,
                                           Set((A,True),(B,False)) -> 0.7,
                                           Set((A,False),(B,True)) -> 0.9,
                                           Set((A,False),(B,False)) -> 0.1))
  private val fBC = new Factor(Set(B,C),Map(Set((B,True),(C,True)) -> 0.2,
                                           Set((B,True),(C,False)) -> 0.8,
                                           Set((B,False),(C,True)) -> 0.6,
                                           Set((B,False),(C,False)) -> 0.4))
  
  def testPointwiseProduct() {
    
    var tmp = EnumerationAskWithVariableElimination.pointwiseProduct(f0,f0)
    expect(Set.empty)(tmp.variables)
    expect(Map(Set.empty -> 1.0))(tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(f0,fA)
    expect(Set(A))(tmp.variables)
    expect(Map(Set((A,True)) -> 0.6,
               Set((A,False)) -> 0.4))(tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(fA,fA)
    expect(Set(A))(tmp.variables)
    expect(Map(Set((A,RandomVariable.True)) -> 0.36,
               Set((A,RandomVariable.False)) -> 0.16000000000000003))(tmp.ptable) //should be 0.16


    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(fA,fB)
    expect(Set(A,B))(tmp.variables)
    expect(Map(Set((A,True),(B,True)) -> 0.42,
               Set((A,True),(B,False)) -> 0.18,
               Set((A,False),(B,True)) ->  0.27999999999999997, //it should be 0.28 but 0.7*0.4 is not 0.28 for whatever reason
               Set((A,False),(B,False)) -> 0.12))(tmp.ptable)


    //example given in Fig 14.10 in AIMA3e
    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(fAB,fBC)
    expect(Set(A,B,C))(tmp.variables)
    expect(Map(Set((A,True),(B,True),(C,True)) -> 0.06,
               Set((A,True),(B,True),(C,False)) -> 0.24,
               Set((A,True),(B,False),(C,True)) -> 0.42,
               Set((A,True),(B,False),(C,False)) -> 0.27999999999999997, //should be 0.28
               Set((A,False),(B,True),(C,True)) -> 0.18000000000000002, //should be 0.18
               Set((A,False),(B,True),(C,False)) -> 0.7200000000000001, //should be 0.72
               Set((A,False),(B,False),(C,True)) -> 0.06,
               Set((A,False),(B,False),(C,False)) -> 0.04000000000000001))(tmp.ptable) //should be 0.04
  }

  def testSumOutAFactor() {
    var tmp = EnumerationAskWithVariableElimination.sumOutAFactor(A,f0)
    expect(Set.empty)(tmp.variables)
    expect(Map(Set.empty -> 1.0))(tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.sumOutAFactor(B,fA)
    expect(fA.variables)(tmp.variables)
    expect(fA.ptable)(tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.sumOutAFactor(A,fA)
    expect(Set.empty)(tmp.variables)
    expect(Map(Set.empty -> 1.0))(tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.sumOutAFactor(A,fAB)
    expect(Set(B))(tmp.variables)
                                                        //should be 0.7+0.1 is not 0.8 for whatever reason
    expect(Map(Set((B,True)) -> 1.2, Set((B,False)) -> 0.7999999999999999))(tmp.ptable)
  }
}
    
