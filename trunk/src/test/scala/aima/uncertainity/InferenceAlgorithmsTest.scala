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

  def testPointwiseProduct() {
    val f1 = new Factor(Set.empty,Map(Set.empty -> 1.0)) //no variables
    
    var tmp = EnumerationAskWithVariableElimination.pointwiseProduct(f1,f1)
    expect(Set.empty)(tmp.variables)
    expect(Map(Set.empty -> 1.0))(tmp.ptable)
    
    val A = RandomVariable("A")
    val f2 = new Factor(Set(A),Map(Set((A,RandomVariable.True)) -> 0.6,
                                   Set((A,RandomVariable.False)) -> 0.4))
    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(f1,f2)
    expect(Set(A))(tmp.variables)
    expect(Map(Set((A,RandomVariable.True)) -> 0.36,
               Set((A,RandomVariable.False)) -> 0.16000000000000003))(tmp.ptable) //for whatever reason 0.4*0.4 = 0.1600..03

    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(f2,f2)
    expect(Set(A))(tmp.variables)
    expect(Map(Set((A,RandomVariable.True)) -> 0.36,
               Set((A,RandomVariable.False)) -> 0.4))(tmp.ptable)

//    val B = RandomVariable("B")
//    val f3 = new Factor(Set(B),Map(Set((B,RandomVariable.True)) -> 0.7,
//                                   Set((B,RandomVariable.False)) -> 0.3))
//    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(f2,f3)
    
  }
}
    
