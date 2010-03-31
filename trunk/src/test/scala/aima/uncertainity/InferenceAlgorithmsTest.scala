package aima.uncertainity

import junit.framework._
import Assert._

//common enumeration ask algorithms tests
object CommonAskTest {

  def testEnumerationAskAimaExample(ask: (RandomVariable,Map[RandomVariable,String],BayesNet)=>Map[String,Double]) {
    val result = ask(RandomVariable("Burglary"),
                     Map(RandomVariable("JohnCalls") -> RandomVariable.True,
                         RandomVariable("MaryCalls") -> RandomVariable.True),
                     ExampleBayesNet.burglaryNetwork)
    assertEquals(2,result.size)
    assertEquals(0.284,result(RandomVariable.True),0.001)
    assertEquals(0.715,result(RandomVariable.False),0.001)
  }

  def testEnumerationAllVariablesExcludingQueryKnown(ask: (RandomVariable,Map[RandomVariable,String],BayesNet)=>Map[String,Double]) {
    val result = ask(RandomVariable("Alarm"),
                     Map(RandomVariable("Burglary") -> RandomVariable.False,
                         RandomVariable("EarthQuake") -> RandomVariable.False,
                         RandomVariable("JohnCalls") -> RandomVariable.True,
                         RandomVariable("MaryCalls") -> RandomVariable.True),
                     ExampleBayesNet.burglaryNetwork)

    assertEquals(2,result.size)
    assertEquals(0.557,result(RandomVariable.True),0.001)
    assertEquals(0.442,result(RandomVariable.False),0.001)
  }
}

class EnumerationAskTest extends TestCase {

  def testEnumerationAskAimaExample() {
    CommonAskTest.testEnumerationAskAimaExample(EnumerationAsk.apply)
  }
  
  def testEnumerationAllVariablesExcludingQueryKnown() {
    CommonAskTest.testEnumerationAllVariablesExcludingQueryKnown(EnumerationAsk.apply)
  }
}


class EnumerationAskWithVariableEliminationTest extends TestCase {
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
    assertEquals(Set.empty,tmp.variables)
    assertEquals(Map(Set.empty -> 1.0),tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(f0,fA)
    assertEquals(Set(A),tmp.variables)
    assertEquals(Map(Set((A,True)) -> 0.6,
                     Set((A,False)) -> 0.4),tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(fA,fA)
    assertEquals(Set(A),tmp.variables)
    assertEquals(0.36, tmp.ptable(Set((A,True))), 0.001)
    assertEquals(0.16, tmp.ptable(Set((A,False))), 0.001)


    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(fA,fB)
    assertEquals(Set(A,B),tmp.variables)
    assertEquals(0.42,tmp.ptable(Set((A,True),(B,True))),0.001)
    assertEquals(0.18,tmp.ptable(Set((A,True),(B,False))),0.001)
    assertEquals(0.28,tmp.ptable(Set((A,False),(B,True))),0.001)
    assertEquals(0.12,tmp.ptable(Set((A,False),(B,False))),0.001)


    //example given in Fig 14.10 in AIMA3e
    tmp = EnumerationAskWithVariableElimination.pointwiseProduct(fAB,fBC)
    assertEquals(Set(A,B,C),tmp.variables)
    assertEquals(0.06,tmp.ptable(Set((A,True),(B,True),(C,True))),0.001)
    assertEquals(0.24,tmp.ptable(Set((A,True),(B,True),(C,False))),0.001)
    assertEquals(0.42,tmp.ptable(Set((A,True),(B,False),(C,True))),0.001)
    assertEquals(0.28,tmp.ptable(Set((A,True),(B,False),(C,False))),0.001)
    assertEquals(0.18,tmp.ptable(Set((A,False),(B,True),(C,True))),0.001)
    assertEquals(0.72,tmp.ptable(Set((A,False),(B,True),(C,False))),0.001)
    assertEquals(0.06,tmp.ptable(Set((A,False),(B,False),(C,True))),0.001)
    assertEquals(0.04,tmp.ptable(Set((A,False),(B,False),(C,False))),0.001)
  }

  def testSumOutAFactor() {
    var tmp = EnumerationAskWithVariableElimination.sumOutAFactor(A,f0)
    assertEquals(Set.empty,tmp.variables)
    assertEquals(Map(Set.empty -> 1.0),tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.sumOutAFactor(B,fA)
    assertEquals(fA.variables,tmp.variables)
    assertEquals(fA.ptable,tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.sumOutAFactor(A,fA)
    assertEquals(Set.empty,tmp.variables)
    assertEquals(Map(Set.empty -> 1.0),tmp.ptable)


    tmp = EnumerationAskWithVariableElimination.sumOutAFactor(A,fAB)
    assertEquals(Set(B),tmp.variables)
    assertEquals(1.2,tmp.ptable(Set((B,True))),0.001)
    assertEquals(0.8,tmp.ptable(Set((B,False))),0.001)
  }

  def testEnumerationAskAimaExample() {
    CommonAskTest.testEnumerationAskAimaExample(EnumerationAskWithVariableElimination.apply)
  }
  
  def testEnumerationAllVariablesExcludingQueryKnown() {
    CommonAskTest.testEnumerationAllVariablesExcludingQueryKnown(EnumerationAskWithVariableElimination.apply)
  }
}


class PriorSampleTest extends TestCase {
  def testIt() {
    //we can't really write a unit test for it, as result will
    //be random, we're doing this just to see that no exceptions
    //are raised
    println(PriorSample(ExampleBayesNet.burglaryNetwork))
    assertTrue(true)
  }
}
