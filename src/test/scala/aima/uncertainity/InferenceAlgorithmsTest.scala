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
    
