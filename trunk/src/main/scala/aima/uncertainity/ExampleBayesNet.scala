package aima.uncertainity

/** Some example Bayes Networks
 *
 * @author Himanshu Gupta
 */
object ExampleBayesNet {

  /** Bayes Net for the Burglary network described in Fig 14.2 */
  def burglaryNetwork = {
    val burglary = RandomVariable("Burglary")
    val earthQuake = RandomVariable("EarthQuake")
    val johnCalls = RandomVariable("JohnCalls")
    val alarm = RandomVariable("Alarm")
    val maryCalls = RandomVariable("MaryCalls")

    val True = RandomVariable.True
    val False = RandomVariable.False

    new BayesNet()
    .add(burglary,Set.empty,
         Map(Set((burglary,True)) -> 0.001,
             Set((burglary,False)) -> 0.999))
    .add(earthQuake,Set.empty,
         Map(Set((earthQuake,True)) -> 0.002,
             Set((earthQuake,False)) -> 0.998))
    .add(alarm,Set(burglary,earthQuake),
         Map(Set((burglary,True),(earthQuake,True),(alarm,True)) -> 0.95,
             Set((burglary,True),(earthQuake,False),(alarm,True)) -> 0.94,
             Set((burglary,False),(earthQuake,True),(alarm,True)) -> 0.29,
             Set((burglary,False),(earthQuake,False),(alarm,True)) -> 0.001,
             Set((burglary,True),(earthQuake,True),(alarm,False)) -> 0.05,
             Set((burglary,True),(earthQuake,False),(alarm,False)) -> 0.06,
             Set((burglary,False),(earthQuake,True),(alarm,False)) -> 0.71,
             Set((burglary,False),(earthQuake,False),(alarm,False)) -> 0.999))
    .add(johnCalls,Set(alarm),
         Map(Set((alarm,True),(johnCalls,True)) -> 0.9,
             Set((alarm,False),(johnCalls,True)) -> 0.05,
             Set((alarm,True),(johnCalls,False)) -> 0.1,
             Set((alarm,False),(johnCalls,False)) -> 0.95))
    .add(maryCalls,Set(alarm),
         Map(Set((alarm,True),(maryCalls,True)) -> 0.7,
             Set((alarm,False),(maryCalls,True)) -> 0.01,
             Set((alarm,True),(maryCalls,False)) -> 0.3,
             Set((alarm,False),(maryCalls,False)) -> 0.99))
  }
}

