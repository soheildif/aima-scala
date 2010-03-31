package aima.uncertainity

/** Some example Bayes Networks
 *
 * @author Himanshu Gupta
 */
object ExampleBayesNet {


  val True = RandomVariable.True
  val False = RandomVariable.False


  /** Bayes Net for the Burglary network described in Fig 14.2 */
  def burglaryNetwork = {
    val burglary = RandomVariable("Burglary")
    val earthQuake = RandomVariable("EarthQuake")
    val johnCalls = RandomVariable("JohnCalls")
    val alarm = RandomVariable("Alarm")
    val maryCalls = RandomVariable("MaryCalls")

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

  /** Bayes Net for the Cloudy network described in Fig 12.12 */
  def cloudyNetwork = {
    val cloudy = RandomVariable("Cloudy")
    val sprinkler = RandomVariable("Sprinkler")
    val rain = RandomVariable("Rain")
    val wetGrass = RandomVariable("WetGrass")

    new BayesNet()
    .add(cloudy,Set.empty,
         Map(Set((cloudy,True)) -> 0.5,
             Set((cloudy,False)) -> 0.5))
    .add(sprinkler,Set(cloudy),
         Map(Set((cloudy,True),(sprinkler,True)) -> 0.1,
             Set((cloudy,False),(sprinkler,True)) -> 0.5,
             Set((cloudy,True),(sprinkler,False)) -> 0.9,
             Set((cloudy,False),(sprinkler,False)) -> 0.5))
    .add(rain,Set(cloudy),
         Map(Set((cloudy,True),(rain,True)) -> 0.8,
             Set((cloudy,False),(rain,True)) -> 0.2,
             Set((cloudy,True),(rain,False)) -> 0.2,
             Set((cloudy,False),(rain,False)) -> 0.8))
    .add(wetGrass,Set(sprinkler,rain),
         Map(Set((sprinkler,True),(rain,True),(wetGrass,True)) -> 0.99,
             Set((sprinkler,True),(rain,False),(wetGrass,True)) -> 0.90,
             Set((sprinkler,False),(rain,True),(wetGrass,True)) -> 0.90,
             Set((sprinkler,False),(rain,False),(wetGrass,True)) -> 0.00,
             Set((sprinkler,True),(rain,True),(wetGrass,False)) -> 0.01,
             Set((sprinkler,True),(rain,False),(wetGrass,False)) -> 0.10,
             Set((sprinkler,False),(rain,True),(wetGrass,False)) -> 0.10,
             Set((sprinkler,False),(rain,False),(wetGrass,False)) -> 1.0))
  }          
}
