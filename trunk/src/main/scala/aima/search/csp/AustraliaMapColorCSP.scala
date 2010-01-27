package aima.search

/** Factory for Australia Map color CSP
 * described in Fig 6.1
 *
 * @author Himanshu Gupta
 */
object AustraliaMapColorCSP {
  val Red = 0
  val Green = 1
  val Blue = 2

  private val domain = List(Red,Green,Blue)

  val Wa = "WA"
  val Nt = "NT"
  val Q = "Q"
  val Sa = "SA"
  val Nsw = "NSW"
  val V = "V"
  val T = "T"

  def csp = {
    
    val prob = new CSP[String,Int]()

    prob.addVariables((Wa,domain),
                      (Nt,domain),
                      (Q,domain),
                      (Sa,domain),
                      (Nsw,domain),
                      (V,domain),
                      (T,domain))                      
    prob.addConstraints(new MapColorConstraint(Wa,Nt),
                       new MapColorConstraint(Nt, Sa),
                       new MapColorConstraint(Wa, Sa),
                       new MapColorConstraint(Nt, Q),
                       new MapColorConstraint(Q, Sa),
                       new MapColorConstraint(Nsw, Sa),
                       new MapColorConstraint(Q, Nsw),
                       new MapColorConstraint(V, Sa),
                       new MapColorConstraint(V, Nsw))
    prob }
}
