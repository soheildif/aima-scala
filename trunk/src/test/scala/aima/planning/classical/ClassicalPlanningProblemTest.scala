package aima.planning.classical

import org.scalatest.Suite

class ClassicalPlanningProblemTest extends Suite {

  //testing the "have cake and eat cake too" problem
  def testIt() {

    val haveCake = new Atom("Have",List("Cake"))
    val eatenCake = new Atom("Eaten",List("Cake"))
    val eatCake = new Atom("Eat",List("Cake"))
    val bakeCake = new Atom("Bake",List("Cake"))

    //actions
    val eatCakeA = new Action(eatCake,
                              Set(PositiveLiteral(haveCake)),
                              Set(NegativeLiteral(haveCake),PositiveLiteral(eatenCake)))

    val bakeCakeA = new Action(bakeCake,
                               Set(NegativeLiteral(haveCake)),
                               Set(PositiveLiteral(haveCake)))

    val p = ClassicalPlanningProblems.haveCakeAndEatCakeToo

    expect(Set(PositiveLiteral(haveCake)))(p.initState)
    expect(Set(PositiveLiteral(haveCake),
               PositiveLiteral(eatenCake))
         )(p.goals)
    expect(Set(eatCakeA,bakeCakeA))(p.actions)
  }
}
