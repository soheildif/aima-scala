package aima.planning.classical

import org.scalatest.Suite

class ClassicalPlanningProblemTest extends Suite {

  //testing the "have cake and eat cake too" problem
  def testIt() {

    val haveCakeP = PositiveLiteral(new Atom("Have",List("Cake")))
    val haveCakeN = NegativeLiteral(new Atom("Have",List("Cake")))
    val eatenCakeP = PositiveLiteral(new Atom("Eaten",List("Cake")))
    val eatenCakeN = NegativeLiteral(new Atom("Eaten",List("Cake")))
    val eatCakeP = PositiveLiteral(new Atom("Eat",List("Cake")))
    val eatCakeN = NegativeLiteral(new Atom("Eat",List("Cake")))
    val bakeCakeP = PositiveLiteral(new Atom("Bake",List("Cake")))
    val bakeCakeN = NegativeLiteral(new Atom("Bake",List("Cake")))

    //actions
    val eatCakeA = Action("Eat(Cake)","","")
    val bakeCakeA = Action("Bake(Cake)","","")

    val p = ClassicalPlanningProblems.haveCakeAndEatCakeToo

    expect(Set(haveCakeP))(p.initState)
    expect(Set(haveCakeP,eatenCakeP))(p.goals)
    expect(Set(eatCakeA,bakeCakeA))(p.actions)
  }
}
