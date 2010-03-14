package aima.planning.classical

import org.scalatest.Suite

class PlanningGraphTest extends Suite {

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
    
    //Planning Graph
    var pGraph = new PlanningGraph(ClassicalPlanningProblems.haveCakeAndEatCakeToo)

    val s0 = pGraph.stateLevel(0)
    expect(Set(PositiveLiteral(haveCake),
               NegativeLiteral(eatenCake))
         )(s0.items)
    expect(Set.empty)(s0.mutexes)

    pGraph = pGraph.expandGraph //A0 and S1 generated
    val a0 = pGraph.actionLevel(0)
    expect(Set(eatCakeA,
               Action.noOp(PositiveLiteral(haveCake)),
               Action.noOp(NegativeLiteral(eatenCake)))
         )(a0.items)
    assert(matchUnorderedPairs(a0.mutexes,
                               Set((Action.noOp(PositiveLiteral(haveCake)),eatCakeA),
                                   (Action.noOp(NegativeLiteral(eatenCake)),eatCakeA))))

    val s1 = pGraph.stateLevel(1)
    expect(Set(PositiveLiteral(haveCake),
               NegativeLiteral(haveCake),
               PositiveLiteral(eatenCake),
               NegativeLiteral(eatenCake))
         )(s1.items)

    assert(matchUnorderedPairs(s1.mutexes,
                               Set((NegativeLiteral(eatenCake).asInstanceOf[Literal],PositiveLiteral(eatenCake).asInstanceOf[Literal]),
                                   (NegativeLiteral(haveCake).asInstanceOf[Literal],PositiveLiteral(haveCake).asInstanceOf[Literal]),
                                   (PositiveLiteral(haveCake).asInstanceOf[Literal],PositiveLiteral(eatenCake).asInstanceOf[Literal]),
                                   (NegativeLiteral(haveCake).asInstanceOf[Literal],NegativeLiteral(eatenCake).asInstanceOf[Literal]))))

    pGraph = pGraph.expandGraph //A1 and S2 generated
    val a1 = pGraph.actionLevel(1)
    expect(Set(eatCakeA,bakeCakeA
               Action.noOp(PositiveLiteral(haveCake)),
               Action.noOp(NegativeLiteral(haveCake)),
               Action.noOp(PositiveLiteral(eatenCake)),
               Action.noOp(NegativeLiteral(eatenCake)))
         )(a1.items)
    assert(matchUnorderedPairs(a1.mutexes,
                               Set((Action.noOp(PositiveLiteral(haveCake)),eatCakeA),
                                   (Action.noOp(NegativeLiteral(eatenCake)),eatCakeA))))
    /*
    assert(matchUnorderedPairs(s2.mutexes,
                               Set((NegativeLiteral(eatenCake).asInstanceOf[Literal],PositiveLiteral(eatenCake).asInstanceOf[Literal]),
                                   (NegativeLiteral(haveCake).asInstanceOf[Literal],PositiveLiteral(haveCake).asInstanceOf[Literal]),
                                   (NegativeLiteral(haveCake).asInstanceOf[Literal],NegativeLiteral(eatenCake).asInstanceOf[Literal]))))*/
  }

  private def matchUnorderedPairs[A](p1: Set[(A,A)],p2: Set[(A,A)]): Boolean = {
    println("expected are: " + p2)
    println("actuals are: " + p1)
    p1.forall(
      _ match {
        case (p1x,p1y) =>
          p2.exists(_ match {
            case (p2x,p2y) =>
              ((p1x == p2x) && (p1y == p2y)) ||
              ((p1x == p2y) && (p1y == p2x))
          })
      }) && (p1.size == p2.size)
  }

}
