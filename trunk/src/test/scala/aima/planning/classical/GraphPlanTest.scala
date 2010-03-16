package aima.planning.classical

import org.scalatest.Suite

class GraphPlanTest extends Suite {


  def testHaveCakeAndEatCakeToo() {
    //the unsolvable goal -- Destroy(Cake)
    assert(GraphPlan(ClassicalPlanningProblems.haveCakeAndEatCakeToo("Destroy(Cake)")) == None)

    //solvable standard problem
    expect(List(Set(Action("Eat(Cake)","","")),
                Set(Action("Bake(Cake)","",""),
                    Action.noOp(PositiveLiteral(new Atom("Eaten",List("Cake"))))))
         )(GraphPlan(ClassicalPlanningProblems.haveCakeAndEatCakeToo).get)
  }

  def testSpareTire() {
    //the unsolvable goal -- At(Spare,Axle) & At(Flat,Axle)
    assert(GraphPlan(ClassicalPlanningProblems.spareTire("At(Spare,Axle) & At(Flat,Axle)")) == None)
    
    //solvable standard problem
    expect(List(Set(Action("Remove(Flat,Axle)","",""),
                    Action("Remove(Spare,Trunk)","",""),
                    Action.noOp(NegativeLiteral(new Atom("At",List("Spare","Axle"))))),
                Set(Action("PutOn(Spare,Axle)","","")))
         )(GraphPlan(ClassicalPlanningProblems.spareTire).get)
  }
}
