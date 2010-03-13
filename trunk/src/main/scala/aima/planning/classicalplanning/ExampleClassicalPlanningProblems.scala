package aima.planning.classical


object ClassicalPlanningProblems {

  /** The "have cake and eat case too" problem,
   * described in Fig 10.7
   *
   * @author Himanshu Gupta
   */
  def haveCakeAndEatCakeToo =
    new ClassicalPlanningProblem(
      "Have(Cake)",
      "Have(Cake) & Eaten(Cake)",
      new Action(
        "Eat(Cake)",
        "Have(Cake)",
        "~Have(Cake) & Eaten(Cake)"
      ),
      new Action(
        "Bake(Cake)",
        "~Have(Cake)",
        "Have(Cake)"
      ))
}
        
