package aima.planning.classical


object ClassicalPlanningProblems {

  /** The "have cake and eat case too" problem,
   * described in Fig 10.7
   *
   * @author Himanshu Gupta
   */
  def haveCakeAndEatCakeToo =
    ClassicalPlanningProblem(
      "Have(Cake)",
      "Have(Cake) & Eaten(Cake)",
      Action(
        "Eat(Cake)",
        "Have(Cake)",
        "~Have(Cake) & Eaten(Cake)"
      ),
      Action(
        "Bake(Cake)",
        "~Have(Cake)",
        "Have(Cake)"
      ))
}
        
