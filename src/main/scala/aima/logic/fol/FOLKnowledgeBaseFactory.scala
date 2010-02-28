package aima.logic.fol

object WeaponsKnowledgeKB {
  
  def apply = {
    val kb = new FOLKnowledgeBase
    kb.tell("American(x) & Weapon(y) & Sells(x,y,z) & Hostile(z) => Criminal(x)")
    kb.tell("Owns(Nono, M1)")
    kb.tell("Missile(M1)")
    kb.tell("Missile(x) & Owns(Nono,x) => Sells(West,x,Nono)")
    kb.tell("Missile(x) => Weapon(x)")
    kb.tell("Enemy(x,America) => Hostile(x)")
    kb.tell("American(West)")
    kb.tell("Enemy(Nono,America)")
    kb
  }
}
