package aima.logic.propositional

import aima.commons.Utils
import scala.collection.immutable.{ListMap,HashMap,Set,ListSet}

/** TT-ENTAILS, described in Fig 7.10
 *
 * @author Himanshu Gupta
 */
object TTEntails {
  def apply(KB: Sentence,alpha: Sentence): Boolean = {
    val symbols = KB.symbols ++ alpha.symbols
    ttCheckAll(KB,alpha,symbols.toList,Map[PropositionSymbol,Boolean]())
  }

  private def ttCheckAll(KB: Sentence,alpha: Sentence,
                         symbols: List[PropositionSymbol],model: Map[PropositionSymbol,Boolean]): Boolean = {
    symbols match {
      case Nil =>
        KB.isTrue(model) match {
          case Some(true) =>
            alpha.isTrue(model) match {
              case Some(x) => x
              case None => throw new IllegalStateException("Model " + model + " does not contain all symbols.")
            }
          case Some(false) => true //when KB is false, always return true
          case None => 
            throw new IllegalStateException("Model " + model + " does not contain all symbols.")
        }
      case first :: rest =>
        (ttCheckAll(KB,alpha,rest,model + (first -> true)) 
         && 
         ttCheckAll(KB,alpha,rest,model + (first -> false)))
    }
  }
}

/** PL-RESOLUTION, described in Fig 7.12
 *
 * @author Himanshu Gupta
 */
/*object PLResolution {
  def apply(KB,alpha: Sentence): Boolean = {
    val clauses = convertToCNF(KB).clauses //TODO, add Not alpha also

    def loop(clauses: Set[Clause]) =
      loopIn(Utils.pairs(clauses.toList),ListSet.empty) match {
        case None => true //Empty clause found, return true
        case Some(new) =>
          if (new.subsetOf(clauses)) false
          else loop(new ++ clauses)
      }

    def loopIn(pairs: List[(Clause,Clause)], new: Set[Clause]): Option[Set[Clause]] =
      pairs match {
        case (c1,c2) :: rest =>
          val resolvents = plResolve(c1,c2)
        if(resolvents.exists(_.isEmpty)) None //Empty clause found
        else loopIn(rest,new ++ resolvents)
        case Nil => Some(new)
      }

    loop(clauses)
  }

  private def plResolve(c1: Clause, c2: Clause): Set[Clause] = {
    
    def loop(ls: List[Literal], result: Set[Clause]): Set[Clause] =
      ls match {
        case l:PositiveLiteral :: rest =>
          if(c2.literals.exists(_ == NegativeLiteral(l.s)))
            loop(rest,result + new Clause( (c1.literals - l) ++ (c2.literals - NegativeLiteral(l.s))))
          else
            loop(rest,result)
        case L;NegativeLiteral :: rest =>
          if(c2.literals.exists(_ == PositiveLiteral(l.s)))
            loop(rest,result + new Clause( (c1.literals - l) ++ (c2.literals - PositiveLiteral(l.s))))
          else
            loop(rest,result)
        case Nil => result
      }

    val resolvents = loop(c1.literals.toList,ListSet.empty)

    //check if a list of literals contain Positive as well as Negative literal
    //for the same symbol
    def isDiscardable(ls: List[Literal]) =
      Utils.pairs.exists( pair =>
                            pair match {
                              case (PositiveLiteral(x),NegativeLiteral(x)) => true
                              case (NegativeLiteral(x),PositiveLiteral(x)) => true
                              case _ => false })
    //discard all such resolvents and return the rest
    resolvents.filter(!isDiscardable(_.literals))    
  }
}*/

/** PL-FC-ENTAILS?, described in Fig 7.15
 *
 * @author Himanshu Gupta
 */
/*object PLFCEntails {

  import scala.collection.mutable.{Map,Queue}

  def apply(KB: Set[DefiniteClause],q: Symbol, knownTrueSymbols: List[Symbol]): Sentence = {
    
    val count = Map(KB.map(c => (c,c.premise.size)))
    val inferred = Map(KB.flatMap(_.premise).map((_,false)))
    val agenda = new Queue[Symbol]()
    agenda ++ knownTrueSymbols

    def loop: Boolean = {
      if(agenda.isEmpty) false
      else {
        val p = agenda.dequeue
        if(p == q) true
        else {
          if(!inferred(p)) {
            inferred += (p -> true)
            KB.foreach(c =>
              if(c.premise.contains(p))
                count += (c -> count(c)-1)
              if (count(c) == 0) agenda.enqueue(c.conclusion))
          }
          loop
        }
      }
    }

    loop
  }
}*/

/** DPLL-SATISFIABLE?, described in Fig 7.17
 *
 * @author Himanshu Gupta
 */
/*object DPLLSatisfiable {
  def apply(s: Sentence): Boolean = DPLL(convertToCNF(s).clauses,s.symbols,Map[Symbol,Boolean]())

  private def DPLL(clauses: Set[Clause],symbols: Set[Symbol],
                   model: Map[Symbol,Boolean]): Boolean = {
    if (clauses.forall(_.isTrue(model) == Some(true))) return true
    if(clauses.exists(_.isTrue(model) == Some(false))) return false

    FindPureSymbol(symbols,clauses,model) match {
      case Some((p,value)) => DPLL(clauses, symbols - p, model + (p -> value))
      case None =>
        FindUnitClause(clauses,model) match {
          case Some((p,value)) => DPLL(clauses,symbols - p, model + (p -> value))
          case None =>
            //TODO
            //p = first, rest = rest
            DPLL(clauses,rest,model + (p -> true)) || DPLL(clauses,rest,model + (p -> false))
        }
    }
  }

  private def FindPureSymbol(symbols, clauses: Set[Clause], model: Map[Symbol,Boolean]): Option[Symbol] = {

    //returns true, if given symbol appears as a Pure PositiveLiteral in given set of clauses
    def isPurePositiveLiteral(p: Symbol, clauses: Set[Clause], model: Map[Symbol,Boolean]) =
      clauses.forall(c =>
        (c.literals.contains(PositiveLiteral(p)),c.literals.contains(NegativeLiteral(p))) match {
          case (_,false) => true
          case (_,true) => c.isTrue(model) == Some(true)
        })

    //Returns true, if given symbol appears as a Pure NegativeLiteral in given set of clauses
    def isPureNegativeLiteral(p: Symbol, clauses: Set[Clause], model: Map[Symbol,Boolean]) =
      clauses.forall(c =>
        (c.literals.contains(PositiveLiteral(p)),c.literals.contains(NegativeLiteral(p))) match {
          case (false,_) => true
          case (true,_) => c.isTrue(model) == Some(true)
        })

    symbols.find(isPurePositiveLiteral(_)) match {
      case Some(p) => Some((p,true))
      case None =>
        symbols.find(isPureNegativeLiteral(_)) match {
          case Some(q) => Some((q,false))
          case None => None
        }
    }
  }

  private def FindUnitClause(clauses: Set[Clause], model: Map[Symbol,Boolean]): Option[(Symbol,Boolean)] = {
    clauses.find( _.literals.filter(!(_.isTrue(model) == Some(true))).size == 1 ) match {
      case None => None
      case Some(c) =>
        c.literals.find... //TODO: basic is done.
    }
  }
}*/

/** WALKSAT, described in Fig 7.18
 *
 * 0.0 <= probability <= 1.0
 * 
 * @author Himanshu Gupta
 */
/*object WalkSat {

  import scala.collection.immutable.Map

  def apply(clauses: Set[Clause], probability: Double, maxFlips: Int): Option[Map[Symbol,Boolean]] = {

    val random = new scala.util.Random(java.util.Random)()
    val randomModel = Map[Symbol,Boolean](clauses.symbols.map((_,random.nextBoolean)))
    
    def loop(counter: Int, model: Map[Symbol,Boolean]): Option = {
      if (counter < maxFlips) {
        //find clauses that fail in the model
        val failedClauses = clauses.filter(_.isTrue(model) match {
                                            case Some(true) => false
                                            case Some(false) => true
                                            case None =>
                                              throw new IllegalStateException("Model should have all symbols defined.")})
        //doable with current specs
    
    



    



*/
