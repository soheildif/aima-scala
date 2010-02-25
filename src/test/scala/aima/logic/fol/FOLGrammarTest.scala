package aima.logic.fol

import org.scalatest.Suite
import scala.collection.immutable.{Set}

/** Tests for FOL Grammar Parser
 *
 * @author Himanshu Gupta
 */
class FOLParserTest extends Suite {

  def testParseVar() {
    expect(Variable("x"))(FOLParser.parseTerm("x"))
  }
  
  def testParseConstant() {
    expect(Constant("John"))(FOLParser.parseTerm("John"))
  }

  def testParseFunction() {
    expect(new Function("Brother",Constant("John"),Constant("Paul")))(FOLParser.parseTerm("Brother(John, Paul)"))
  }

  def testParsePredicate() {
    expect(new Predicate("King",Constant("John")))(FOLParser.parse("King(John)"))
  }

  def testTermEquality() {
    expect(new Equal(new Function("BrotherOf", Constant("John")),
                     new Function("EnemyOf", Constant("Saladin"))))(FOLParser.parse("BrotherOf(John) = EnemyOf(Saladin)"))
  }

  def testTermEquality2() {
    expect(new Equal(
      new Function("BrotherOf",
                   Constant("John")),
      Variable("y")))(FOLParser.parse("BrotherOf(John) = y"))
  }

  def testNotSentence() {
    expect(new Negation(new Equal(new Function("BrotherOf", Constant("John")),
                                  new Function("EnemyOf", Constant("Saladin")))))(FOLParser.parse("~ BrotherOf(John)=     EnemyOf(Saladin )"))
  }
    
  def testSimpleParanthizedSentence() {
    expect(new Negation(new Predicate("King",Constant("John"))))(FOLParser.parse("(~ King(John))"))
  }

  def testExtraParanthizedSentence() {
    expect(new Negation(new Predicate("King",Constant("John"))))(FOLParser.parse("(((~ King(John))))"))
  }

  def testComplexParanthizedSentence() {
    expect(new Negation(new Equal(new Function("BrotherOf", Constant("John")),
                                  new Function("EnemyOf", Constant("Saladin")))))(FOLParser.parse("(~ BrotherOf(John)=     EnemyOf(Saladin ))"))
  }

  def testSimpleConnectedSentence() {
    expect(new Conjunction(new Predicate("King",Constant("John")),
                           new Negation(new Equal(new Function("Brother",Constant("Richard")),
                                                Variable("y"))))
         )(FOLParser.parse("King(John)/\\ ~Brother(Richard) =y"))
  }

  def testComplexConnectedSentence() {
    val expected = new Disjunction(new Conjunction(new Predicate("King",Constant("John")),
                                                   new Negation(new Predicate("King",Constant("Richard")))),
                                   new Predicate("King",Constant("Saladin")))
    expect(expected)(FOLParser.parse("(King(John) /\\ ~ King(Richard))  \\/King(Saladin)"))
  }

  def testComplexConnectedSentence2() {
    expect(new Conjunction(new Predicate("King",Variable("x")),
                           new Equal(new Function("BrotherOf",Variable("x")),
                                     Variable("y")))
         )(FOLParser.parse("(King(x) /\\ BrotherOf(x) = y)"))
  }


  def testQuantifierWithSingleVariable() {
    val expected = new UniversalQuantifier(Variable("x"),new Predicate("King",Variable("x")))
    expect(expected)(FOLParser.parse("4L x King(x)"))
  }


  def testQuantifierWithTwoVariables() {
    val expected = new ExistentialQuantifier(Variable("x"),
                                             new ExistentialQuantifier(Variable("y"),
                                                                       new Conjunction(new Predicate("King",Variable("x")),
                                                                                       new Equal(new Function("BrotherOf",Variable("x")),
                                                                                                 Variable("y")))))
    expect(expected)(FOLParser.parse("3E x,y (King(x) /\\ BrotherOf(x) = y)"))
  }

  def testQuantifierSentenceWithPathologicalParanthesizing() {
    val expected = new UniversalQuantifier(Variable("x"),
                                             new UniversalQuantifier(Variable("y"),
                                                                       new Conjunction(new Predicate("King",Variable("x")),
                                                                                       new Equal(new Function("BrotherOf",Variable("x")),
                                                                                                 Variable("y")))))
    expect(expected)(FOLParser.parse("( ( ((4L x,y (King(x) /\\ (BrotherOf(x) = y  ))) )   )    )"))
  }

  def testNestedQuantifier() {
    val expected = new UniversalQuantifier(Variable("x"),
                                             new ExistentialQuantifier(Variable("y"),
                                                                       new Conjunction(new Predicate("King",Variable("x")),
                                                                                       new Equal(new Function("BrotherOf",Variable("x")),
                                                                                                 Variable("y")))))
    expect(expected)(FOLParser.parse("4L x (3E y (King(x) /\\ BrotherOf(x) = y))"))
  }

  def testNestedQuantifier2() {
    val expected = new ExistentialQuantifier(Variable("x"),
                                             new UniversalQuantifier(Variable("y"),
                                                                       new Conjunction(new Predicate("King",Variable("x")),
                                                                                       new Equal(new Function("BrotherOf",Variable("x")),
                                                                                                 Variable("y")))))
    expect(expected)(FOLParser.parse("3E x (4L y (King(x) /\\ BrotherOf(x) = y))"))
  }

  def testImplication() {
    val expected = new Conditional(new Conjunction(new Predicate("Missile",Variable("m")),
                                                   new Predicate("Owns",Variable("m"))),
                                   new Predicate("Sells",Constant("West"),Variable("m"),Constant("Nono")))
    expect(expected)(FOLParser.parse("(Missile(m) /\\ Owns(m)) => Sells(West,m,Nono)"))
  }
}
