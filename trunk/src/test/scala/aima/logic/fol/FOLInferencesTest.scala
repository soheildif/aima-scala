package aima.logic.fol

import org.scalatest.Suite
import scala.collection.immutable.{Set}

/** Tests for FOL-FC-ASK
 *
 * @author Himanshu Gupta
 */
class FOLFCAskTest extends Suite {

  def testWeaponsKBCriminalWestSuccess() {
    val query = FOLParser.parse("Criminal(West)").asInstanceOf[AtomicSentence]
    expect(Some(Set(Map())))(FOLFCAsk(KBFactory.weaponsKB,query))
  }

  def testWeaponsKBCriminalXReturnsWest() {
    val query = FOLParser.parse("Criminal(x)").asInstanceOf[AtomicSentence]

    expect(Some(Set(Map(Variable("x") -> Constant("West")))))(FOLFCAsk(KBFactory.weaponsKB,query))
  }

  def testKingsKBRichardEvilFalse() {
    val query = FOLParser.parse("Evil(Richard)").asInstanceOf[AtomicSentence]
    expect(None)(FOLFCAsk(KBFactory.kingsKB,query))
  }

  def testKingsKBJohnEvilSuccess() {
    val query = FOLParser.parse("Evil(John)").asInstanceOf[AtomicSentence]
    expect(Some(Set(Map())))(FOLFCAsk(KBFactory.kingsKB,query))
  }

  def testKingsKBEvilXReturnsJohn() {
    val query = FOLParser.parse("Evil(x)").asInstanceOf[AtomicSentence]
    expect(
      Some(Set(
        Map(Variable("x")->Constant("John"))))
         )(FOLFCAsk(KBFactory.kingsKB,query))
  }

  def testKingsKBKingXReturnsJohnAndRichard() {
    val query = FOLParser.parse("King(x)").asInstanceOf[AtomicSentence]
    expect(
      Some(Set(
        Map(Variable("x")->Constant("John")),
        Map(Variable("x")->Constant("Richard"))))
         )(FOLFCAsk(KBFactory.kingsKB,query))
  }
}

/** Tests for FOL-BC-ASK
 *
 * @author Himanshu Gupta
 */
class FOLBCAskTest extends Suite {

  def testWeaponsKBCriminalWestSuccess() {
    val query = FOLParser.parse("Criminal(West)").asInstanceOf[AtomicSentence]
    expect(Set(Map()))(FOLBCAsk(KBFactory.weaponsKB,query))
  }

  def testWeaponsKBCriminalXReturnsWest() {
    val query = FOLParser.parse("Criminal(x)").asInstanceOf[AtomicSentence]

    expect(Set(Map(Variable("x") -> Constant("West"))))(FOLBCAsk(KBFactory.weaponsKB,query))
  }

  def testKingsKBRichardEvilFalse() {
    val query = FOLParser.parse("Evil(Richard)").asInstanceOf[AtomicSentence]
    expect(Set())(FOLBCAsk(KBFactory.kingsKB,query))
  }

  def testKingsKBJohnEvilSuccess() {
    val query = FOLParser.parse("Evil(John)").asInstanceOf[AtomicSentence]
    expect(Set(Map()))(FOLBCAsk(KBFactory.kingsKB,query))
  }

  def testKingsKBEvilXReturnsJohn() {
    val query = FOLParser.parse("Evil(x)").asInstanceOf[AtomicSentence]
    expect(
      Set(
        Map(Variable("x")->Constant("John")))
         )(FOLBCAsk(KBFactory.kingsKB,query))
  }

  def testKingsKBKingXReturnsJohnAndRichard() {
    val query = FOLParser.parse("King(x)").asInstanceOf[AtomicSentence]
    expect(
      Set(
        Map(Variable("x")->Constant("John")),
        Map(Variable("x")->Constant("Richard")))
         )(FOLBCAsk(KBFactory.kingsKB,query))
  }
}
