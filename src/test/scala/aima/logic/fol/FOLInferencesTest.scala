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
    assert(FOLFCAsk(KBFactory.weaponsKB,query).size == 1)
  }

  def testWeaponsKBCriminalXReturnsWest() {
    val query = FOLParser.parse("Criminal(x)").asInstanceOf[AtomicSentence]
    val result = FOLFCAsk(KBFactory.weaponsKB,query)
    assert(result.size == 1)
    
    val expectedPair = Variable("x") -> Constant("West")
    assert(result.exists( _.exists(_ == expectedPair)))
  }

  def testKingsKBRichardEvilFalse() {
    val query = FOLParser.parse("Evil(Richard)").asInstanceOf[AtomicSentence]
    assert(FOLFCAsk(KBFactory.kingsKB,query).isEmpty)
  }

  def testKingsKBJohnEvilSuccess() {
    val query = FOLParser.parse("Evil(John)").asInstanceOf[AtomicSentence]
    assert(FOLFCAsk(KBFactory.kingsKB,query).size == 1)
  }

  def testKingsKBEvilXReturnsJohn() {
    val query = FOLParser.parse("Evil(x)").asInstanceOf[AtomicSentence]

    val result = FOLFCAsk(KBFactory.kingsKB,query)
    assert(result.size == 1)

    val expectedPair = Variable("x")->Constant("John")
    assert(result.exists(_.exists(_ == expectedPair)))
  }

  def testKingsKBKingXReturnsJohnAndRichard() {
    val query = FOLParser.parse("King(x)").asInstanceOf[AtomicSentence]

    val result = FOLFCAsk(KBFactory.kingsKB,query)
    assert(result.size == 2)

    val ep1 = Variable("x")->Constant("John")
    val ep2 = Variable("x")->Constant("Richard")

    assert(result.exists(_.exists(_ == ep1)))
    assert(result.exists(_.exists(_ == ep2)))
  }
}


/** Tests for FOL-BC-ASK
 *
 * @author Himanshu Gupta
 */
class FOLBCAskTest extends Suite {

  def testWeaponsKBCriminalWestSuccess() {
    val query = FOLParser.parse("Criminal(West)").asInstanceOf[AtomicSentence]
    assert(FOLBCAsk(KBFactory.weaponsKB,query).size == 1)
  }

  def testWeaponsKBCriminalXReturnsWest() {
    val query = FOLParser.parse("Criminal(x)").asInstanceOf[AtomicSentence]
    val result = FOLBCAsk(KBFactory.weaponsKB,query)
    assert(result.size == 1)
    
    val expectedPair = Variable("x") -> Constant("West")
    assert(result.exists( _.exists(_ == expectedPair)))
  }

  def testKingsKBRichardEvilFalse() {
    val query = FOLParser.parse("Evil(Richard)").asInstanceOf[AtomicSentence]
    assert(FOLBCAsk(KBFactory.kingsKB,query).isEmpty)
  }

  def testKingsKBJohnEvilSuccess() {
    val query = FOLParser.parse("Evil(John)").asInstanceOf[AtomicSentence]
    assert(FOLBCAsk(KBFactory.kingsKB,query).size == 1)
  }

  def testKingsKBEvilXReturnsJohn() {
    val query = FOLParser.parse("Evil(x)").asInstanceOf[AtomicSentence]

    val result = FOLBCAsk(KBFactory.kingsKB,query)
    assert(result.size == 1)

    val expectedPair = Variable("x")->Constant("John")
    assert(result.exists(_.exists(_ == expectedPair)))
  }

  def testKingsKBKingXReturnsJohnAndRichard() {
    val query = FOLParser.parse("King(x)").asInstanceOf[AtomicSentence]

    val result = FOLBCAsk(KBFactory.kingsKB,query)
    assert(result.size == 2)

    val ep1 = Variable("x")->Constant("John")
    val ep2 = Variable("x")->Constant("Richard")

    assert(result.exists(_.exists(_ == ep1)))
    assert(result.exists(_.exists(_ == ep2)))
  }
}

/** Tests for FOL Resolution
 *
 * @author Himanshu Gupta
 */
class FOLResolutionTest extends Suite {

  def testCuriosityKillsTunaSucceeds() {
    assert(FOLResolution(KBFactory.lovesAnimalKB, FOLParser.parse("Kills(Curiosity,Tuna)")))
  }
/* TODO: work on it
  def testJackKillsTunaFails() {
    assert(!FOLResolution(KBFactory.lovesAnimalKB, FOLParser.parse("Kills(Jack,Tuna)")))
  }
*/
  def testEqualityAxiomsKBabcAEqualsCSucceeds() {
    assert(FOLResolution(KBFactory.aBCEqualityKB(true),FOLParser.parse("A = C")))
  }

  def testEqualityNoAxiomsKBabcAEqualsCFails() {
    assert(!FOLResolution(KBFactory.aBCEqualityKB(false),FOLParser.parse("A = C")))
  }
}
