package aima.logic.fol

import org.scalatest.Suite
import scala.collection.immutable.{Set}

/** Tests for FOL Grammar Parser
 *
 * @author Himanshu Gupta
 */
class FOLParserTest extends Suite {

  def testParser() {
    expect(new Predicate("King",Constant("John")))(FOLParser.parse("King(John)"))
  }
}
