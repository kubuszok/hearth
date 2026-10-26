package hearth
package crossquotes

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossUnitQuoteFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CrossUnitQuoteFixturesImpl {

  def testCrossUnitQuoteImpl(value: c.Expr[String]): c.Expr[String] = testCrossUnitQuote(value)
}

object CrossUnitQuoteFixtures {

  def testCrossUnitQuote(value: String): String = macro CrossUnitQuoteFixtures.testCrossUnitQuoteImpl
}
