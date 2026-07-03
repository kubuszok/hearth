package hearth
package crossquotes

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class Issue320ReproFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with Issue320ReproFixturesImpl {

  def testCrossUnitQuoteImpl(value: c.Expr[String]): c.Expr[String] = testCrossUnitQuote(value)
}

object Issue320ReproFixtures {

  def testCrossUnitQuote(value: String): String = macro Issue320ReproFixtures.testCrossUnitQuoteImpl
}
