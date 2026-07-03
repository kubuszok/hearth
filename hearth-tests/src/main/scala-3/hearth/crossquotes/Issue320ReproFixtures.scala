package hearth
package crossquotes

import scala.quoted.*

final private class Issue320ReproFixtures(q: Quotes) extends MacroCommonsScala3(using q), Issue320ReproFixturesImpl

object Issue320ReproFixtures {

  inline def testCrossUnitQuote(inline value: String): String = ${ testCrossUnitQuoteImpl('value) }
  private def testCrossUnitQuoteImpl(value: Expr[String])(using q: Quotes): Expr[String] =
    new Issue320ReproFixtures(q).testCrossUnitQuote(value)
}
