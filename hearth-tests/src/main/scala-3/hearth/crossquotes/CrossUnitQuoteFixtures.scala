package hearth
package crossquotes

import scala.quoted.*

final private class CrossUnitQuoteFixtures(q: Quotes) extends MacroCommonsScala3(using q), CrossUnitQuoteFixturesImpl

object CrossUnitQuoteFixtures {

  inline def testCrossUnitQuote(inline value: String): String = ${ testCrossUnitQuoteImpl('value) }
  private def testCrossUnitQuoteImpl(value: Expr[String])(using q: Quotes): Expr[String] =
    new CrossUnitQuoteFixtures(q).testCrossUnitQuote(value)
}
