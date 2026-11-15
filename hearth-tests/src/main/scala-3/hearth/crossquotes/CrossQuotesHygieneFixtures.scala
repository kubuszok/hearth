package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class CrossQuotesHygieneFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      CrossQuotesHygieneFixturesImpl

object CrossQuotesHygieneFixtures {

  inline def testQuoteHygiene[A](inline value: A): Data = ${ testQuoteHygieneImpl[A]('value) }
  private def testQuoteHygieneImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new CrossQuotesHygieneFixtures(q).testQuoteHygiene[A](value)
}
