package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossQuotesHygieneFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CrossQuotesHygieneFixturesImpl {

  def testQuoteHygieneImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testQuoteHygiene[A](value)
}

object CrossQuotesHygieneFixtures {

  def testQuoteHygiene[A](value: A): Data = macro CrossQuotesHygieneFixtures.testQuoteHygieneImpl[A]
}
