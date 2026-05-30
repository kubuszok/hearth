package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ExprCodecFixtures(q: Quotes) extends MacroCommonsScala3(using q), ExprCodecFixturesImpl

object ExprCodecFixtures {

  inline def testExprCodecRoundTrip[A](inline expr: A): Data = ${
    testExprCodecRoundTripImpl[A]('expr)
  }
  private def testExprCodecRoundTripImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[Data] =
    new ExprCodecFixtures(q).testExprCodecRoundTrip[A](expr)
}
