package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ExprCodecFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ExprCodecFixturesImpl {

  def testExprCodecRoundTripImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[Data] =
    testExprCodecRoundTrip[A](expr)
}

object ExprCodecFixtures {

  def testExprCodecRoundTrip[A](expr: A): Data =
    macro ExprCodecFixtures.testExprCodecRoundTripImpl[A]
}
