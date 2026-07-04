package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class AnnotatedExprFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotatedExprFixturesImpl {

  def testAnnotatedRoundtripImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[A] = testAnnotatedRoundtrip[A](value)
  def testAnnotatedRenderedImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = testAnnotatedRendered[A](value)
}

object AnnotatedExprFixtures {

  def testAnnotatedRoundtrip[A](value: A): A = macro AnnotatedExprFixtures.testAnnotatedRoundtripImpl[A]
  def testAnnotatedRendered[A](value: A): String = macro AnnotatedExprFixtures.testAnnotatedRenderedImpl[A]
}
