package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class AnnotatedExprFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnnotatedExprFixturesImpl {

  def testNowarnImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[A] = testNowarn[A](value)
  def testNowarnMsgImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[A] = testNowarnMsg[A](value)
  def testSuppressWarningsImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[A] = testSuppressWarnings[A](value)
  def testNowarnMsgRenderedImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = testNowarnMsgRendered[A](value)
}

object AnnotatedExprFixtures {

  def testNowarn[A](value: A): A = macro AnnotatedExprFixtures.testNowarnImpl[A]
  def testNowarnMsg[A](value: A): A = macro AnnotatedExprFixtures.testNowarnMsgImpl[A]
  def testSuppressWarnings[A](value: A): A = macro AnnotatedExprFixtures.testSuppressWarningsImpl[A]
  def testNowarnMsgRendered[A](value: A): String = macro AnnotatedExprFixtures.testNowarnMsgRenderedImpl[A]
}
