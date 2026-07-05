package hearth
package typed

import scala.quoted.*

final private class AnnotatedExprFixtures(q: Quotes) extends MacroCommonsScala3(using q), AnnotatedExprFixturesImpl

object AnnotatedExprFixtures {

  inline def testNowarn[A](inline value: A): A = ${ testNowarnImpl[A]('value) }
  private def testNowarnImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[A] =
    new AnnotatedExprFixtures(q).testNowarn[A](value)

  inline def testNowarnMsg[A](inline value: A): A = ${ testNowarnMsgImpl[A]('value) }
  private def testNowarnMsgImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[A] =
    new AnnotatedExprFixtures(q).testNowarnMsg[A](value)

  inline def testSuppressWarnings[A](inline value: A): A = ${ testSuppressWarningsImpl[A]('value) }
  private def testSuppressWarningsImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[A] =
    new AnnotatedExprFixtures(q).testSuppressWarnings[A](value)

  inline def testNowarnMsgRendered[A](inline value: A): String = ${ testNowarnMsgRenderedImpl[A]('value) }
  private def testNowarnMsgRenderedImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[String] =
    new AnnotatedExprFixtures(q).testNowarnMsgRendered[A](value)
}
