package hearth
package typed

import scala.quoted.*

final private class AnnotatedExprFixtures(q: Quotes) extends MacroCommonsScala3(using q), AnnotatedExprFixturesImpl

object AnnotatedExprFixtures {

  inline def testAnnotatedRoundtrip[A](inline value: A): A = ${ testAnnotatedRoundtripImpl[A]('value) }
  private def testAnnotatedRoundtripImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[A] =
    new AnnotatedExprFixtures(q).testAnnotatedRoundtrip[A](value)

  inline def testAnnotatedRendered[A](inline value: A): String = ${ testAnnotatedRenderedImpl[A]('value) }
  private def testAnnotatedRenderedImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[String] =
    new AnnotatedExprFixtures(q).testAnnotatedRendered[A](value)
}
