package hearth
package typed

import scala.quoted.*

final private class CrossQuotesFixtures(q: Quotes) extends MacroCommonsScala3(using q), CrossQuotesFixturesImpl

object CrossQuotesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def simpleExpr: String = ${ simpleExprImpl }
  private def simpleExprImpl[A: Type](using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).simpleExpr

  inline def genericExpr[A](inline e: A): String = ${ genericExprImpl[A]('{ e }) }
  private def genericExprImpl[A: Type](e: Expr[A])(using q: Quotes): Expr[String] =
    new CrossQuotesFixtures(q).genericExpr[A](e)

  inline def unsanitizedExpr: String = ${ unsanitizedExprImpl }
  private def unsanitizedExprImpl(using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).unsanitizedExpr

  inline def nestedExpr: String = ${ nestedExprImpl }
  private def nestedExprImpl(using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).nestedExpr
}
