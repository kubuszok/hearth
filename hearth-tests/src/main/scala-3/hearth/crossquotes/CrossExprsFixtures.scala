package hearth
package crossquotes

import scala.quoted.*

final private class CrossExprsFixtures(q: Quotes) extends MacroCommonsScala3(using q), CrossExprsFixturesImpl

object CrossExprsFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def simpleExpr: String = ${ simpleExprImpl }
  private def simpleExprImpl[A: Type](using q: Quotes): Expr[String] = new CrossExprsFixtures(q).simpleExpr

  inline def genericExpr[A](inline e: A): String = ${ genericExprImpl[A]('{ e }) }
  private def genericExprImpl[A: Type](e: Expr[A])(using q: Quotes): Expr[String] =
    new CrossExprsFixtures(q).genericExpr[A](e)

  inline def unsanitizedExpr: String = ${ unsanitizedExprImpl }
  private def unsanitizedExprImpl(using q: Quotes): Expr[String] = new CrossExprsFixtures(q).unsanitizedExpr

  inline def nestedExpr: String = ${ nestedExprImpl }
  private def nestedExprImpl(using q: Quotes): Expr[String] = new CrossExprsFixtures(q).nestedExpr
}
