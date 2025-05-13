package hearth
package typed

import scala.quoted.*

final class CrossQuotesFixtures(q: Quotes) extends MacroCommonsScala3(using q) with CrossQuotesFixturesImpl

object CrossQuotesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def simpleType: String = ${ simpleTypeImpl }
  def simpleTypeImpl[A: Type](using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).simpleType

  inline def genericType[A]: String = ${ genericTypeImpl[A] }
  def genericTypeImpl[A: Type](using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).genericType[A]

  inline def unsanitizedType: String = ${ unsanitizedTypeImpl }
  def unsanitizedTypeImpl(using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).unsanitizedType

  inline def simpleExpr: String = ${ simpleExprImpl }
  def simpleExprImpl[A: Type](using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).simpleExpr

  inline def genericExpr[A](inline e: A): String = ${ genericExprImpl[A]('{ e }) }
  def genericExprImpl[A: Type](e: Expr[A])(using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).genericExpr[A](e)

  inline def unsanitizedExpr: String = ${ unsanitizedExprImpl }
  def unsanitizedExprImpl(using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).unsanitizedExpr
}
