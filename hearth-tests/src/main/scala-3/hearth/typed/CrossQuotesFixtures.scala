package hearth
package typed

import scala.quoted.*

final class CrossQuotesFixtures(q: Quotes) extends MacroCommonsScala3(using q) with CrossQuotesFixturesImpl

object CrossQuotesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def simpleExpr: String = ${ simpleExprImpl }
  def simpleExprImpl[A: Type](using q: Quotes): Expr[String] = new CrossQuotesFixtures(q).simpleExpr
}
