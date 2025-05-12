package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final class CrossQuotesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with CrossQuotesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def simpleExprImpl: c.Expr[String] = simpleExpr
}

object CrossQuotesFixtures {

  def simpleExpr: String = macro CrossQuotesFixtures.simpleExprImpl
}
