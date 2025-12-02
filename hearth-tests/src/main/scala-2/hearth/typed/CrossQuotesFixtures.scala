package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossQuotesFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CrossQuotesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def simpleExprImpl: c.Expr[String] = simpleExpr
  def genericExprImpl[A: c.WeakTypeTag](e: Expr[A]): c.Expr[String] = genericExpr[A](e)
  def unsanitizedExprImpl: c.Expr[String] = unsanitizedExpr

  def nestedExprImpl: c.Expr[String] = nestedExpr
}

object CrossQuotesFixtures {

  def simpleExpr: String = macro CrossQuotesFixtures.simpleExprImpl
  def genericExpr[A](e: A): String = macro CrossQuotesFixtures.genericExprImpl[A]
  def unsanitizedExpr: String = macro CrossQuotesFixtures.unsanitizedExprImpl

  def nestedExpr: String = macro CrossQuotesFixtures.nestedExprImpl
}
