package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final class CrossQuotesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with CrossQuotesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def simpleTypeImpl: c.Expr[String] = simpleType
  def genericTypeImpl[A: c.WeakTypeTag]: c.Expr[String] = genericType[A]

  def simpleExprImpl: c.Expr[String] = simpleExpr
  def genericExprImpl[A: c.WeakTypeTag](e: Expr[A]): c.Expr[String] = genericExpr[A](e)
}

object CrossQuotesFixtures {

  def simpleType: String = macro CrossQuotesFixtures.simpleTypeImpl
  def genericType[A]: String = macro CrossQuotesFixtures.genericTypeImpl[A]

  def simpleExpr: String = macro CrossQuotesFixtures.simpleExprImpl
  def genericExpr[A](e: A): String = macro CrossQuotesFixtures.genericExprImpl[A]
}
