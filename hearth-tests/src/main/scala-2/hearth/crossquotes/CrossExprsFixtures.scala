package hearth
package crossquotes

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossExprsFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with CrossExprsFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def simpleExprImpl: c.Expr[String] = simpleExpr
  def genericExprImpl[A: c.WeakTypeTag](e: Expr[A]): c.Expr[String] = genericExpr[A](e)
  def unsanitizedExprImpl: c.Expr[String] = unsanitizedExpr

  def nestedExprImpl: c.Expr[String] = nestedExpr
}

object CrossExprsFixtures {

  def simpleExpr: String = macro CrossExprsFixtures.simpleExprImpl
  def genericExpr[A](e: A): String = macro CrossExprsFixtures.genericExprImpl[A]
  def unsanitizedExpr: String = macro CrossExprsFixtures.unsanitizedExprImpl

  def nestedExpr: String = macro CrossExprsFixtures.nestedExprImpl
}
