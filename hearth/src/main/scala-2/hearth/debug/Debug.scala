package hearth
package debug

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class Debug(val c: blackbox.Context) extends MacroCommonsScala2 with DebugMacros {

  // TODO: create macro annotation which would allow to do the following

  def withFinalCodeInIDEImpl[A](expr: c.Expr[A]): c.Expr[A] = withFinalCodeInIDE(expr)
  def withFinalASTInIDEImpl[A](expr: c.Expr[A]): c.Expr[A] = withFinalASTInIDE(expr)
  def withGivenCodeInIDEImpl[A: c.WeakTypeTag]: c.Expr[A] = withGivenCodeInIDE[A]
  def withGivenASTInIDEImpl[A: c.WeakTypeTag]: c.Expr[A] = withGivenASTInIDE[A]
}
object Debug {

  def withFinalCodeInIDE[A](expr: A): A = macro Debug.withFinalCodeInIDEImpl[A]
  def withFinalASTInIDE[A](expr: A): A = macro Debug.withFinalASTInIDEImpl[A]
  def withGivenCodeInIDE[A]: A = macro Debug.withGivenCodeInIDEImpl[A]
  def withGivenASTInIDE[A]: A = macro Debug.withGivenASTInIDEImpl[A]
}
