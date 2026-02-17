package hearth
package debug

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class DebugOpsImpl(val c: blackbox.Context) extends MacroCommonsScala2 with DebugMacros {

  import c.universe._

  private def extractExpr[A: c.WeakTypeTag]: c.Expr[A] = {
    val tree = c.prefix.tree
    val arg = tree match {
      case Apply(_, List(arg))               => arg
      case Apply(TypeApply(_, _), List(arg)) => arg
      case other =>
        c.abort(c.enclosingPosition, s"Cannot extract expression from Debug extension prefix: ${showRaw(other)}")
    }
    c.Expr[A](arg)
  }

  def withFinalCodeInIDEImpl[A: c.WeakTypeTag]: c.Expr[A] = withFinalCodeInIDE(extractExpr[A])
  def withFinalASTInIDEImpl[A: c.WeakTypeTag]: c.Expr[A] = withFinalASTInIDE(extractExpr[A])
  def withInferredTypeInIDEImpl[A: c.WeakTypeTag]: c.Expr[A] = withInferredTypeInIDE(extractExpr[A])
}

/** Extension methods for debugging macros using dot notation.
  *
  * Import these to use `expr.withFinalCodeInIDE` instead of `Debug.withFinalCodeInIDE(expr)`.
  *
  * {{{
  * import hearth.debug.DebugOps._
  *
  * someExpression.withFinalCodeInIDE
  * someExpression.withFinalASTInIDE
  * someExpression.withInferredTypeInIDE
  * }}}
  *
  * @since 0.3.0
  */
object DebugOps {

  /** Provides debug extension methods on any expression.
    *
    * @since 0.3.0
    */
  implicit class DebugExtensions[A](val expr: A) extends AnyVal {

    /** Passes expression unchanged, but displays its code in IDE as a hint.
      *
      * @since 0.3.0
      */
    def withFinalCodeInIDE: A = macro DebugOpsImpl.withFinalCodeInIDEImpl[A]

    /** Passes expression unchanged, but displays its AST in IDE as a hint.
      *
      * @since 0.3.0
      */
    def withFinalASTInIDE: A = macro DebugOpsImpl.withFinalASTInIDEImpl[A]

    /** Passes expression unchanged, but displays its inferred type in IDE as a hint.
      *
      * @since 0.3.0
      */
    def withInferredTypeInIDE: A = macro DebugOpsImpl.withInferredTypeInIDEImpl[A]
  }
}
