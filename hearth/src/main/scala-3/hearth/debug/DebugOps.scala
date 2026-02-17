package hearth
package debug

import scala.quoted.*

final private class DebugOpsHelper(q: Quotes) extends MacroCommonsScala3(using q), DebugMacros

/** Extension methods for debugging macros using dot notation.
  *
  * Import these to use `expr.withFinalCodeInIDE` instead of `Debug.withFinalCodeInIDE(expr)`.
  *
  * {{{
  * import hearth.debug.DebugOps.*
  *
  * someExpression.withFinalCodeInIDE
  * someExpression.withFinalASTInIDE
  * someExpression.withInferredTypeInIDE
  * }}}
  *
  * @since 0.3.0
  */
object DebugOps {

  extension [A](inline expr: A) {

    /** Passes expression unchanged, but displays its code in IDE as a hint.
      *
      * @since 0.3.0
      */
    inline def withFinalCodeInIDE: A = ${ withFinalCodeInIDEImpl[A]('expr) }

    /** Passes expression unchanged, but displays its AST in IDE as a hint.
      *
      * @since 0.3.0
      */
    inline def withFinalASTInIDE: A = ${ withFinalASTInIDEImpl[A]('expr) }

    /** Passes expression unchanged, but displays its inferred type in IDE as a hint.
      *
      * @since 0.3.0
      */
    inline def withInferredTypeInIDE: A = ${ withInferredTypeInIDEImpl[A]('expr) }
  }

  private def withFinalCodeInIDEImpl[A](expr: Expr[A])(using q: Quotes): Expr[A] =
    new DebugOpsHelper(q).withFinalCodeInIDE(expr)
  private def withFinalASTInIDEImpl[A](expr: Expr[A])(using q: Quotes): Expr[A] =
    new DebugOpsHelper(q).withFinalASTInIDE(expr)
  private def withInferredTypeInIDEImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[A] =
    new DebugOpsHelper(q).withInferredTypeInIDE(expr)
}
