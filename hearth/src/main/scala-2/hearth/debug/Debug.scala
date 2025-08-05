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

/** Utilities for debugging macros.
  *
  * Allow e.g.:
  *
  *   - previewing to what code the macro was expanded:
  *     {{{
  *     Debug.withFinalCodeInIDE {
  *       someMacroExpansion(arg)
  *     }
  *     }}}
  *   - previewing the AST of the some code to help understand what AST we have to construct:
  *     {{{
  *     Debug.withFinalASTInIDE {
  *       someExampleOfCodeWeWouldLikeToConstruct
  *     }
  *     }}}
  *   - previewing the code of the implicit value of type A, to understand how exactly it was summoned:
  *     {{{
  *     Debug.withGivenCodeInIDE[TypeClass[A]]
  *     }}}
  *   - like above, but with its AST:
  *     {{{
  *     Debug.withGivenASTInIDE[TypeClass[A]]
  *     }}}
  *
  * @since 0.1.0
  */
object Debug {

  /** Passes expression unchanged, but displays its code in IDE as a hint.
    *
    * @since 0.1.0
    */
  def withFinalCodeInIDE[A](expr: A): A = macro Debug.withFinalCodeInIDEImpl[A]

  /** Passes expression unchanged, but displays its AST in IDE as a hint.
    *
    * @since 0.1.0
    */
  def withFinalASTInIDE[A](expr: A): A = macro Debug.withFinalASTInIDEImpl[A]

  /** Summons value of type A, and displays its code in IDE as a hint.
    *
    * @since 0.1.0
    */
  def withGivenCodeInIDE[A]: A = macro Debug.withGivenCodeInIDEImpl[A]

  /** Summons value of type A, and displays its AST in IDE as a hint.
    *
    * @since 0.1.0
    */
  def withGivenASTInIDE[A]: A = macro Debug.withGivenASTInIDEImpl[A]
}
