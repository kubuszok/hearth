package hearth
package debug

import scala.quoted.*

final private class Debug(q: Quotes) extends MacroCommonsScala3(using q), DebugMacros

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
  *   - previewing the inferred type of some expression:
  *     {{{
  *     Debug.withInferredTypeInIDE {
  *       someExpression
  *     }
  *     }}}
  *
  * @since 0.1.0
  */
object Debug {

  // TODO: create macro annotation which would allow to do the following

  /** Passes expression unchanged, but displays its code in IDE as a hint.
    *
    * @since 0.1.0
    */
  inline def withFinalCodeInIDE[A](inline expr: A): A = ${ Debug.withFinalCodeInIDEImpl[A]('expr) }
  private def withFinalCodeInIDEImpl[A](expr: Expr[A])(using q: Quotes): Expr[A] = new Debug(q).withFinalCodeInIDE(expr)

  /** Passes expression unchanged, but displays its AST in IDE as a hint.
    *
    * @since 0.1.0
    */
  inline def withFinalASTInIDE[A](inline expr: A): A = ${ Debug.withFinalASTInIDEImpl[A]('expr) }
  private def withFinalASTInIDEImpl[A](expr: Expr[A])(using q: Quotes): Expr[A] = new Debug(q).withFinalASTInIDE(expr)

  /** Summons value of type A, and displays its code in IDE as a hint.
    *
    * @since 0.1.0
    */
  inline def withGivenCodeInIDE[A]: A = ${ Debug.withGivenCodeInIDEImpl[A] }
  private def withGivenCodeInIDEImpl[A: Type](using q: Quotes): Expr[A] = new Debug(q).withGivenCodeInIDE[A]

  /** Summons value of type A, and displays its AST in IDE as a hint.
    *
    * @since 0.1.0
    */
  inline def withGivenASTInIDE[A]: A = ${ Debug.withGivenASTInIDEImpl[A] }
  private def withGivenASTInIDEImpl[A: Type](using q: Quotes): Expr[A] = new Debug(q).withGivenASTInIDE[A]

  /** Passes expression unchanged, but displays its inferred type in IDE as a hint.
    *
    * @since 0.3.0
    */
  inline def withInferredTypeInIDE[A](inline expr: A): A = ${ Debug.withInferredTypeInIDEImpl[A]('expr) }
  private def withInferredTypeInIDEImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[A] =
    new Debug(q).withInferredTypeInIDE(expr)

  /** Summons value of type A, and displays its inferred type in IDE as a hint.
    *
    * @since 0.3.0
    */
  inline def withGivenTypeInIDE[A]: A = ${ Debug.withGivenTypeInIDEImpl[A] }
  private def withGivenTypeInIDEImpl[A: Type](using q: Quotes): Expr[A] = new Debug(q).withGivenTypeInIDE[A]
}
