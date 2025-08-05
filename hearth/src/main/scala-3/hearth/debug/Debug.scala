package hearth
package debug

import scala.quoted.*

final class Debug(q: Quotes) extends MacroCommonsScala3(using q), DebugMacros
object Debug {

  inline def withFinalCodeInIDE[A](inline expr: A): A = ${ Debug.withFinalCodeInIDEImpl[A]('{ expr }) }
  private def withFinalCodeInIDEImpl[A](expr: Expr[A])(using q: Quotes): Expr[A] = new Debug(q).withFinalCodeInIDE(expr)
  inline def withFinalASTInIDE[A](inline expr: A): A = ${ Debug.withFinalASTInIDEImpl[A]('{ expr }) }
  private def withFinalASTInIDEImpl[A](expr: Expr[A])(using q: Quotes): Expr[A] = new Debug(q).withFinalASTInIDE(expr)
  inline def withGivenCodeInIDE[A]: A = ${ Debug.withGivenCodeInIDEImpl[A] }
  private def withGivenCodeInIDEImpl[A: Type](using q: Quotes): Expr[A] = new Debug(q).withGivenCodeInIDE[A]
  inline def withGivenASTInIDE[A]: A = ${ Debug.withGivenASTInIDEImpl[A] }
  private def withGivenASTInIDEImpl[A: Type](using q: Quotes): Expr[A] = new Debug(q).withGivenASTInIDE[A]
}
