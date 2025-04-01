package hearth
package typed

import scala.language.implicitConversions

trait Exprs { this: MacroCommons =>

  /** Platform-specific untyped type representation (`c.Expr[A]` in 2, `scala.quoted.Expr[A]` in 3) */
  type Expr[A]

  val Expr: ExprModule
  trait ExprModule { this: Expr.type =>

    def plainPrint[A](expr: Expr[A]): String = removeAnsiColors(prettyPrint(expr))
    def prettyPrint[A](expr: Expr[A]): String

    def summonImplicit[A: Type]: Option[Expr[A]]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]
  }

  implicit final class ExprMethods[A](private val expr: Expr[A]) {

    def plainPrint: String = Expr.plainPrint(expr)
    def prettyPrint: String = Expr.prettyPrint(expr)

    def asUntyped: UntypedExpr = UntypedExpr.fromTyped(expr)

    def as_??(implicit A: Type[A]): Expr_?? = Existential[Expr, A](expr)
    def as_??>:[L <: A](implicit A: Type[A]): Expr_??>:[L] = Existential.LowerBounded[L, Expr, A](expr)
    def as_??<:[U >: A](implicit A: Type[A]): Expr_??<:[U] = Existential.UpperBounded[U, Expr, A](expr)
    def as_<:??<:[L <: A, U >: A](implicit A: Type[A]): Expr_<:??<:[L, U] = Existential.Bounded[L, U, Expr, A](expr)
  }

  // Aliases to make the (very common) existential types shorter

  final type Expr_?? = Existential[Expr]
  final type Expr_??>:[L] = Existential.LowerBounded[L, Expr]
  final type Expr_??<:[U] = Existential.UpperBounded[U, Expr]
  final type Expr_<:??<:[L, U >: L] = Existential.Bounded[L, U, Expr]

  implicit def ExistentialExprMethods(expr: Expr_??): BoundedExistentialExprMethods[Nothing, Any] =
    new BoundedExistentialExprMethods[Nothing, Any](expr)
  implicit def LowerBoundedExistentialExprMethods[L](expr: Expr_??>:[L]): BoundedExistentialExprMethods[L, Any] =
    new BoundedExistentialExprMethods[L, Any](expr)
  implicit def UpperBoundedExistentialExprMethods[U](expr: Expr_??<:[U]): BoundedExistentialExprMethods[Nothing, U] =
    new BoundedExistentialExprMethods[Nothing, U](expr)
  implicit final class BoundedExistentialExprMethods[L, U >: L](private val expr: Expr_<:??<:[L, U]) {

    def plainPrint: String = Expr.plainPrint(expr.value)
    def prettyPrint: String = Expr.prettyPrint(expr.value)

    def asUntyped: UntypedExpr = UntypedExpr.fromTyped(expr.value)
  }
}
