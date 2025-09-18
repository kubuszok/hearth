package hearth.untyped

import hearth.MacroCommons

trait UntypedExprs { this: MacroCommons =>

  /** Platform-specific untyped expr representation (`c.Tree` in 2, `quotes.reflect.Term` in 3).
    *
    * Typed [[Expr]] and [[UntypedExpr]] exist because some macro operations do not require the full knowledge about the
    * type (because they operate on Symbols, and we would have to convert from the typed representation to Symbols to
    * use them), and some require the full knowledge about the type (because e.g. type parameters from the class has to
    * be applied to its methods and their arguments/returned values).
    *
    * The implementation will use the right underlying representation to perform the operations, and where possible
    * convert between typed and untyped representations, but the distinction would be useful in cases where some
    * operations are only available to only one of them. Then user could covert between them in the context where the
    * missing information is available.
    *
    * Note that existential type [[Expr_??]], is not an [[UntypedExpr]] - it's a typed representaiton, where the macro
    * during **its excution** would know the exact type BUT it's inconvenient for us to use generics to represent that
    * exact type during compilation of the macro itself (not it's expansion).
    *
    * @since 0.1.0
    */
  type UntypedExpr

  val UntypedExpr: UntypedExprModule
  trait UntypedExprModule { this: UntypedExpr.type =>

    def fromTyped[A](expr: Expr[A]): UntypedExpr
    def toTyped[A: Type](untyped: UntypedExpr): Expr[A]
    def as_??(untyped: UntypedExpr): Expr_??
  }

  implicit final class UntypedExprMethods(private val untyped: UntypedExpr) {

    def asTyped[A: Type]: Expr[A] = UntypedExpr.toTyped(untyped)
    def as_?? : Expr_?? = UntypedExpr.as_??(untyped)
  }
}
