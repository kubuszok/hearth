package hearth.untyped

import hearth.MacroCommons

trait Exprs { this: MacroCommons =>

  /** Platform-specific untyped expr representation (`c.Tree` in 2, `quotes.reflect.Term` in 3) */
  type UntypedExpr

  val UntypedExpr: UntypedExprModule
  trait UntypedExprModule { this: UntypedExpr.type =>

    def fromTyped[A](expr: Expr[A]): UntypedExpr
    def toTyped[A: Type](untyped: UntypedExpr): Expr[A]
    def as_??(untyped: UntypedExpr): Expr_??

    def defaultValue(untyped: UntypedType)(param: UntypedParameter): Option[UntypedExpr]
  }

  implicit final class UntypedExprMethods(private val untyped: UntypedExpr) {

    def asTyped[A: Type]: Expr[A] = UntypedExpr.toTyped(untyped)
    def as_?? : Expr_?? = UntypedExpr.as_??(untyped)
  }
}
