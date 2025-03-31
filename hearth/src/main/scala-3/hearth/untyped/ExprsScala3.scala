package hearth
package untyped

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedExpr = Term

  object UntypedExpr extends UntypedExprModule {

    def fromTyped[A](expr: Expr[A]): UntypedExpr = expr.asTerm
    def toTyped[A: Type](untyped: UntypedExpr): Expr[A] = untyped.asExprOf[A]
    def as_??(untyped: UntypedExpr): Expr_?? = {
      val resultType = UntypedType.as_??(untyped.tpe)
      import resultType.Underlying as Result
      toTyped[Result](untyped).as_??
    }
  }
}
