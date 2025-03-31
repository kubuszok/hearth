package hearth
package untyped

import hearth.MacroCommonsScala2

trait ExprsScala2 extends Exprs { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type UntypedExpr = Tree

  object UntypedExpr extends UntypedExprModule {

    def fromTyped[A](expr: Expr[A]): UntypedExpr = expr.tree
    def toTyped[A: Type](untyped: UntypedExpr): Expr[A] = c.Expr[A](untyped)
    def as_??(untyped: UntypedExpr): Expr_?? = {
      val resultType: ?? = UntypedType.as_??(
        try
          c.Expr(untyped).actualType.finalResultType
        catch {
          case _: Throwable => c.Expr(untyped).staticType.finalResultType
        }
      )
      import resultType.Underlying as Result
      toTyped[Result](untyped).as_??
    }
  }
}
