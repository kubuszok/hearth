package hearth
package untyped

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedExpr = Term

  object UntypedExpr extends UntypedExprModule {

    override def fromTyped[A](expr: Expr[A]): UntypedExpr = expr.asTerm
    override def toTyped[A: Type](untyped: UntypedExpr): Expr[A] = untyped.asExprOf[A]
    override def as_??(untyped: UntypedExpr): Expr_?? = {
      val resultType = UntypedType.as_??(untyped.tpe)
      import resultType.Underlying as Result
      toTyped[Result](untyped).as_??
    }

    override def defaultValue(instanceTpe: UntypedType)(param: UntypedParameter): Option[UntypedExpr] = ???
  }
}
