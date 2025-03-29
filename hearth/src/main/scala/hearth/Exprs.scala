package hearth

trait Exprs { this: MacroCommons =>

  type Expr[A]

  val Expr: ExprModule
  trait ExprModule { this: Expr.type =>

    def prettyPrint[A](expr: Expr[A]): String
  }

  implicit class ExprMethods[A](private val expr: Expr[A]) {
    def prettyPrint: String = Expr.prettyPrint(expr)
  }

  type Expr_?? = Any
}
