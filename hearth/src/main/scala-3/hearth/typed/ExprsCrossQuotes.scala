package hearth
package typed

private[typed] trait ExprsCrossQuotes { this: Exprs =>

  trait ExprCrossQuotes { this: Expr.type =>

    @scala.annotation.compileTimeOnly("Install cross-quotes-plugin to use this method")
    final def quote[A](expr: A): Expr[A] = sys.error("Install cross-quotes-plugin to use this method")

    @scala.annotation.compileTimeOnly("Install cross-quotes-plugin to use this method")
    final def splice[A](expr: Expr[A]): A = sys.error("Install cross-quotes-plugin to use this method")
  }
}
