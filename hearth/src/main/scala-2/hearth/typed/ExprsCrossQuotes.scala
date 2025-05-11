package hearth
package typed

import hearth.cq.CrossQuotesMacros

import scala.language.experimental.macros

private[typed] trait ExprsCrossQuotes { this: Exprs =>

  trait ExprCrossQuotes { this: Expr.type =>

    @scala.annotation.compileTimeOnly("Should have been expanded by the hearth-cross-quotes macros")
    def quote[A](expr: A): Expr[A] = macro CrossQuotesMacros.quoteImpl[A]

    @scala.annotation.compileTimeOnly("Should have been expanded by the hearth-cross-quotes macros")
    def splice[A](expr: Expr[A]): A = sys.error("Should be called only inside of Expr.quote { ... }")
  }
}
