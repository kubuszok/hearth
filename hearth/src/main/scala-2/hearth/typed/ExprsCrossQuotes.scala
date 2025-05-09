package hearth
package typed

import hearth.cq.CrossQuotesMacros

import scala.language.experimental.macros

private[typed] trait ExprsCrossQuotes { this: Exprs =>

  trait ExprCrossQuotes { this: Expr.type =>

    // @scala.annotation.compileTimeOnly("Install cross-quotes-plugin to use this method")
    def quote[A](expr: A): Expr[A] = macro CrossQuotesMacros.quoteImpl[A]

    @scala.annotation.compileTimeOnly("Install cross-quotes-plugin to use this method")
    def splice[A](expr: Expr[A]): A = sys.error("Should be called only inside of Expr.quote { ... }")
  }
}
