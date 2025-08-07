package hearth

trait EnvironmentCrossQuotesSupport { this: Environments =>

  type Type[A]

  type Expr[A]

  /** Scala 3 needs to provide some platform-specific utilities to use in the compiler plugin. */
  private[hearth] trait CrossQuotesSupport { this: CrossQuotes.type =>

    // Utilities to convert between Type[A] <=> scala.quoted.Type[A] and Expr[A] <=> scala.quoted.Expr[A].
    // This is useful because type-inference in compiler plugins is a PITA, and expressing castling like that
    // allow us to use it.

    final inline def typeCrossToQuotes[A](tpe: Type[A]): scala.quoted.Type[A] = tpe.asInstanceOf[scala.quoted.Type[A]]
    final inline def typeQuotesToCross[A](tpe: scala.quoted.Type[A]): Type[A] = tpe.asInstanceOf[Type[A]]
    final inline def exprCrossToQuotes[A](expr: Expr[A]): scala.quoted.Expr[A] = expr.asInstanceOf[scala.quoted.Expr[A]]
    final inline def exprQuotesToCross[A](expr: scala.quoted.Expr[A]): Expr[A] = expr.asInstanceOf[Expr[A]]

    protected var currentCtx: scala.quoted.Quotes = _

    /** Necessary to use for achieving correct expansion when we extract some Expr.quote inside an Expr.splice,
      * otherwise nested quote-insides can't work.
      */
    final def nestedCtx[A](using newContext: scala.quoted.Quotes)(thunk: => A): A = {
      assert(newContext != currentCtx, "Nested context should not loop")
      val oldContext = currentCtx
      currentCtx = newContext
      try
        thunk
      finally
        currentCtx = oldContext
    }
  }
}
