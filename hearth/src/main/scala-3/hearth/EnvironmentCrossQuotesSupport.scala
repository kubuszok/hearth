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

    /** Converts an `UntypedType` (= `TypeRepr`) to a `scala.quoted.Type[?]` for injection into quotes. */
    final def untypedToQuotedType(untyped: Any): Any = {
      given q: scala.quoted.Quotes = currentCtx
      untyped.asInstanceOf[q.reflect.TypeRepr].asType
    }

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

  /** Captures the current [[scala.quoted.Quotes]] context for Hearth utlities.
    *
    * As you may have noticed, Hearth API does not take any [[scala.quoted.Quotes]] as an implicit parameter, to make it
    * possible to share the code between Scala 2 and Scala 3 macros.
    *
    * However, Scala 3 macros create a new implicit [[scala.quoted.Quotes]] inside a splice (`${ /* new implicit Quotes
    * */ ... }`), and takes [[scala.quoted.Quotes]] as an implicit parameter when creating an Expr with quoting (`/*
    * using Quotes */ '{}`).
    *
    * Because of that, it is necessary to capture the current [[scala.quoted.Quotes]] within splices (if you aim to call
    * Hearth utilities inside a splice), and restore it before creating an Expr with quoting (when there is a change
    * that some Hearth utility might use it, e.g. during recursive Expr building).
    *
    * These are handled automatically by Cross-Quotes [[Expr.quote]] and [[Expr.splice]] when you are constructing
    * expressions in a cross-compilable way, but have to be handled manually when you are constructing expressions in a
    * Scala 3 specific way.
    *
    * {{{
    * withQuotes { // restores the current Quotes context
    *   '{
    *     val value = ...
    *     ${ passQuotes { recursiveCall('{ value }) } } // captures the current Quotes context
    *   }
    * }
    * }}}
    *
    * @see
    *   [[withQuotes]] for a complimentary method
    * @see
    *   [[CrossQuotes.nestedCtx]] for the underlying implementation
    *
    * @since 0.3.0
    */
  final def passQuotes[A](using scala.quoted.Quotes)(thunk: => A): A =
    CrossQuotes.nestedCtx(thunk)

  /** Restores the current [[scala.quoted.Quotes]] context for Scala 3-specific quoting.
    *
    * {{{
    * withQuotes { // restores the current Quotes context
    *   '{
    *     val value = ...
    *     ${ passQuotes { recursiveCall('{ value }) } } // captures the current Quotes context
    *   }
    * }
    * }}}
    *
    * @see
    *   [[passQuotes]] for a complimentary method and the explanation
    * @see
    *   [[CrossQuotes.ctx]] for the underlying implementation
    *
    * @since 0.3.0
    */
  final def withQuotes[A](thunk: scala.quoted.Quotes ?=> A): A =
    thunk(using CrossQuotes.ctx[scala.quoted.Quotes])
}
