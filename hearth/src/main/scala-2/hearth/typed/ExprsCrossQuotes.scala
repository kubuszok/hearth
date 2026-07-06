package hearth
package typed

import hearth.cq.CrossQuotesMacros

import scala.language.experimental.macros

private[typed] trait ExprsCrossQuotes { this: Exprs =>

  trait ExprCrossQuotes { this: Expr.type =>

    // The scaladoc on `quote`/`splice` below is duplicated verbatim in the scala-3 copy of this file
    // (there is no shared supertype carrying it) — keep in sync with the scala-3 copy.

    /** Quotes an expression into its typed AST (`Expr[A]`), portably across Scala 2 and 3.
      *
      * Write the code you want to ''generate'' as ordinary Scala inside the braces; Cross-Quotes rewrites it into a
      * native Scala 2 quasiquote (`c.Expr`/`q"..."`) or a Scala 3 quote (`'{ }`) at compile time, so the IDE still
      * gives you completion and type-checking. Use [[splice]] inside the block to weave in other `Expr` values.
      *
      * ==Ownership contract==
      *
      * An `Expr` that you intend to [[splice]] must be '''constructed inside''' the `Expr.quote { ... }` scope (or
      * enclosing `Expr.splice` body) that splices it. Cross-Quotes' automatic `Quotes`/owner management is applied ''at
      * construction time'' and is '''not retroactive''': an `Expr` built earlier and merely referenced later still
      * carries the owner / `Quotes` scope that was active when it was built. On Scala 3 under `-Xcheck-macros` a
      * violation surfaces as "Block contains definition with different owners" or a `ScopeException`. To satisfy the
      * contract, '''defer construction to the splice site''': build the `Expr` through a `def` or `lazy val` (never a
      * plain eager `val`), or keep it inside a lazy `MIO` value. Caches must hold the ''recipe'' / `MIO`, never a
      * materialized `Expr`.
      *
      * ==Prefer laziness (Scala 3)==
      *
      * A plain `val` that stores a Hearth-built `Expr` is evaluated once and pins that `Expr` to the `Quotes` scope
      * active at that point; splicing it later inside `Expr.quote` — which enters a ''nested'' `Quotes` — then fails
      * with a wrong-staging-level error on Scala 3. Use `def` / `lazy val` / a lazy `MIO` so the builder re-runs at the
      * splice site under the current dynamic `Quotes`. (Scala 2 `Expr`s are untyped trees without scope tracking, so
      * this is a Scala-3-only hazard — but writing lazily is the portable habit.)
      *
      * ==Automatic context management==
      *
      * Inside the block you do '''not''' summon `WeakTypeTag`s (Scala 2) or pass `scala.quoted.Quotes` (Scala 3) by
      * hand: Cross-Quotes injects the implicit `Type[_]` conversions the native quotation layer needs and threads the
      * local context automatically. In particular you never call `passQuotes` / `withQuotes` inside `Expr.quote` —
      * those are only for hand-written native `'{ }` / `${ }`.
      *
      * @see
      *   [[splice]]
      * @see
      *   docs/user-guide/cross-quotes.md
      *
      * @since 0.1.0
      *
      * @tparam A
      *   the static type of the resulting `Expr`
      * @param expr
      *   the code to quote; '''by-name''', and the body is rewritten at compile time rather than evaluated at runtime
      * @return
      *   an `Expr[A]` — the AST of `expr`, valid only within the macro expansion
      */
    @scala.annotation.compileTimeOnly("Should have been expanded by the hearth-cross-quotes macros")
    def quote[A](expr: => A): Expr[A] = macro CrossQuotesMacros.quoteImpl[A]

    /** Splices (unquotes) an `Expr`'s AST back into surrounding quoted code.
      *
      * Can be used '''only directly inside''' an [[quote]] block, and '''only''' on an `Expr[A]`; it has no meaning at
      * runtime (calling it outside a quote is a compile-time error).
      *
      * The spliced `Expr` must have been built in a scope compatible with the enclosing [[quote]] — see the ownership
      * contract on [[quote]]. On Scala 3 the `Expr` should be produced lazily (`def` / `lazy val` / lazy `MIO`) so it
      * is materialized at the splice site under the current `Quotes` (see [[quote]]).
      *
      * @see
      *   [[quote]]
      * @see
      *   docs/user-guide/cross-quotes.md
      *
      * @since 0.1.0
      *
      * @tparam A
      *   the static type of the spliced `Expr`
      * @param expr
      *   the `Expr` whose AST to inline; must be built in a scope compatible with the enclosing [[quote]] (see the
      *   ownership contract there)
      * @return
      *   the spliced value, as seen by the surrounding quoted code (never materialized at runtime)
      */
    @scala.annotation.compileTimeOnly("Should have been expanded by the hearth-cross-quotes macros")
    def splice[A](expr: Expr[A]): A = sys.error("Should be called only inside of Expr.quote { ... }")
  }
}
