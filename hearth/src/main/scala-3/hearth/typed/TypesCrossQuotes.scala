package hearth
package typed

private[typed] trait TypesCrossQuotes { this: Types =>

  trait TypeCrossQuotes { this: Type.type =>

    // The scaladoc on `of` below is duplicated verbatim in the scala-2 copy of this file
    // (there is no shared supertype carrying it) — keep in sync with the scala-2 copy.

    /** Materializes a `Type[A]` for a statically-known type `A`, portably across Scala 2 and 3.
      *
      * Rewritten by Cross-Quotes into `weakTypeTag[A]` (Scala 2) or `scala.quoted.Type.of[A]` (Scala 3). Enclosing
      * `[A: Type]` context bounds and implicit `Type[_]` params are wired in automatically, so `Type.of[Option[A]]`
      * works for an abstract `A: Type` — you never summon a `WeakTypeTag` or pass `scala.quoted.Quotes` by hand.
      *
      * ==Self-referential implicit==
      *
      * Both `weakTypeTag[A]` and `scala.quoted.Type.of[A]` pick up an implicit `Type[A]` in scope. Since hearth#285 the
      * val being defined is excluded from that search, so
      * `implicit val ConfigT: Type[Configuration] = Type.of[Configuration]` works for '''statically-known''' types; but
      * for an '''abstract''' `A` whose only `Type[A]` would be the val itself there is nothing to materialize from —
      * get `Type[A]` from a `[A: Type]` context bound / implicit param instead.
      *
      * ==Limitations==
      *
      *   - `Type.of` referencing an existentially-imported type fails to compile; remedy: use a `def` with type bounds.
      *   - `Type.of[F[A, ?]]` loses the wildcard inside methods with type params (`not found: type ?`); avoid the
      *     inline wildcard (name the param or restructure).
      *   - Sibling `implicit lazy val Type` givens that mutually force each other can deadlock on Scala 3 cross-quotes
      *     (hearth#316); remedy: hoist them to '''non-implicit''' lazies.
      *
      * @tparam A
      *   the (statically-known) type to materialize
      * @return
      *   a `Type[A]` usable in both quotation and reflection APIs
      * @see
      *   docs/user-guide/cross-quotes.md
      * @since 0.1.0
      */
    @scala.annotation.compileTimeOnly("Install cross-quotes-plugin to use this method")
    final def of[A]: Type[A] = sys.error("Install cross-quotes-plugin to use this method")
  }
}
