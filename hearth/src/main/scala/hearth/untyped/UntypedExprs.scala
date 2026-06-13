package hearth.untyped

import hearth.MacroCommons

trait UntypedExprs { this: MacroCommons =>

  /** Platform-specific untyped expr representation (`c.Tree` in 2, `quotes.reflect.Term` in 3).
    *
    * Typed [[Expr]] and [[UntypedExpr]] exist because some macro operations do not require the full knowledge about the
    * type (because they operate on Symbols, and we would have to convert from the typed representation to Symbols to
    * use them), and some require the full knowledge about the type (because e.g. type parameters from the class have to
    * be applied to its methods and their arguments/returned values).
    *
    * The implementation will use the right underlying representation to perform the operations, and where possible
    * convert between typed and untyped representations, but the distinction would be useful in cases where some
    * operations are available to only one of them. Then the user could convert between them in the context where the
    * missing information is available.
    *
    * Note that existential type [[Expr_??]], is not an [[UntypedExpr]] - it's a typed representation, where the macro
    * during **its execution** would know the exact type BUT it's inconvenient for us to use generics to represent that
    * exact type during compilation of the macro itself (not its expansion).
    *
    * @since 0.1.0
    */
  type UntypedExpr

  val UntypedExpr: UntypedExprModule
  trait UntypedExprModule { this: UntypedExpr.type =>

    def fromTyped[A](expr: Expr[A]): UntypedExpr
    def toTyped[A: Type](untyped: UntypedExpr): Expr[A]
    def as_??(untyped: UntypedExpr): Expr_??

    /** Returns the [[Position]] of this expression's underlying tree, if available.
      *
      * Useful for assert-style macros that want to recover the original source text of a (sub)expression via
      * [[PositionMethods.sourceCode]].
      *
      * @since 0.4.0
      */
    def position(untyped: UntypedExpr): Option[Position]
    final def as_??(untyped: UntypedExpr, knownType: UntypedType): Expr_?? = {
      implicit val tpe: Type[Any] = UntypedType.toTyped[Any](knownType)
      toTyped[Any](untyped).as_??
    }
  }

  implicit final class UntypedExprMethods(private val untyped: UntypedExpr) {

    def asTyped[A: Type]: Expr[A] = UntypedExpr.toTyped(untyped)
    def as_?? : Expr_?? = UntypedExpr.as_??(untyped)

    /** Returns the [[Position]] of this expression's underlying tree, if available.
      *
      * @since 0.4.0
      */
    def position: Option[Position] = UntypedExpr.position(untyped)

    /** Returns the original source text of this expression, if available.
      *
      * @since 0.4.0
      */
    def sourceCode: Option[String] = UntypedExpr.position(untyped).flatMap(Position.sourceCode)
  }
}
