package hearth
package typed

import hearth.fp.data.{NonEmptyList, NonEmptyVector}
import hearth.fp.syntax.*

import scala.language.implicitConversions

trait Exprs extends ExprsCrossQuotes with ExprsCompat { this: MacroCommons =>

  /** Platform-specific untyped type representation (`c.Expr[A]` in 2, `scala.quoted.Expr[A]` in 3).
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
  type Expr[A]

  val Expr: ExprModule
  trait ExprModule extends ExprCrossQuotes with ExprCompat { this: Expr.type =>

    /** Lifts a value into its [[Expr]] using the given [[ExprCodec]]: `Expr(value)` is exactly
      * `ExprCodec[A].toExpr(value)`.
      *
      * '''Pitfall:''' for enum / constant values, cross-quotes may reify a constant by its simple name, which can
      * resolve to the wrong symbol in the expansion scope; reference such values through a fully-qualified path or a
      * companion method to stay safe.
      *
      * @param value
      *   the value to lift into an expression
      * @return
      *   an `Expr` reproducing `value` at the splice site
      * @see
      *   [[unapply]] for the inverse (unlift), and plain-text `docs/user-guide/cross-quotes.md`
      * @since 0.1.0
      */
    final def apply[A: ExprCodec](value: A): Expr[A] = ExprCodec[A].toExpr(value)

    /** Unlifts a constant [[Expr]] back to its value via the given [[ExprCodec]], enabling `case Expr(v) => ...` in a
      * pattern match; yields `None` when `expr` is not a compile-time constant.
      *
      * @param expr
      *   the expression to unlift
      * @return
      *   `Some(value)` if `expr` is a compile-time constant, else `None`
      * @see
      *   [[apply]] for the inverse (lift)
      * @since 0.1.0
      */
    final def unapply[A: ExprCodec](expr: Expr[A]): Option[A] = ExprCodec[A].fromExpr(expr)

    /** Uncolored source-like rendering; this is the correct key for comparing/sorting/asserting on expressions — do not
      * regex-strip ANSI escapes off [[prettyPrint]].
      *
      * Renders `expr` as compilable-looking source code without any ANSI color codes. Because the output is stable and
      * free of escape sequences, it is the value you should use as a map key, in a `Set`, when sorting, or in test
      * assertions. Its colored sibling [[prettyPrint]] is for human display only.
      *
      * Unlike the `Method`/`Type` renderers, the Expr-side printers take no `SyntaxHighlight` parameter — the
      * colored/uncolored choice IS the `plain` vs `pretty` method name. For the raw compiler-tree structure (rather
      * than source-like code) use [[plainAST]].
      *
      * @param expr
      *   the expression to render
      * @return
      *   uncolored, source-like rendering of `expr`
      * @see
      *   [[prettyPrint]] for the colored, human-display variant
      * @see
      *   [[plainAST]] for the uncolored raw-AST (tree structure) variant
      * @see
      *   [[TypeModule.plainPrint]] for the analogous Type-side renderer
      * @see
      *   plain-text `docs/user-guide/better-printers.md`
      * @since 0.1.0
      */
    def plainPrint[A](expr: Expr[A]): String

    /** ANSI-colored, source-like rendering for human display only (compiler messages, logs); use [[plainPrint]] for
      * comparing, sorting or asserting on expressions.
      *
      * Same source-like rendering as [[plainPrint]] but with ANSI syntax highlighting. Never regex-strip the color
      * codes off this to recover a comparison key — call [[plainPrint]] instead.
      *
      * @param expr
      *   the expression to render
      * @return
      *   colored, source-like rendering of `expr`
      * @see
      *   [[plainPrint]] for the uncolored compare/sort key
      * @see
      *   [[prettyAST]] for the colored raw-AST (tree structure) variant
      * @see
      *   [[TypeModule.prettyPrint]] for the analogous Type-side renderer
      * @since 0.1.0
      */
    def prettyPrint[A](expr: Expr[A]): String

    /** Uncolored dump of the raw compiler AST (tree structure, not source-like code) — for debugging tree shape; see
      * [[plainPrint]] for source rendering.
      *
      * @param expr
      *   the expression whose tree structure to render
      * @return
      *   uncolored dump of the underlying compiler AST
      * @since 0.1.0
      */
    def plainAST[A](expr: Expr[A]): String

    /** ANSI-colored dump of the raw compiler AST (tree structure, not source-like code) — for debugging tree shape; see
      * [[prettyPrint]] for source rendering. Use [[plainAST]] for a comparison-safe (uncolored) variant.
      *
      * @param expr
      *   the expression whose tree structure to render
      * @return
      *   colored dump of the underlying compiler AST
      * @since 0.1.0
      */
    def prettyAST[A](expr: Expr[A]): String

    /** Summons an implicit / given of type `A` at the macro-expansion point, returning a [[SummoningResult]] that
      * distinguishes found / ambiguous / diverging / not-found outcomes without ever throwing.
      *
      * @return
      *   a [[SummoningResult]] carrying the found `Expr[A]` or the reason none was produced
      * @see
      *   [[SummoningResult]] for projecting the outcome
      * @see
      *   [[summonImplicitIgnoring]] to exclude specific implicit definitions from the search
      * @since 0.1.0
      */
    def summonImplicit[A: Type]: SummoningResult[A]

    /** Like [[summonImplicit]] but excludes the given implicit definitions from the search — e.g. so a macro does not
      * summon the very instance it is currently deriving.
      *
      * @param excluded
      *   implicit definitions (as [[UntypedMethod]]s) to omit from the implicit search
      * @return
      *   a [[SummoningResult]] as in [[summonImplicit]]
      * @since 0.1.0
      */
    def summonImplicitIgnoring[A: Type](excluded: UntypedMethod*): SummoningResult[A]
    def summonImplicitByType(tpe: UntypedType): Option[UntypedExpr]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit]

    /** Attaches the annotation `@Ann(arguments...)` to `expr` by binding it to a fresh, annotated `val`, producing the
      * equivalent of `{ @Ann(arguments...) val fresh = expr; fresh }`.
      *
      * `Ann` is the annotation TYPE and `arguments` its constructor arguments (empty for a parameterless annotation).
      * The annotation is built in annotation position, so this works uniformly for Scala annotations
      * (`scala.annotation.nowarn`, with or without a message) and for JAVA annotations (`java.lang.SuppressWarnings`) —
      * the latter cannot be instantiated in expression position (`new SuppressWarnings(...)` does not typecheck), which
      * is why an annotation TYPE rather than an instance `Expr` is taken. Use this to wrap generated code in a
      * user-configured `@nowarn`/`@SuppressWarnings` (or any annotation), which is otherwise not expressible
      * cross-platform.
      *
      * {{{
      * // @nowarn
      * expr.annotated[scala.annotation.nowarn]()
      * // @nowarn("msg")
      * expr.annotated[scala.annotation.nowarn](Expr("msg").asUntyped)
      * // @SuppressWarnings(Array("..."))
      * expr.annotated[java.lang.SuppressWarnings](Expr.quote(scala.Array("...")).asUntyped)
      * }}}
      *
      * @since 0.4.1
      */
    def annotated[A: Type, Ann: Type](expr: Expr[A], arguments: List[UntypedExpr]): Expr[A]

    def singletonOf[A: Type]: Option[Expr[A]]

    /** Returns the type of an expression as seen by the compiler.
      *
      * '''Warning:''' the returned type may be narrower than the declared `A`. For instance, if `A` is `Any` but the
      * expression tree carries `String`, this method returns `Type[String]` (typed as `Type[A]`). Use this only when
      * you need the compiler-inferred type and are aware that it may be more specific than the type parameter suggests.
      *
      * @since 0.3.1
      */
    def typeOf[A](expr: Expr[A]): Type[A]

    /** Evaluates an expression tree at macro-expansion time via reflection, returning the runtime value.
      *
      * `Right` holds the statically-computed value; `Left` holds the aggregated reasons the tree is NOT statically
      * evaluable (a non-constant sub-tree, an unsupported type, etc.). This is the inverse of `semiQuote`.
      *
      * Supported without overrides: primitives, `String`, `BigInt`/`BigDecimal`, `Data`, case classes (evaluated
      * field-by-field), enums / sealed traits (dispatched to the matching child at runtime), and singletons / case
      * objects.
      *
      * `overrides` is a dispatch function keyed by the erased [[UntypedType]] of a (sub)value; it supplies custom
      * macro-time evaluation for nested types and is checked BEFORE the built-in reflection path. Each entry is an
      * [[EvalOverride]] (`Some(f)` to override, `None` to fall through).
      *
      * @param expr
      *   the expression tree to evaluate at macro time
      * @param overrides
      *   per-type evaluation dispatch, keyed by [[UntypedType]]
      * @return
      *   `Right(value)` if statically evaluable, else `Left` with the aggregated reasons
      * @see
      *   `semiQuote` for the inverse (value → `Expr`)
      * @see
      *   [[EvalOverride]] for the per-type override shape
      * @since 0.3.0
      */
    def semiEval[A](
        expr: Expr[A],
        overrides: UntypedType => Existential[EvalOverride]
    ): Either[NonEmptyVector[String], A]

    /** Evaluates an expression tree at macro time via reflection with no custom overrides — see the two-argument
      * `semiEval` overload for the full contract.
      *
      * @param expr
      *   the expression tree to evaluate at macro time
      * @return
      *   `Right(value)` if statically evaluable, else `Left` with the aggregated reasons
      * @since 0.3.0
      */
    final def semiEval[A](expr: Expr[A]): Either[NonEmptyVector[String], A] = semiEval(expr, null)

    /** Turns a runtime value into an expression tree at macro time — the inverse of `semiEval`.
      *
      * `Right` holds the constructed `Expr`; `Left` holds a message when the value's type is not quotable.
      *
      * Supported without overrides: primitives, `String`, `BigInt`/`BigDecimal`, `Data`, case classes (quoted
      * field-by-field), enums / sealed traits (dispatched to the matching child at runtime), and singletons / case
      * objects.
      *
      * `overrides` is a dispatch function keyed by the erased [[UntypedType]] of a (sub)value; it supplies custom
      * macro-time quoting for nested types and is checked BEFORE the built-in path. Each entry is a [[QuoteOverride]]
      * (`Some(f)` to override, `None` to fall through).
      *
      * @param value
      *   the runtime value to lift into an `Expr`
      * @param overrides
      *   per-type quoting dispatch, keyed by [[UntypedType]]
      * @return
      *   `Right(expr)` if the value is quotable, else `Left` with the reason
      * @see
      *   [[EvalOverride]] and the `semiEval` overloads for the inverse (`Expr` → value)
      * @see
      *   [[QuoteOverride]] for the per-type override shape
      * @since 0.3.0
      */
    def semiQuote[A: Type](value: A, overrides: UntypedType => Existential[QuoteOverride]): Either[String, Expr[A]]

    /** Turns a runtime value into an expression tree at macro time with no custom overrides — see the two-argument
      * `semiQuote` overload for the full contract.
      *
      * @param value
      *   the runtime value to lift into an `Expr`
      * @return
      *   `Right(expr)` if the value is quotable, else `Left` with the reason
      * @since 0.3.0
      */
    final def semiQuote[A: Type](value: A): Either[String, Expr[A]] = semiQuote(value, null)

    def NullExprCodec: ExprCodec[Null]
    def UnitExprCodec: ExprCodec[Unit]
    def BooleanExprCodec: ExprCodec[Boolean]
    def ByteExprCodec: ExprCodec[Byte]
    def ShortExprCodec: ExprCodec[Short]
    def IntExprCodec: ExprCodec[Int]
    def LongExprCodec: ExprCodec[Long]
    def FloatExprCodec: ExprCodec[Float]
    def DoubleExprCodec: ExprCodec[Double]
    def CharExprCodec: ExprCodec[Char]
    def StringExprCodec: ExprCodec[String]

    def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]]
    def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]]

    def BigIntExprCodec: ExprCodec[BigInt]
    def BigDecimalExprCodec: ExprCodec[BigDecimal]
    def StringContextExprCodec: ExprCodec[StringContext]

    def ArrayExprCodec[A: ExprCodec: Type]: ExprCodec[Array[A]]
    def SeqExprCodec[A: ExprCodec: Type]: ExprCodec[Seq[A]]
    def ListExprCodec[A: ExprCodec: Type]: ExprCodec[List[A]]
    def NilExprCodec: ExprCodec[Nil.type]
    def VectorExprCodec[A: ExprCodec: Type]: ExprCodec[Vector[A]]
    def MapExprCodec[K: ExprCodec: Type, V: ExprCodec: Type]: ExprCodec[Map[K, V]]
    def SetExprCodec[A: ExprCodec: Type]: ExprCodec[Set[A]]
    def OptionExprCodec[A: ExprCodec: Type]: ExprCodec[Option[A]]
    def SomeExprCodec[A: ExprCodec: Type]: ExprCodec[Some[A]]
    def NoneExprCodec: ExprCodec[None.type]
    def EitherExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Either[L, R]]
    def LeftExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Left[L, R]]
    def RightExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Right[L, R]]

    final def DataExprCodec: ExprCodec[data.Data] = new ExprCodec[data.Data] {

      private val DataType: Type[data.Data] = Type.of[hearth.data.Data]

      private val StringType: Type[String] = Type.of[String]

      def toExpr(value: data.Data): Expr[data.Data] = value.fold(
        onNull = Expr.quote(hearth.data.Data()),
        onInt = i => {
          val inner: Expr[Int] = Expr(i)
          Expr.quote(hearth.data.Data(Expr.splice(inner)))
        },
        onLong = l => {
          val inner: Expr[Long] = Expr(l)
          Expr.quote(hearth.data.Data(Expr.splice(inner)))
        },
        onFloat = f => {
          val inner: Expr[Float] = Expr(f)
          Expr.quote(hearth.data.Data(Expr.splice(inner)))
        },
        onDouble = d => {
          val inner: Expr[Double] = Expr(d)
          Expr.quote(hearth.data.Data(Expr.splice(inner)))
        },
        onBoolean = b => {
          val inner: Expr[Boolean] = Expr(b)
          Expr.quote(hearth.data.Data(Expr.splice(inner)))
        },
        onString = s => {
          val inner: Expr[String] = Expr(s)
          Expr.quote(hearth.data.Data(Expr.splice(inner)))
        },
        onList = l => {
          implicit val dt: Type[hearth.data.Data] = DataType
          val inner: Expr[List[data.Data]] = Expr(l)
          Expr.quote(hearth.data.Data((Expr.splice(inner))))
        },
        onMap = m => {
          implicit val st: Type[String] = StringType
          implicit val dt: Type[hearth.data.Data] = DataType
          val inner: Expr[Map[String, hearth.data.Data]] = Expr(m)
          Expr.quote(hearth.data.Data(Expr.splice(inner)))
        }
      )
      def fromExpr(expr: Expr[data.Data]): Option[data.Data] = expr.semiEval.toOption
    }

    def HearthVersionExprCodec: ExprCodec[HearthVersion]
    def JDKVersionExprCodec: ExprCodec[JDKVersion]
    def ScalaVersionExprCodec: ExprCodec[ScalaVersion]
    def LanguageVersionExprCodec: ExprCodec[LanguageVersion]
    def PlatformExprCodec: ExprCodec[Platform]
  }

  implicit final class ExprMethods[A](private val expr: Expr[A]) {

    def value(implicit codec: ExprCodec[A]): Option[A] = codec.fromExpr(expr)

    /** Uncolored source-like rendering; this is the correct key for comparing/sorting/asserting on expressions — do not
      * regex-strip ANSI escapes off [[prettyPrint]]. See [[ExprModule.plainPrint]].
      *
      * @since 0.1.0
      */
    def plainPrint: String = Expr.plainPrint(expr)

    /** ANSI-colored source-like rendering for human display only; use [[plainPrint]] for comparisons — see
      * [[ExprModule.prettyPrint]].
      *
      * @since 0.1.0
      */
    def prettyPrint: String = Expr.prettyPrint(expr)

    /** Uncolored dump of the raw compiler AST (tree structure, not source) — see [[ExprModule.plainAST]].
      *
      * @since 0.1.0
      */
    def plainAST: String = Expr.plainAST(expr)

    /** ANSI-colored dump of the raw compiler AST (tree structure, not source) — see [[ExprModule.prettyAST]].
      *
      * @since 0.1.0
      */
    def prettyAST: String = Expr.prettyAST(expr)

    def upcast[B](implicit A: Type[A], B: Type[B]): Expr[B] = Expr.upcast(expr)
    def suppressUnused(implicit A: Type[A]): Expr[Unit] = Expr.suppressUnused(expr)

    /** Attaches an annotation to this expression — see [[ExprModule.annotated]].
      *
      * @since 0.4.1
      */
    def annotated[Ann: Type](arguments: UntypedExpr*)(implicit A: Type[A]): Expr[A] =
      Expr.annotated[A, Ann](expr, arguments.toList)

    /** Returns the type of this expression as seen by the compiler.
      *
      * '''Warning:''' the returned type may be narrower than `A` — see [[ExprModule.typeOf]].
      *
      * @since 0.3.1
      */
    def tpe: Type[A] = Expr.typeOf(expr)

    /** Evaluates this expression at macro time with no custom overrides — see `semiEval`.
      *
      * @since 0.3.0
      */
    def semiEval: Either[NonEmptyVector[String], A] = Expr.semiEval(expr)

    /** Evaluates this expression at macro time with per-type overrides — see `semiEval`.
      *
      * @param overrides
      *   per-type evaluation dispatch, keyed by [[UntypedType]]
      * @since 0.3.0
      */
    def semiEval(overrides: UntypedType => Existential[EvalOverride]): Either[NonEmptyVector[String], A] =
      Expr.semiEval(expr, overrides)

    def asUntyped: UntypedExpr = UntypedExpr.fromTyped(expr)

    /** Returns the [[Position]] of this expression's underlying tree, if available.
      *
      * @since 0.4.0
      */
    def position: Option[Position] = UntypedExpr.position(UntypedExpr.fromTyped(expr))

    /** Returns the original source text of this expression, if available.
      *
      * Useful for assert-style macros that want to show the source of a (sub)expression in a failure message.
      *
      * @since 0.4.0
      */
    def sourceCode: Option[String] = position.flatMap(Position.sourceCode)

    def as_??(implicit A: Type[A]): Expr_?? = Existential[Expr, A](expr)
    def as_??>:[L <: A](implicit A: Type[A]): Expr_??>:[L] = Existential.LowerBounded[L, Expr, A](expr)
    def as_??<:[U >: A](implicit A: Type[A]): Expr_??<:[U] = Existential.UpperBounded[U, Expr, A](expr)
    def as_<:??<:[L <: A, U >: A](implicit A: Type[A]): Expr_<:??<:[L, U] = Existential.Bounded[L, U, Expr, A](expr)
  }

  // Aliases to make the (very common) existential types shorter

  final type Expr_?? = Existential[Expr]
  final type Expr_??>:[L] = Existential.LowerBounded[L, Expr]
  final type Expr_??<:[U] = Existential.UpperBounded[U, Expr]
  final type Expr_<:??<:[L, U >: L] = Existential.Bounded[L, U, Expr]

  implicit final def ExistentialExprMethods(expr: Expr_??): BoundedExistentialExprMethods[Nothing, Any] =
    new BoundedExistentialExprMethods[Nothing, Any](expr)
  implicit final def LowerBoundedExistentialExprMethods[L](expr: Expr_??>:[L]): BoundedExistentialExprMethods[L, Any] =
    new BoundedExistentialExprMethods[L, Any](expr)
  implicit final def UpperBoundedExistentialExprMethods[U](
      expr: Expr_??<:[U]
  ): BoundedExistentialExprMethods[Nothing, U] =
    new BoundedExistentialExprMethods[Nothing, U](expr)
  implicit final class BoundedExistentialExprMethods[L, U >: L](private val expr: Expr_<:??<:[L, U]) {

    /** Uncolored source-like rendering; this is the correct key for comparing/sorting/asserting on expressions — do not
      * regex-strip ANSI escapes off [[prettyPrint]]. See [[ExprModule.plainPrint]].
      *
      * @since 0.1.0
      */
    def plainPrint: String = Expr.plainPrint(expr.value)

    /** ANSI-colored source-like rendering for human display only; use [[plainPrint]] for comparisons — see
      * [[ExprModule.prettyPrint]].
      *
      * @since 0.1.0
      */
    def prettyPrint: String = Expr.prettyPrint(expr.value)

    /** Uncolored dump of the raw compiler AST (tree structure, not source) — see [[ExprModule.plainAST]].
      *
      * @since 0.1.0
      */
    def plainAST: String = Expr.plainAST(expr.value)

    /** ANSI-colored dump of the raw compiler AST (tree structure, not source) — see [[ExprModule.prettyAST]].
      *
      * @since 0.1.0
      */
    def prettyAST: String = Expr.prettyAST(expr.value)

    def asUntyped: UntypedExpr = UntypedExpr.fromTyped(expr.value)
  }

  /** Generalizes over type classes for converting values to and from `Expr`s.
    *
    *   - `Lifting`/`Unlifting` on Scala 2
    *   - `ToExpr`/`FromExpr` on Scala 3
    *
    * Built-in codecs exist for primitives, collections, options, eithers, and Hearth types. For custom types (case
    * classes, sealed traits, singletons), use `ExprCodec.derived[A]` to derive a codec semi-automatically:
    *
    * {{{
    * val codec: ExprCodec[MyType] = ExprCodec.derived[MyType]
    * }}}
    *
    * Derivation supports:
    *   - '''Case classes''' — fields are lifted via their own `ExprCodec` instances (built-in or recursively derived),
    *     constructed via `CaseClass.construct`
    *   - '''Sealed traits / enums''' — runtime dispatch to the matching child codec
    *   - '''Singletons''' (case objects) — lifted via `SingletonValue.singletonExpr`
    *   - '''Nested types''' — field codecs are resolved recursively
    *
    * `derived` is a '''semi-automatic''' derivation you call '''explicitly''' — it is NOT an implicit and is not
    * summoned for you; hand the result to `Expr(...)` / `Expr.unapply` or keep it in a `val`.
    *
    * Derivation short-circuits on a fixed set of '''built-in''' types (leaves) and derives everything else
    * structurally. That fast-path set is exactly: `Null`, `Unit`, all primitives (`Boolean`, `Byte`, `Short`, `Int`,
    * `Long`, `Float`, `Double`, `Char`), `String`, `BigInt`, `BigDecimal`, `Data`, and `Seq[E]` where `E` is itself a
    * built-in (this last case exists only so vararg case-class fields round-trip). Note this is NARROWER than the set
    * of implicit `ExprCodec` instances available from this companion (which also covers
    * `List`/`Vector`/`Set`/`Map`/`Option`/ `Either`/`Array`/`Class`/`ClassTag`/`StringContext` and Hearth
    * version/platform types).
    *
    * '''Pitfall:''' when lifting enum / constant values, cross-quotes may reify a constant by its simple name, which
    * can resolve to the wrong symbol in the expansion scope — reference such values through a fully-qualified path or a
    * companion method (see plain-text `docs/user-guide/cross-quotes.md`).
    *
    * @see
    *   [[https://docs.scala-lang.org/overviews/quasiquotes/lifting.html]] for Scala 2 underlying concept
    * @see
    *   [[https://github.com/scala/scala/blob/master/src/reflect/scala/reflect/api/StandardLiftables.scala]] for Scala 2
    *   built-in liftables
    * @see
    *   [[https://docs.scala-lang.org/scala3/guides/macros/macros.html#extracting-values-from-expressions]] for Scala 3
    *   `FromExpr`
    * @see
    *   [[https://github.com/scala/scala3/blob/master/library/src/scala/quoted/FromExpr.scala]] for Scala 3 built-in
    *   FromExpr
    * @see
    *   [[https://docs.scala-lang.org/scala3/guides/macros/macros.html#creating-expression-from-values]] for Scala 3
    *   `ToExpr`
    * @see
    *   [[https://github.com/scala/scala3/blob/master/library/src/scala/quoted/ToExpr.scala]] for Scala 3 built-in
    *   ToExpr
    *
    * @since 0.1.0
    */
  trait ExprCodec[A] {

    def toExpr(value: A): Expr[A]
    def fromExpr(expr: Expr[A]): Option[A]
  }
  object ExprCodec extends ExprCodecImplicits0 {

    def apply[A](implicit codec: ExprCodec[A]): ExprCodec[A] = codec

    implicit lazy val NullExprCodec: ExprCodec[Null] = Expr.NullExprCodec
    implicit lazy val UnitExprCodec: ExprCodec[Unit] = Expr.UnitExprCodec
    implicit lazy val BooleanExprCodec: ExprCodec[Boolean] = Expr.BooleanExprCodec
    implicit lazy val ByteExprCodec: ExprCodec[Byte] = Expr.ByteExprCodec
    implicit lazy val ShortExprCodec: ExprCodec[Short] = Expr.ShortExprCodec
    implicit lazy val IntExprCodec: ExprCodec[Int] = Expr.IntExprCodec
    implicit lazy val LongExprCodec: ExprCodec[Long] = Expr.LongExprCodec
    implicit lazy val FloatExprCodec: ExprCodec[Float] = Expr.FloatExprCodec
    implicit lazy val DoubleExprCodec: ExprCodec[Double] = Expr.DoubleExprCodec
    implicit lazy val CharExprCodec: ExprCodec[Char] = Expr.CharExprCodec
    implicit lazy val StringExprCodec: ExprCodec[String] = Expr.StringExprCodec

    implicit def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]] = Expr.ClassExprCodec[A]
    implicit def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]] = Expr.ClassTagExprCodec[A]

    implicit lazy val BigIntExprCodec: ExprCodec[BigInt] = Expr.BigIntExprCodec
    implicit lazy val BigDecimalExprCodec: ExprCodec[BigDecimal] = Expr.BigDecimalExprCodec
    implicit lazy val StringContextExprCodec: ExprCodec[StringContext] = Expr.StringContextExprCodec

    implicit def ArrayExprCodec[A: ExprCodec: Type]: ExprCodec[Array[A]] = Expr.ArrayExprCodec[A]
    implicit lazy val NilExprCodec: ExprCodec[Nil.type] = Expr.NilExprCodec
    implicit def SomeExprCodec[A: ExprCodec: Type]: ExprCodec[Some[A]] = Expr.SomeExprCodec[A]
    implicit lazy val NoneExprCodec: ExprCodec[None.type] = Expr.NoneExprCodec
    implicit def LeftExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Left[L, R]] = Expr.LeftExprCodec[L, R]
    implicit def RightExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Right[L, R]] =
      Expr.RightExprCodec[L, R]

    implicit lazy val DataCodec: ExprCodec[data.Data] = Expr.DataExprCodec

    implicit lazy val HearthVersionCodec: ExprCodec[HearthVersion] = Expr.HearthVersionExprCodec
    implicit lazy val JDKVersionCodec: ExprCodec[JDKVersion] = Expr.JDKVersionExprCodec
    implicit lazy val ScalaVersionCodec: ExprCodec[ScalaVersion] = Expr.ScalaVersionExprCodec
    implicit lazy val LanguageVersionCodec: ExprCodec[LanguageVersion] = Expr.LanguageVersionExprCodec
    implicit lazy val PlatformCodec: ExprCodec[Platform] = Expr.PlatformExprCodec
  }
  private[hearth] trait ExprCodecImplicits0 extends ExprCodecImplicits1 with ExprCodecCompat { this: ExprCodec.type =>

    implicit def ListExprCodec[A: ExprCodec: Type]: ExprCodec[List[A]] = Expr.ListExprCodec[A]
    implicit def VectorExprCodec[A: ExprCodec: Type]: ExprCodec[Vector[A]] = Expr.VectorExprCodec[A]
    implicit def MapExprCodec[K: ExprCodec: Type, V: ExprCodec: Type]: ExprCodec[Map[K, V]] = Expr.MapExprCodec[K, V]
    implicit def SetExprCodec[A: ExprCodec: Type]: ExprCodec[Set[A]] = Expr.SetExprCodec[A]
    implicit def OptionExprCodec[A: ExprCodec: Type]: ExprCodec[Option[A]] = Expr.OptionExprCodec[A]
    implicit def EitherExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Either[L, R]] =
      Expr.EitherExprCodec[L, R]
  }
  private[hearth] trait ExprCodecImplicits1 { this: ExprCodec.type =>

    implicit def SeqExprCodec[A: ExprCodec: Type]: ExprCodec[Seq[A]] = Expr.SeqExprCodec[A]
  }

  /** Per-type override for `semiEval`: `Some(f)` supplies custom macro-time evaluation for values of type `A`,
    * dispatched by [[UntypedType]]; `None` falls through to the built-in reflection path.
    *
    * @see
    *   `semiEval`
    * @see
    *   [[QuoteOverride]] for the inverse (value → `Expr`)
    * @since 0.3.0
    */
  type EvalOverride[A] = Option[Expr[A] => Either[String, A]]
  object EvalOverride {

    /** Wraps a macro-time evaluation function as an override.
      *
      * @param f
      *   evaluates an `Expr[A]` to `Right(value)` or `Left(reason)`
      * @since 0.3.0
      */
    def apply[A](f: Expr[A] => Either[String, A]): EvalOverride[A] = Some(f)

    /** No override — fall through to the built-in reflection path.
      *
      * @since 0.3.0
      */
    def none[A]: EvalOverride[A] = None
  }

  /** Per-type override for `semiQuote`: `Some(f)` supplies custom macro-time quoting for values of type `A`, dispatched
    * by [[UntypedType]]; `None` falls through to the built-in path.
    *
    * @see
    *   `semiQuote`
    * @see
    *   [[EvalOverride]] for the inverse (`Expr` → value)
    * @since 0.3.0
    */
  type QuoteOverride[A] = Option[A => Either[String, Expr[A]]]
  object QuoteOverride {

    /** Wraps a macro-time quoting function as an override.
      *
      * @param f
      *   quotes a value of type `A` to `Right(expr)` or `Left(reason)`
      * @since 0.3.0
      */
    def apply[A](f: A => Either[String, Expr[A]]): QuoteOverride[A] = Some(f)

    /** No override — fall through to the built-in path.
      *
      * @since 0.3.0
      */
    def none[A]: QuoteOverride[A] = None
  }

  /** Outcome of [[ExprModule.summonImplicit]], distinguishing found / ambiguous / diverging / not-found without
    * throwing. Project it via `toOption`, `toEither` or `fold`.
    *
    * @see
    *   [[ExprModule.summonImplicit]]
    * @since 0.1.0
    */
  sealed trait SummoningResult[A] extends Product with Serializable {

    final def isDefined: Boolean = this match {
      case SummoningResult.Found(_) => true
      case _                        => false
    }
    final def isEmpty: Boolean = !isDefined
    final def nonEmpty: Boolean = isDefined

    final def get: Expr[A] = toEither match {
      case Right(expr) => expr
      case Left(error) => throw new NoSuchElementException(error)
    }

    final def toOption: Option[Expr[A]] = this match {
      case SummoningResult.Found(expr) => Some(expr)
      case _                           => None
    }

    final def toEither: Either[String, Expr[A]] = this match {
      case SummoningResult.Found(expr)    => Right(expr)
      case SummoningResult.Ambiguous(tpe) => Left(s"Ambiguous implicit value of type ${tpe.prettyPrint}")
      case SummoningResult.Diverging(tpe) => Left(s"Diverging implicit value of type ${tpe.prettyPrint}")
      case SummoningResult.NotFound(tpe)  => Left(s"No implicit value of type ${tpe.prettyPrint} found")
    }

    final def fold[B](
        found: Expr[A] => B,
        ambiguous: Type[A] => B,
        diverging: Type[A] => B,
        notFound: Type[A] => B
    ): B = this match {
      case SummoningResult.Found(expr)    => found(expr)
      case SummoningResult.Ambiguous(tpe) => ambiguous(tpe)
      case SummoningResult.Diverging(tpe) => diverging(tpe)
      case SummoningResult.NotFound(tpe)  => notFound(tpe)
    }
  }
  object SummoningResult {

    final case class Found[A](expr: Expr[A]) extends SummoningResult[A]
    final case class Ambiguous[A](tpe: Type[A]) extends SummoningResult[A]
    final case class Diverging[A](tpe: Type[A]) extends SummoningResult[A]
    final case class NotFound[A](tpe: Type[A]) extends SummoningResult[A]
  }

  /** Allow us to convert VarArgs to various collection types.
    *
    * When the macro input is variadic argument, we have different interface between Scala 2 and Scala 3:
    *   - Scala 2: `Seq[Expr[A]]`
    *   - Scala 3: `Expr[Seq[A]]`
    *
    * This type allows us to use the same interface in both cases.
    *
    * @since 0.1.0
    */
  type VarArgs[A]

  val VarArgs: VarArgsModule
  trait VarArgsModule { this: VarArgs.type =>

    def toIterable[A](args: VarArgs[A]): Iterable[Expr[A]]

    def from[A: Type](iterable: Iterable[Expr[A]]): Expr[Seq[A]]
    final def apply[A: Type](exprs: Expr[A]*): Expr[Seq[A]] = from(exprs.to(Iterable))
  }

  implicit final class VarArgsMethods[A](private val varArgs: VarArgs[A]) {

    def toIterable: Iterable[Expr[A]] = VarArgs.toIterable(varArgs)
    def toSeq: Seq[Expr[A]] = toIterable.toSeq
    def toList: List[Expr[A]] = toIterable.toList
    def toVector: Vector[Expr[A]] = toIterable.toVector
    def to[C](factory: scala.collection.Factory[Expr[A], C]): C = toIterable.to(factory)
  }

  /** Provides support for building pattern-matching expressions.
    *
    * {{{
    * // generates:
    * // expr match {
    * //   case aName: A =>
    * //     ... // : Out
    * //   case bName: B =>
    * //     ... //: Out
    * //   ...
    * // } // : Out
    * expr.matchOn(
    *   MatchCase.typeMatch[A]("aName").map { a: Expr[A] =>
    *     // use a
    *     createExprOut(a): Expr[Out]
    *   },
    *   MatchCase.typeMatch[B]("bName").map { b: Expr[B] =>
    *     // use b
    *     createExprOut(b): Expr[Out]
    *   },
    *   ...
    * )
    * }}}
    *
    * @since 0.1.0
    */
  type MatchCase[A]

  val MatchCase: MatchCaseModule
  trait MatchCaseModule { this: MatchCase.type =>

    def typeMatch[A: Type](freshName: FreshName = FreshName.FromType): MatchCase[Expr[A]]

    def eqValue[A: Type](expr: Expr[A], freshName: FreshName = FreshName.FromExpr): MatchCase[Expr[A]]

    /** Creates a case that matches the value with a user-provided `scala.reflect.TypeTest` extractor (Scala 3-only).
      *
      * Generates `case name @ tt(_) => ...` where `tt` is the implicit `scala.reflect.TypeTest[A, B]` summoned at the
      * macro expansion point (`A` being the type of the matched value, `B` the type the case narrows it to). Used for
      * union members that a runtime class test cannot discriminate soundly (same/related erasure like
      * `List[Int] | List[String]`, abstract types, type parameters) - the `TypeTest` acts as a total runtime
      * discriminator supplied by the user.
      *
      * Fails (with an `AssertionError`) when no implicit `scala.reflect.TypeTest[A, B]` is in scope; guard calls with
      * [[Type.unionMemberRequiresTypeTest]] + [[Type.directChildren]] (which verifies summonability for unions), or
      * make sure the instance exists. On Scala 2 (which has no `scala.reflect.TypeTest`) this method always fails the
      * same way; guard calls with [[Type.isUnionType]] / [[Type.unionMemberRequiresTypeTest]], which are always `false`
      * on Scala 2.
      *
      * @since 0.4.0
      */
    def typeTestMatch[A: Type, B: Type](freshName: FreshName = FreshName.FromType): MatchCase[Expr[B]]

    def matchOn[A: Type, B: Type](toMatch: Expr[A])(cases: NonEmptyVector[MatchCase[Expr[B]]]): Expr[B]

    def partition[A, B, C](matchCase: MatchCase[A])(f: A => Either[B, C]): Either[MatchCase[B], MatchCase[C]]

    def traverse: fp.Traverse[MatchCase]

    /** DirectStyle instance for MatchCase.
      *
      * '''Safety:''' `MatchCase` wraps exactly one metadata (name + expression) alongside the value. This means:
      *   - `runSafe` '''must''' be called exactly once inside `scoped` — calling it zero times will throw an
      *     `AssertionError`, and calling it more than once will silently discard metadata from earlier calls.
      *   - This is in contrast to types like `Either`/`Option`/`Try` where zero or multiple `runSafe` calls are safe.
      *
      * Because of these constraints, the implicit is placed inside `object unsafe` and requires an explicit import:
      * {{{
      * import MatchCase.unsafe.*
      * }}}
      *
      * @since 0.3.0
      */
    def directStyle: fp.DirectStyle[MatchCase]

    object unsafe {
      implicit final val MatchCaseDirectStyle: fp.DirectStyle[MatchCase] = directStyle
    }
  }
  implicit final val MatchCaseTraverse: fp.Traverse[MatchCase] = MatchCase.traverse

  implicit final class MatchCaseMethods[A](private val matchCase: MatchCase[A]) {

    def partition[B, C](f: A => Either[B, C]): Either[MatchCase[B], MatchCase[C]] =
      MatchCase.partition(matchCase)(f)
  }

  implicit final class MatchClauseMethods[A: Type](private val toMatch: Expr[A]) {

    def matchOn[B: Type](cases: NonEmptyVector[MatchCase[Expr[B]]]): Expr[B] =
      MatchCase.matchOn(toMatch)(cases)

    def matchOn[B: Type](cases: NonEmptyList[MatchCase[Expr[B]]]): Expr[B] =
      matchOn(cases.toNonEmptyVector)

    def matchOn[B: Type](head: MatchCase[Expr[B]], tail: MatchCase[Expr[B]]*): Expr[B] =
      matchOn(NonEmptyVector(head, tail*))
  }

  /** To avoid name clashes, we need to generate them. This enum provides various strategies to generate fresh names.
    *
    * @since 0.1.0
    */
  sealed trait FreshName extends Product with Serializable
  object FreshName {
    case object FromType extends FreshName
    case object FromExpr extends FreshName
    final case class FromPrefix(prefix: String) extends FreshName

    /** Allows passing String instead of `FreshName.FromPrefix(value)`.
      *
      * @since 0.1.0
      */
    implicit def stringToFreshName(prefix: String): FreshName = FromPrefix(prefix)
  }

  /** Stores one or more val/var/lazy val/def definitions, as well as the code that uses them.
    *
    * Allow us to use the val/var/lazy val/def to construct some expression, and then safely close the scope, make sure
    * that the definition that's inaccessible outside of it, won't ve visible outside of it, while returning the value
    * of a whole block.
    *
    * Prior to 0.2.0 it was named `Scoped`.
    *
    * @since 0.1.0
    */
  type ValDefs[A]

  /** Create definitions, like `val`, `var`, `lazy val`, `def`, that should be accessible only inside some scope.
    *
    * Will take care of opening and closing that scope, which makes it easier to e.g. not use the definition before it
    * was defined.
    *
    * {{{
    * ValDefs.createVal(Expr(1), "a").use { (a: Expr[A]) =>
    *   createExprB(a): Expr[B] // use a
    * } // : Expr[B]
    * // a is not accessible here
    * }}}
    *
    * Prior to 0.2.0 it was named `Scoped`.
    *
    * @since 0.1.0
    */
  val ValDefs: ValDefsModule
  trait ValDefsModule { this: ValDefs.type =>

    def createVal[A: Type](
        value: Expr[A],
        // $COVERAGE-OFF$
        freshName: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): ValDefs[Expr[A]]
    def createVar[A: Type](
        initialValue: Expr[A],
        // $COVERAGE-OFF$
        freshName: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): ValDefs[(Expr[A], Expr[A] => Expr[Unit])]
    def createLazy[A: Type](
        value: Expr[A],
        // $COVERAGE-OFF$
        freshName: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): ValDefs[Expr[A]]
    def createDef[A: Type](
        value: Expr[A],
        // $COVERAGE-OFF$
        freshName: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): ValDefs[Expr[A]]

    def partition[A, B, C](scoped: ValDefs[A])(f: A => Either[B, C]): Either[ValDefs[B], ValDefs[C]]

    def closeScope[A](scoped: ValDefs[Expr[A]]): Expr[A]

    def traverse: fp.ApplicativeTraverse[ValDefs]

    /** DirectStyle instance for ValDefs.
      *
      * '''Safety:''' `ValDefs` accumulates definitions, so zero `runSafe` calls produce empty definitions and multiple
      * calls correctly merge them — matching the behavior of `map2`. This instance is safe to use without restrictions.
      *
      * @since 0.3.0
      */
    def directStyle: fp.DirectStyle[ValDefs]
  }
  implicit final val ValDefsTraverse: fp.ApplicativeTraverse[ValDefs] = ValDefs.traverse
  implicit final val ValDefsDirectStyle: fp.DirectStyle[ValDefs] = ValDefs.directStyle

  implicit final class ValDefsMethods[A](private val scoped: ValDefs[A]) {

    def partition[B, C](f: A => Either[B, C]): Either[ValDefs[B], ValDefs[C]] = ValDefs.partition(scoped)(f)

    def close[B](implicit ev: A <:< Expr[B]): Expr[B] = use(ev(_))

    def use[B](f: A => Expr[B]): Expr[B] = ValDefs.closeScope(scoped.map(f))
  }

  /** Allow us to build def, when we might need to call it recursively and we want to aggregate errors.
    *
    * One use case could be building some def, where:
    *   - we want to construct the type class method's body
    *   - we want to reuse the same transformation logic and "cache" it as a def
    *   - we want it to take some arguments
    *   - we want to handle recursive data types
    *   - the whole computation is fallible, and we would like to aggregate "parallel" errors
    *
    * as such case requires us to know the return type at the time of building the def, but also having the reference to
    * the def that will be build. It also becomes impossible to use: `Expr.quotes { def ... =  ... }` approach with
    * direct style (it has no error aggregation and a def is a statement).
    *
    * {{{
    * // Simple case (where builder is not actually needed)
    * ValDefBuilder.of1[A, Return]("myMethod", "a").buildWith { (self: Expr[A] => Expr[Return], a: Expr[A]) =>
    *   // use self and a
    *   createExprB(self, a): Expr[B]
    * } // : ValDefs[Expr[A] => Expr[Return]]
    *
    * // More complex case (where builder is needed to aggregate errors)
    * ValDefBuilder.of1[A, Return]("myMethod2", "a").traverse[MIO, Return] { (self: Expr[A] => Expr[Return], a: Expr[A]) =>
    *   // use a
    *   createExprBOrError(self, a): MIO[Expr[Return]]
    * }.map(_.build) // : MIO[ValDefs[Expr[A] => Expr[Return]]]
    * }}}
    *
    * If we are not taking arguments, do not need recursion nor error aggregation, we can use [[ValDefs]] to create a
    * def.
    *
    * @since 0.2.0
    *
    * @param Signature
    *   def signature, as used by a macro, e.g. `Expr[A] => Expr[Return]`, `Expr[A], Expr[B] => Expr[Return]`, etc.
    * @param Return
    *   return type of the def, as used by a macro, e.g. `Return`, `Return`, etc.
    * @param Value
    *   current value of a builder - when it becomes Expr of Return, we can build the def returning this Expr
    */
  type ValDefBuilder[Signature, Return, Value]

  val ValDefBuilder: ValDefBuilderModule
  trait ValDefBuilderModule { this: ValDefBuilder.type =>

    def ofVal[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Unit]
    def ofVar[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned] => Expr[Unit]]
    def ofLazy[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned]]

    // format: off
    def ofDef0[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned]]
    def ofDef1[A: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[Expr[A] => Expr[Return], Return, (Expr[A] => Expr[Return], Expr[A])]
    def ofDef2[A: Type, B: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B]) => Expr[Return], Return, ((Expr[A], Expr[B]) => Expr[Return], (Expr[A], Expr[B]))]
    def ofDef3[A: Type, B: Type, C: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C]) => Expr[Return], (Expr[A], Expr[B], Expr[C]))]
    def ofDef4[A: Type, B: Type, C: Type, D: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D]))]
    def ofDef5[A: Type, B: Type, C: Type, D: Type, E: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]))]
    def ofDef6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]))]
    def ofDef7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]))]
    def ofDef8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]))]
    def ofDef9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]))]
    def ofDef10[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]))]
    def ofDef11[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]))]
    def ofDef12[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]))]
    def ofDef13[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]))]
    def ofDef14[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]))]
    def ofDef15[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Return: Type](
        freshName: FreshName,
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]))]
    def ofDef16[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]))]
    def ofDef17[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]))]
    def ofDef18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]))]
    def ofDef19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]))]
    def ofDef20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType,
        freshT: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]))]
    def ofDef21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType,
        freshT: FreshName = FreshName.FromType,
        freshU: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]))]
    def ofDef22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Return: Type](
        freshName: FreshName,
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType,
        freshT: FreshName = FreshName.FromType,
        freshU: FreshName = FreshName.FromType,
        freshV: FreshName = FreshName.FromType,
        // $COVERAGE-ON$
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Return], Return, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Return], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]))]
    // format: on

    def build[Signature, Returned](builder: ValDefBuilder[Signature, Returned, Expr[Returned]]): ValDefs[Signature]

    def buildCached[Signature, Returned](
        cache: ValDefsCache,
        key: String,
        builder: ValDefBuilder[Signature, Returned, Expr[Returned]]
    ): ValDefsCache

    def forwardDeclare[Signature, Returned, Value](
        cache: ValDefsCache,
        key: String,
        builder: ValDefBuilder[Signature, Returned, Value]
    ): ValDefsCache

    def isBuilt[Signature, Returned, Value](
        cache: ValDefsCache,
        key: String,
        builder: ValDefBuilder[Signature, Returned, Value]
    ): Boolean

    def partition[Signature, Returned, A, B, C](builder: ValDefBuilder[Signature, Returned, A])(
        f: A => Either[B, C]
    ): Either[ValDefBuilder[Signature, Returned, B], ValDefBuilder[Signature, Returned, C]]

    def traverse[Signature, Returned]: fp.Traverse[ValDefBuilder[Signature, Returned, *]]

    /** DirectStyle instance for ValDefBuilder.
      *
      * '''Safety:''' `ValDefBuilder` wraps exactly one `Mk` (build strategy) alongside the value. This means:
      *   - `runSafe` '''must''' be called exactly once inside `scoped` — calling it zero times will throw an
      *     `AssertionError`, and calling it more than once will silently discard metadata from earlier calls.
      *   - This is in contrast to types like `Either`/`Option`/`Try` where zero or multiple `runSafe` calls are safe.
      *
      * Because of these constraints, the implicit is placed inside `object unsafe` and requires an explicit import:
      * {{{
      * import ValDefBuilder.unsafe.*
      * }}}
      *
      * @since 0.3.0
      */
    def directStyle[Signature, Returned]: fp.DirectStyle[ValDefBuilder[Signature, Returned, *]]

    object unsafe {
      implicit final def ValDefBuilderDirectStyle[Signature, Returned]
          : fp.DirectStyle[ValDefBuilder[Signature, Returned, *]] =
        directStyle
    }
  }
  implicit final def ValDefBuilderTraverse[Signature, Returned]: fp.Traverse[ValDefBuilder[Signature, Returned, *]] =
    ValDefBuilder.traverse

  implicit final class ValDefBuilderMethods[Signature, Returned, A](
      private val builder: ValDefBuilder[Signature, Returned, A]
  ) {

    def partition[B, C](
        f: A => Either[B, C]
    ): Either[ValDefBuilder[Signature, Returned, B], ValDefBuilder[Signature, Returned, C]] =
      ValDefBuilder.partition(builder)(f)

    def buildWith(f: A => Expr[Returned]): ValDefs[Signature] = ValDefBuilder.build(builder.map(f))

    def build(implicit ev: A <:< Expr[Returned]): ValDefs[Signature] = buildWith(ev)

    def forwardDeclare(cache: ValDefsCache, key: String): ValDefsCache =
      ValDefBuilder.forwardDeclare(cache, key, builder)

    def isBuilt(cache: ValDefsCache, key: String): Boolean =
      ValDefBuilder.isBuilt(cache, key, builder)

    def buildCachedWith(cache: ValDefsCache, key: String)(f: A => Expr[Returned]): ValDefsCache =
      ValDefBuilder.buildCached(cache, key, builder.map(f))

    def buildCached(cache: ValDefsCache, key: String)(implicit ev: A <:< Expr[Returned]): ValDefsCache =
      buildCachedWith(cache, key)(ev)
  }

  /** Cache for defs, so that you can avoid computing everything inside nested [[ValDefs]]s when you need to construct
    * multiple defs.
    *
    * Assumes that cached values are distinguished by:
    *   - a key String
    *   - a list of input parameters' types
    *   - a return type
    *
    * We should provide the string key ourselves, to prevent e.g. accidentally creating the same def twice, in 2
    * branches, without noticing (defs would use fresh names, so they would be different)
    *
    * {{{
    * // Simple case (where builder is not actually needed)
    *
    * var cache = ValDefsCache.empty
    *
    * val myMethod = ValDefBuilder.of1[A, Return]("myMethod", "a")
    *
    * // This will let us use cache.get1[A, Return]("myMethod") to call the method
    * // before it's actually built, e.g. inside of if.
    * cache = myMethod.forwardDeclare(cache, "myMethod")
    *
    * myMethod.buildWith { (_, a: Expr[A]) =>
    *   // pass cache and a
    *   createExprB(cache, a): Expr[B]
    * } // : ValDefs[Expr[A] => Expr[Return]]
    * }}}
    *
    * {{{
    * // More complex case (where builder is needed to aggregate errors)
    *
    * val cacheLocal = ValDefsCache.mlocal
    *
    * def createExprBOrError(a): MIO[Return] = ... // using cacheLocal.get1[A, Return]("myMethod2")
    *
    * for {
    *   _ <- MIO.void
    *   myMethod2 = ValDefBuilder.of1[A, Return]("myMethod2", "a")
    *   // This will let us use cacheLocal.get1[A, Return]("myMethod") to call the method
    *   // before it's actually built, e.g. inside of if.
    *   _ <- cacheLocal.forwardDeclare(myMethod2, "myMethod2")
    *   myMethod2WithBody <- myMethod2.traverse[MIO, Return] { (_, a: Expr[A]) =>
    *     // use a, cache would be handled as a "global" variable
    *     createExprBOrError(a): MIO[Expr[Return]]
    *   }
    * } yield myMethod2WithBody.build // : MIO[ValDefs[Expr[A] => Expr[Return]]]
    * }}}
    *
    * @since 0.2.0
    */
  type ValDefsCache

  val ValDefsCache: ValDefsCacheModule
  trait ValDefsCacheModule { this: ValDefsCache.type =>

    def empty: ValDefsCache

    def mlocal: fp.effect.MLocal[ValDefsCache] = fp.effect.MLocal.unsafeSharedParallel(empty)

    // format: off
    def get0Ary[Returned: Type](cache: ValDefsCache, key: String): Option[Expr[Returned]]
    def get1Ary[A: Type, Returned: Type](cache: ValDefsCache, key: String): Option[Expr[A] => Expr[Returned]]
    def get2Ary[A: Type, B: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B]) => Expr[Returned]]
    def get3Ary[A: Type, B: Type, C: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C]) => Expr[Returned]]
    def get4Ary[A: Type, B: Type, C: Type, D: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned]]
    def get5Ary[A: Type, B: Type, C: Type, D: Type, E: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned]]
    def get6Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned]]
    def get7Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned]]
    def get8Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned]]
    def get9Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned]]
    def get10Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned]]
    def get11Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned]]
    def get12Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned]]
    def get13Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned]]
    def get14Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned]]
    def get15Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned]]
    def get16Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned]]
    def get17Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned]]
    def get18Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned]]
    def get19Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned]]
    def get20Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned]]
    def get21Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned]]
    def get22Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned]]
    // format: on

    def toValDefs(cache: ValDefsCache): ValDefs[Unit]
  }
  implicit final class ValDefsCacheMethods(private val cache: ValDefsCache) {

    // format: off
    def get0Ary[Returned: Type](key: String): Option[Expr[Returned]] = ValDefsCache.get0Ary(cache, key)
    def get1Ary[A: Type, Returned: Type](key: String): Option[Expr[A] => Expr[Returned]] = ValDefsCache.get1Ary(cache, key)
    def get2Ary[A: Type, B: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B]) => Expr[Returned]] = ValDefsCache.get2Ary(cache, key)
    def get3Ary[A: Type, B: Type, C: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C]) => Expr[Returned]] = ValDefsCache.get3Ary(cache, key)
    def get4Ary[A: Type, B: Type, C: Type, D: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned]] = ValDefsCache.get4Ary(cache, key)
    def get5Ary[A: Type, B: Type, C: Type, D: Type, E: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned]] = ValDefsCache.get5Ary(cache, key)
    def get6Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned]] = ValDefsCache.get6Ary(cache, key)
    def get7Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned]] = ValDefsCache.get7Ary(cache, key)
    def get8Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned]] = ValDefsCache.get8Ary(cache, key)
    def get9Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned]] = ValDefsCache.get9Ary(cache, key)
    def get10Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned]] = ValDefsCache.get10Ary(cache, key)
    def get11Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned]] = ValDefsCache.get11Ary(cache, key)
    def get12Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned]] = ValDefsCache.get12Ary(cache, key)
    def get13Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned]] = ValDefsCache.get13Ary(cache, key)
    def get14Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned]] = ValDefsCache.get14Ary(cache, key)
    def get15Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned]] = ValDefsCache.get15Ary(cache, key)
    def get16Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned]] = ValDefsCache.get16Ary(cache, key)
    def get17Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned]] = ValDefsCache.get17Ary(cache, key)
    def get18Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned]] = ValDefsCache.get18Ary(cache, key)
    def get19Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned]] = ValDefsCache.get19Ary(cache, key)
    def get20Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned]] = ValDefsCache.get20Ary(cache, key)
    def get21Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned]] = ValDefsCache.get21Ary(cache, key)
    def get22Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Returned: Type](key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned]] = ValDefsCache.get22Ary(cache, key)
    // format: on

    def toValDefs: ValDefs[Unit] = ValDefsCache.toValDefs(cache)
  }

  /** Allow us to build Lambda, when we don't know the return type and we want to aggregate errors.
    *
    * One use case could be building some lambda, where:
    *   - depending on available implicits, the final result would have a different return type
    *   - the whole computation is fallible, and we would like to aggregate "parallel" errors
    *
    * as such case makes it impossible to use: `(Expr[A], Expr[B], ...) => { ... }` approach (unknown return type) with
    * direct style (it has no error aggregation).
    *
    * {{{
    * // Simple case (where builder is not actually needed)
    * LambdaBuilder.of1[A]("a").buildWith { (a: Expr[A]) =>
    *   // use a
    *   createExprB(a): Expr[B]
    * } // : Expr[A => B]
    *
    * // More complex case (where builder is needed to aggregate errors)
    * LambdaBuilder.of1[A]("a").traverse[MIO, B] { (a: Expr[A]) =>
    *   // use a
    *   createExprBOrError(a): MIO[Expr[B]]
    * }.map(_.build) // : MIO[Expr[A => B]]
    * }}}
    *
    * If we don't need to aggregate errors, we can use direct-style to combine partial results inside the lambda body.
    *
    * {{{
    * Expr.quote { (a: A) => Expr.unquote(createExprB(Expr.quote(a))) } // : Expr[A => B]
    * }}}
    *
    * If we are not building a lambda, but a normal expression we can use [[ValDefs]] with Applicative/Parallel/Traverse
    * combinators.
    *
    * @since 0.1.0
    *
    * @param From
    *   lambda signature without return type (e.g. `A => *`, `(A, B) => *`, ...) - if we just used A, (A, B), (A, B, C),
    *   we would define all lambdas as tupled, and we don't want that
    * @param To
    *   current result - when we reach the point that it's Expr of sth, we can build the lambda returning this Expr
    */
  type LambdaBuilder[From[_], To]

  val LambdaBuilder: LambdaBuilderModule
  trait LambdaBuilderModule { this: LambdaBuilder.type =>

    // format: off
    def of1[A: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[A => *, Expr[A]]
    def of2[A: Type, B: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])]
    def of3[A: Type, B: Type, C: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C) => *, (Expr[A], Expr[B], Expr[C])]
    def of4[A: Type, B: Type, C: Type, D: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D) => *, (Expr[A], Expr[B], Expr[C], Expr[D])]
    def of5[A: Type, B: Type, C: Type, D: Type, E: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E])]
    def of6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F])]
    def of7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G])]
    def of8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H])]
    def of9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I])]
    def of10[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J])]
    def of11[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K])]
    def of12[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L])]
    def of13[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M])]
    def of14[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N])]
    def of15[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O])]
    def of16[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P])]
    def of17[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q])]
    def of18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R])]
    def of19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S])]
    def of20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType,
        freshT: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T])]
    def of21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType,
        freshT: FreshName = FreshName.FromType,
        freshU: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U])]
    def of22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type](
        // $COVERAGE-OFF$
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType,
        freshJ: FreshName = FreshName.FromType,
        freshK: FreshName = FreshName.FromType,
        freshL: FreshName = FreshName.FromType,
        freshM: FreshName = FreshName.FromType,
        freshN: FreshName = FreshName.FromType,
        freshO: FreshName = FreshName.FromType,
        freshP: FreshName = FreshName.FromType,
        freshQ: FreshName = FreshName.FromType,
        freshR: FreshName = FreshName.FromType,
        freshS: FreshName = FreshName.FromType,
        freshT: FreshName = FreshName.FromType,
        freshU: FreshName = FreshName.FromType,
        freshV: FreshName = FreshName.FromType
        // $COVERAGE-ON$
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V])]
    // format: on

    def build[From[_], To: Type](builder: LambdaBuilder[From, Expr[To]]): Expr[From[To]]

    def partition[From[_], A, B, C](builder: LambdaBuilder[From, A])(
        f: A => Either[B, C]
    ): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]]

    def traverse[From[_]]: fp.Traverse[LambdaBuilder[From, *]]

    /** DirectStyle instance for LambdaBuilder.
      *
      * '''Safety:''' `LambdaBuilder` wraps exactly one `Mk` (build strategy) alongside the value. This means:
      *   - `runSafe` '''must''' be called exactly once inside `scoped` — calling it zero times will throw an
      *     `AssertionError`, and calling it more than once will silently discard metadata from earlier calls.
      *   - This is in contrast to types like `Either`/`Option`/`Try` where zero or multiple `runSafe` calls are safe.
      *
      * Because of these constraints, the implicit is placed inside `object unsafe` and requires an explicit import:
      * {{{
      * import LambdaBuilder.unsafe.*
      * }}}
      *
      * @since 0.3.0
      */
    def directStyle[From[_]]: fp.DirectStyle[LambdaBuilder[From, *]]

    object unsafe {
      implicit final def LambdaBuilderDirectStyle[From[_]]: fp.DirectStyle[LambdaBuilder[From, *]] = directStyle
    }
  }
  implicit final def LambdaBuilderTraverse[From[_]]: fp.Traverse[LambdaBuilder[From, *]] = LambdaBuilder.traverse

  implicit final class LambdaBuilderMethods[From[_], A](private val builder: LambdaBuilder[From, A]) {

    def partition[B, C](f: A => Either[B, C]): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]] =
      LambdaBuilder.partition(builder)(f)

    def buildWith[To: Type](f: A => Expr[To]): Expr[From[To]] = LambdaBuilder.build(builder.map(f))

    def build[To: Type](implicit ev: A <:< Expr[To]): Expr[From[To]] = buildWith(ev)
  }

  // --- Expression destructuring ---

  /** A decomposed expression representing the high-level intent of a compiler AST.
    *
    * Unlike a raw AST mirror, `DestructuredExpr` captures **semantic** structure: a [[DestructuredExpr.MethodCall]]
    * holds a resolved [[Method]] from Hearth's API plus an ordered list of what was applied (instance, type args, value
    * args). This maps directly to [[Method.fold]] for reconstruction.
    *
    * Parsing is total and maximal: every sub-expression is recursively destructured; only the smallest unresolvable
    * leaf becomes [[DestructuredExpr.NonDestructurable]]. Compiler noise (Scala 3 `Inlined` wrappers, etc.) is stripped
    * automatically.
    *
    * '''Recovering a singleton / enum value (the `Type.Ctor` idiom).''' To pull a singleton or (Java/Scala) enum value
    * back out of a destructured expression, collect the [[DestructuredExpr.Singleton]] nodes (its `name` is the module
    * path) or match a node's `tpe` against a `Type.Ctor` extractor. Beware: on Scala 2 the typer WIDENS singleton
    * types, so a lambda parameter ascribed `Color.Black.type` shows up as `Color` in `Lambda.Param.tpe`; use
    * [[DestructuredExpr.Lambda.Param]]'s `declaredTpe` (the syntactic type, singleton preserved — hearth#341; on Scala
    * 3 the two are equal) when you need to `Type.Ctor`-match a selected singleton:
    *
    * {{{
    * // e.g. lambda `(c: Color.Black.type) => ...` fed through the fixtures:
    * DestructuredExpr.extractLambda(expr) match {
    *   case Right(info) =>
    *     info.params.map(_.declaredTpe.plainPrint) // "Color.Black.type" (not the widened "Color")
    *   case Left(error) => // ...
    * }
    *
    * // Collect every singleton/enum reference in a parsed tree:
    * DestructuredExpr.parse(expr).collect {
    *   case s: DestructuredExpr.Singleton => s.name // "None", "Nil", "Color.Black", ...
    * }
    * }}}
    *
    * @see
    *   [[DestructuredExpr.Lambda.Param]] (`tpe` vs `declaredTpe`) for the widened-vs-syntactic parameter type
    *   distinction
    * @since 0.4.0
    */
  sealed trait DestructuredExpr {

    val tpe: ??

    def toUntypedExpr: UntypedExpr

    def plainPrint: String

    final def children: List[DestructuredExpr] = this match {
      case _: DestructuredExpr.Literal           => Nil
      case _: DestructuredExpr.Singleton         => Nil
      case _: DestructuredExpr.NonDestructurable => Nil
      case _: DestructuredExpr.Lambda.ParamRef   => Nil
      case mc: DestructuredExpr.MethodCall       =>
        mc.applied.flatMap {
          case ai: DestructuredExpr.MethodCall.AppliedInstance => List(ai.value)
          case _: DestructuredExpr.MethodCall.AppliedTypes     => Nil
          case av: DestructuredExpr.MethodCall.AppliedValues   => av.args
        }
      case lam: DestructuredExpr.Lambda => List(lam.body)
      case b: DestructuredExpr.Block    => b.statements :+ b.result
      case va: DestructuredExpr.Varargs => va.elements
    }

    final def collect[B](pf: PartialFunction[DestructuredExpr, B]): List[B] = {
      val buffer = new scala.collection.mutable.ListBuffer[B]
      def go(node: DestructuredExpr): Unit = {
        if (pf.isDefinedAt(node)) buffer += pf(node)
        node.children.foreach(go)
      }
      go(this)
      buffer.result()
    }
  }
  object DestructuredExpr {

    /** Parse a typed expression into a [[DestructuredExpr]].
      *
      * Start here, then either pattern-match on the node types (`MethodCall`, `Lambda`, `Singleton`, `Literal`,
      * `Block`, `Varargs`, `NonDestructurable`) or use the `extract*` helpers ([[extractFieldPath]],
      * [[extractLambda]]). To recover a singleton / enum value, collect [[Singleton]] nodes or `Type.Ctor`-match a
      * node's `tpe` — see the [[DestructuredExpr]] trait doc for the worked idiom (and the `declaredTpe` caveat on
      * Scala 2).
      *
      * @param expr
      *   the typed expression to destructure
      * @return
      *   the semantic decomposition of `expr`
      * @see
      *   [[DestructuredExpr]] for the node types and the singleton/enum-extraction example
      * @since 0.4.0
      */
    final def parse[A: Type](expr: Expr[A]): DestructuredExpr = destructureExpr(UntypedExpr.fromTyped(expr))

    /** Parse an untyped expression into a [[DestructuredExpr]].
      *
      * @since 0.4.0
      */
    final def parseUntyped(expr: UntypedExpr): DestructuredExpr = destructureExpr(expr)

    /** Extract a field access path from a lambda like `_.field1.field2`.
      *
      * Walks the [[MethodCall]] chain: each call with only an [[MethodCall.AppliedInstance]] and no value/type args is
      * a field access step.
      *
      * @since 0.4.0
      */
    final def extractFieldPath[A: Type, B: Type](lambda: Expr[A => B]): Either[String, FieldPath] = {
      val parsed = parseUntyped(UntypedExpr.fromTyped(lambda))
      parsed match {
        case lam: Lambda =>
          lam.params match {
            case List(param) =>
              collectFieldSteps(lam.body, param).flatMap {
                case Nil      => Left("Empty field path - the lambda body must access at least one field")
                case segments => Right(new FieldPath(Type[A].as_??, segments))
              }
            case params => Left(s"Expected a single-parameter lambda, got ${params.size} parameters")
          }
        case _ =>
          Left(s"Expected a lambda expression, got ${parsed.plainPrint}")
      }
    }

    private def collectFieldSteps(
        expr: DestructuredExpr,
        rootParam: Lambda.Param
    ): Either[String, List[FieldPathSegment]] = expr match {
      case mc: MethodCall =>
        mc.applied match {
          case List(ai: MethodCall.AppliedInstance) =>
            val segment = new FieldPathSegment(mc.method.name, ai.value.tpe, mc.tpe, mc.method)
            collectFieldSteps(ai.value, rootParam).map(_ :+ segment)
          case _ =>
            Left(s"Expected a field access (method with only an instance), got ${expr.plainPrint}")
        }
      case ref: Lambda.ParamRef if ref.param eq rootParam =>
        Right(Nil)
      case _ =>
        Left(s"Expected a field access chain on the lambda parameter, got ${expr.plainPrint}")
    }

    /** Extract lambda parameters and body from an expression.
      *
      * @since 0.4.0
      */
    final def extractLambda[A: Type](expr: Expr[A]): Either[String, LambdaInfo] = {
      val parsed = parse[A](expr)
      parsed match {
        case lam: Lambda =>
          Right(new LambdaInfo(lam.params, lam.body))
        case _ =>
          Left(s"Expected a lambda expression, got ${parsed.plainPrint}")
      }
    }

    // --- Node types ---

    /** A resolved method/field call with its applied arguments.
      *
      * `method` is the resolved [[Method]] from Hearth's API. `applied` mirrors `method.expectations` in order — each
      * entry corresponds to one step of the [[Method]] builder chain. To reconstruct, walk `method.fold` feeding each
      * [[MethodCall.Applied]] entry.
      *
      * @since 0.4.0
      */
    final class MethodCall private[hearth] (
        val tpe: ??,
        val method: Method,
        val applied: List[MethodCall.Applied],
        private[hearth] val rebuild: () => UntypedExpr
    ) extends DestructuredExpr {
      def toUntypedExpr: UntypedExpr = rebuild()
      def plainPrint: String = {
        val parts = applied.map {
          case ai: MethodCall.AppliedInstance => ai.value.plainPrint
          case at: MethodCall.AppliedTypes    => at.typeArgs.map(_.plainPrint).mkString("[", ", ", "]")
          case av: MethodCall.AppliedValues   => av.args.map(_.plainPrint).mkString("(", ", ", ")")
        }
        s"${method.name}${parts.mkString}"
      }
    }
    object MethodCall {

      /** One step of an applied method call, mapping to [[Method]] builder chain steps.
        *
        * @since 0.4.0
        */
      sealed trait Applied

      /** Instance expression (receiver). Maps to [[Method.OnInstance]].
        *
        * @since 0.4.0
        */
      final class AppliedInstance private[hearth] (val value: DestructuredExpr) extends Applied

      /** Type arguments. Maps to [[Method.ApplyTypes]].
        *
        * @since 0.4.0
        */
      final class AppliedTypes private[hearth] (val typeArgs: List[??]) extends Applied

      /** Value arguments for one parameter clause. Maps to [[Method.ApplyValues]].
        *
        * @since 0.4.0
        */
      final class AppliedValues private[hearth] (val args: List[DestructuredExpr]) extends Applied
    }

    /** A lambda expression: `(params) => body`.
      *
      * @since 0.4.0
      */
    final class Lambda private[hearth] (
        val tpe: ??,
        val params: List[Lambda.Param],
        val body: DestructuredExpr,
        private[hearth] val rebuild: () => UntypedExpr
    ) extends DestructuredExpr {
      def toUntypedExpr: UntypedExpr = rebuild()
      def plainPrint: String = {
        val ps = params.map(p => s"${p.name}: ${p.tpe.plainPrint}").mkString(", ")
        s"($ps) => ${body.plainPrint}"
      }
    }
    object Lambda {

      /** A lambda parameter.
        *
        * @param tpe
        *   the parameter type as seen by the compiler — WIDENED on Scala 2 (the typer widens singleton parameter types,
        *   so e.g. a Java-enum-value ascription `Color.Black.type` shows as `Color`).
        * @param declaredTpe
        *   the DECLARED / syntactic parameter type, with singletons preserved (from `tpt.original` on Scala 2); use
        *   this to recover e.g. a selected Java-enum value via `Type.Ctor` matching. On Scala 3 the compiler already
        *   retains the precise type, so `declaredTpe == tpe` there. See hearth#341.
        *
        * @since 0.4.0
        */
      final class Param private[hearth] (val name: String, val tpe: ??, val declaredTpe: ??) {
        private[hearth] def this(name: String, tpe: ??) = this(name, tpe, tpe)
      }

      /** Reference to a lambda parameter in the body.
        *
        * @since 0.4.0
        */
      final class ParamRef private[hearth] (
          val tpe: ??,
          val param: Param,
          private[hearth] val rebuild: () => UntypedExpr
      ) extends DestructuredExpr {
        def toUntypedExpr: UntypedExpr = rebuild()
        def plainPrint: String = param.name
      }
    }

    /** A literal constant: `42`, `"hello"`, `true`, `null`, `()`, etc.
      *
      * @since 0.4.0
      */
    final class Literal private[hearth] (
        val tpe: ??,
        val value: Any,
        private[hearth] val rebuild: () => UntypedExpr
    ) extends DestructuredExpr {
      def toUntypedExpr: UntypedExpr = rebuild()
      def plainPrint: String = value match {
        case s: String => s"\"$s\""
        case null      => "null"
        case ()        => "()"
        case c: Char   => s"'$c'"
        case l: Long   => s"${l}L"
        case f: Float  => s"${f}f"
        case other     => other.toString
      }
    }

    /** A singleton/module reference: `None`, `Nil`, companion objects.
      *
      * @since 0.4.0
      */
    final class Singleton private[hearth] (
        val tpe: ??,
        val name: String,
        private[hearth] val rebuild: () => UntypedExpr
    ) extends DestructuredExpr {
      def toUntypedExpr: UntypedExpr = rebuild()
      def plainPrint: String = name
    }

    /** A block: `{ stmt1; stmt2; ...; result }`.
      *
      * @since 0.4.0
      */
    final class Block private[hearth] (
        val tpe: ??,
        val statements: List[DestructuredExpr],
        val result: DestructuredExpr,
        private[hearth] val rebuild: () => UntypedExpr
    ) extends DestructuredExpr {
      def toUntypedExpr: UntypedExpr = rebuild()
      def plainPrint: String = {
        val stmts = (statements.map(_.plainPrint) :+ result.plainPrint).mkString("; ")
        s"{ $stmts }"
      }
    }

    /** A vararg (repeated) argument slot filled with individual elements: `method(a, b, c)` where the parameter is
      * declared as `xs: A*`.
      *
      * The whole slot is represented as **one** argument inside [[MethodCall.AppliedValues]], mirroring how
      * [[Method.ApplyValues]] expects a single `Expr[Seq[A]]` argument for a vararg parameter (see
      * [[Parameter.isVararg]]). [[tpe]] is the normalized `scala.collection.immutable.Seq[A]` on both platforms, and
      * the individual element expressions stay recoverable through [[elements]].
      *
      * Note: when the call site spreads an existing sequence (`method(seq*)`), there is no `Varargs` node — the slot is
      * simply the destructured sequence expression itself (already of type `Seq[A]`).
      *
      * [[toUntypedExpr]] synthesizes a `scala.collection.immutable.Seq(elements*)` expression, since the raw repeated
      * argument tree is not a valid standalone expression.
      *
      * @since 0.4.0
      */
    final class Varargs private[hearth] (
        val tpe: ??,
        val elements: List[DestructuredExpr],
        private[hearth] val rebuild: () => UntypedExpr
    ) extends DestructuredExpr {
      def toUntypedExpr: UntypedExpr = rebuild()
      def plainPrint: String = elements.map(_.plainPrint).mkString(", ")
    }

    /** A sub-expression that could not be destructured into a semantic node.
      *
      * @since 0.4.0
      */
    final class NonDestructurable private[hearth] (
        val tpe: ??,
        val raw: UntypedExpr,
        val description: String
    ) extends DestructuredExpr {
      def toUntypedExpr: UntypedExpr = raw
      def plainPrint: String = s"<non-destructurable: $description>"
    }
  }

  protected def destructureExpr(expr: UntypedExpr): DestructuredExpr

  /** A single step in a field access path, resolved against the [[Method]] API.
    *
    * @since 0.4.0
    */
  final class FieldPathSegment(
      val name: String,
      val sourceType: ??,
      val resultType: ??,
      val method: Method
  )

  /** A parsed field access path extracted from a lambda like `_.a.b.c`.
    *
    * @since 0.4.0
    */
  final class FieldPath(
      val root: ??,
      val segments: List[FieldPathSegment]
  ) {

    final def fieldNames: List[String] = segments.map(_.name)

    final def leafType: ?? = segments.last.resultType

    final def depth: Int = segments.size

    final def plainPrint: String = fieldNames.mkString("_.", ".", "")
  }

  /** Decomposed lambda information: parameters and body.
    *
    * @since 0.4.0
    */
  final class LambdaInfo(
      val params: List[DestructuredExpr.Lambda.Param],
      val body: DestructuredExpr
  )

  /** Utilities for working with annotation expressions returned by `Type.annotations`, `Method.annotations` and
    * `Parameter.annotations`.
    *
    * Annotation expressions are constructor invocations (`new MyAnnotation(args...)`), so their arguments can be
    * recovered structurally via [[DestructuredExpr]] - `constructorArguments` does exactly that. Combined with the
    * `annotationsOfType`/`hasAnnotationOfType` filters this allows reading annotations without dropping to
    * platform-specific APIs:
    *
    * {{{
    * // Read the Int argument of the first @MyAnnotation(n) on type A:
    * val n: Option[Int] = Type.annotationsOfType[A, MyAnnotation].headOption
    *   .flatMap(ann => Annotations.constructorArguments(ann))
    *   .flatMap(_.headOption)
    *   .flatMap { arg =>
    *     import arg.Underlying as ArgType
    *     arg.value.semiEval.toOption.collect { case n: Int => n }
    *   }
    * }}}
    *
    * @since 0.4.0
    */
  object Annotations {

    /** Filters annotation expressions to those whose type is a subtype of `Ann`, typing each result as `Expr[Ann]`.
      *
      * Subtype (`<:<`) matching is used (rather than `=:=`) so that a whole annotation hierarchy can be matched by its
      * common base type. The upcast is safe because each annotation expression carries its exact type.
      *
      * @since 0.4.0
      */
    def filterOfType[Ann: Type](annotations: List[Expr_??]): List[Expr[Ann]] =
      annotations.collect {
        case ann if ann.Underlying <:< Type[Ann] =>
          import ann.Underlying as ExactAnn
          ann.value.upcast[Ann]
      }

    /** Extracts the constructor arguments of an annotation expression (`new MyAnnotation(args...)`).
      *
      * Returns the argument expressions flattened across all parameter lists, in order, each with its exact type
      * (`Some(Nil)` for no-argument annotations). Returns `None` if the expression is not a constructor invocation.
      *
      * Individual arguments can then be decoded with `arg.value.semiEval` (after `import arg.Underlying`) or via
      * [[ExprCodec]]-based `Expr.unapply`.
      *
      * @since 0.4.0
      */
    def constructorArguments(annotation: Expr_??): Option[List[Expr_??]] =
      constructorArguments(annotation.value)

    /** Extracts the constructor arguments of an annotation expression (`new MyAnnotation(args...)`).
      *
      * Variant of the `Expr_??` overload for already-typed annotation expressions, e.g. those returned by
      * `annotationsOfType`.
      *
      * @since 0.4.0
      */
    def constructorArguments[A](annotation: Expr[A]): Option[List[Expr_??]] =
      DestructuredExpr.parseUntyped(UntypedExpr.fromTyped(annotation)) match {
        case mc: DestructuredExpr.MethodCall if mc.method.isConstructor =>
          Some(
            mc.applied
              .collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }
              .flatten
              .map { arg =>
                import arg.tpe.Underlying as ArgType
                arg.toUntypedExpr.asTyped[ArgType].as_??
              }
          )
        case _ => None
      }

    /** Extracts the constructor arguments of an annotation expression and decodes each to its runtime value.
      *
      * Convenience over `constructorArguments` + `semiEval`: each argument is evaluated at macro time, so literal
      * arguments (e.g. the `"first_name"` in `@fieldName("first_name")`) come back as plain values. Arguments that
      * cannot be evaluated are returned as `Left` with the evaluation errors, without failing the whole list.
      *
      * Returns `None` if the expression is not a constructor invocation. Example:
      *
      * {{{
      * // Read the String argument of the first @fieldName(...) on a constructor parameter:
      * val name: Option[String] = param.annotationsOfType[fieldName].headOption
      *   .flatMap(ann => Annotations.decodedConstructorArguments(ann))
      *   .flatMap(_.headOption.flatMap(_.toOption))
      *   .collect { case s: String => s }
      * }}}
      *
      * @since 0.4.0
      */
    def decodedConstructorArguments(annotation: Expr_??): Option[List[Either[String, Any]]] =
      decodedConstructorArguments(annotation.value)

    /** Extracts the constructor arguments of an annotation expression and decodes each to its runtime value.
      *
      * Variant of the `Expr_??` overload for already-typed annotation expressions, e.g. those returned by
      * `annotationsOfType`.
      *
      * @since 0.4.0
      */
    def decodedConstructorArguments[A](annotation: Expr[A]): Option[List[Either[String, Any]]] =
      constructorArguments(annotation).map(_.map { arg =>
        (arg.value.semiEval: Either[NonEmptyVector[String], Any]).left.map(_.mkString(", "))
      })
  }
}
