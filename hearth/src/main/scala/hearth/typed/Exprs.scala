package hearth
package typed

import hearth.fp.data.{NonEmptyList, NonEmptyVector}
import hearth.fp.syntax.*

import scala.language.implicitConversions

trait Exprs extends ExprsCrossQuotes { this: MacroCommons =>

  /** Platform-specific untyped type representation (`c.Expr[A]` in 2, `scala.quoted.Expr[A]` in 3).
    *
    * Typed [[Expr]] and [[UntypedExpr]] exist because some macro operations do not require the full knowledge about the
    * type (because they operate on Symbols, and we would have to convert from the typed representation to Symbols to
    * use them), and some require the full knowledge about the type (because e.g. type parameters from the class has to
    * be applied to its methods and their arguments/returned values).
    *
    * The implementation will use the right underlying representation to perform the operations, and where possible
    * convert between typed and untyped representations, but the distinction would be useful in cases where some
    * operations are only available to only one of them. Then user could covert between them in the context where the
    * missing information is available.
    *
    * Note that existential type [[Expr_??]], is not an [[UntypedExpr]] - it's a typed representaiton, where the macro
    * during **its excution** would know the exact type BUT it's inconvenient for us to use generics to represent that
    * exact type during compilation of the macro itself (not it's expansion).
    *
    * @since 0.1.0
    */
  type Expr[A]

  val Expr: ExprModule
  trait ExprModule extends ExprCrossQuotes { this: Expr.type =>

    def apply[A: ExprCodec](value: A): Expr[A] = ExprCodec[A].toExpr(value)

    def unapply[A: ExprCodec](expr: Expr[A]): Option[A] = ExprCodec[A].fromExpr(expr)

    def plainPrint[A](expr: Expr[A]): String = removeAnsiColors(prettyPrint(expr))
    def prettyPrint[A](expr: Expr[A]): String
    def plainAST[A](expr: Expr[A]): String = removeAnsiColors(prettyAST(expr))
    def prettyAST[A](expr: Expr[A]): String

    def summonImplicit[A: Type]: Option[Expr[A]]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit]

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

    // TODO: def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]] = ???
    // TODO: def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]] = ???

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

    def DataExprCodec: ExprCodec[data.Data] = new ExprCodec[data.Data] {

      val DataType: Type[data.Data] = Type.of[data.Data]

      val StringType: Type[String] = Type.of[String]

      def toExpr(value: data.Data): Expr[data.Data] = value.fold(
        onNull = Expr.quote(data.Data()),
        onInt = i => {
          val inner: Expr[Int] = Expr(i)
          Expr.quote(data.Data(Expr.splice(inner)))
        },
        onLong = l => {
          val inner: Expr[Long] = Expr(l)
          Expr.quote(data.Data(Expr.splice(inner)))
        },
        onFloat = f => {
          val inner: Expr[Float] = Expr(f)
          Expr.quote(data.Data(Expr.splice(inner)))
        },
        onDouble = d => {
          val inner: Expr[Double] = Expr(d)
          Expr.quote(data.Data(Expr.splice(inner)))
        },
        onBoolean = b => {
          val inner: Expr[Boolean] = Expr(b)
          Expr.quote(data.Data(Expr.splice(inner)))
        },
        onString = s => {
          val inner: Expr[String] = Expr(s)
          Expr.quote(data.Data(Expr.splice(inner)))
        },
        onList = l => {
          implicit val dt: Type[data.Data] = DataType
          val inner: Expr[List[data.Data]] = Expr(l)
          Expr.quote(data.Data((Expr.splice(inner))))
        },
        onMap = m => {
          implicit val st: Type[String] = StringType
          implicit val dt: Type[data.Data] = DataType
          val inner: Expr[Map[String, data.Data]] = Expr(m)
          Expr.quote(data.Data(Expr.splice(inner)))
        }
      )
      def fromExpr(expr: Expr[data.Data]): Option[data.Data] = None // TODO
    }
  }

  implicit final class ExprMethods[A](private val expr: Expr[A]) {

    def value(implicit codec: ExprCodec[A]): Option[A] = codec.fromExpr(expr)

    def plainPrint: String = Expr.plainPrint(expr)
    def prettyPrint: String = Expr.prettyPrint(expr)
    def plainAST: String = Expr.plainAST(expr)
    def prettyAST: String = Expr.prettyAST(expr)

    def upcast[B](implicit A: Type[A], B: Type[B]): Expr[B] = Expr.upcast(expr)
    def suppressUnused(implicit A: Type[A]): Expr[Unit] = Expr.suppressUnused(expr)

    def asUntyped: UntypedExpr = UntypedExpr.fromTyped(expr)

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

  implicit def ExistentialExprMethods(expr: Expr_??): BoundedExistentialExprMethods[Nothing, Any] =
    new BoundedExistentialExprMethods[Nothing, Any](expr)
  implicit def LowerBoundedExistentialExprMethods[L](expr: Expr_??>:[L]): BoundedExistentialExprMethods[L, Any] =
    new BoundedExistentialExprMethods[L, Any](expr)
  implicit def UpperBoundedExistentialExprMethods[U](expr: Expr_??<:[U]): BoundedExistentialExprMethods[Nothing, U] =
    new BoundedExistentialExprMethods[Nothing, U](expr)
  implicit final class BoundedExistentialExprMethods[L, U >: L](private val expr: Expr_<:??<:[L, U]) {

    def plainPrint: String = Expr.plainPrint(expr.value)
    def prettyPrint: String = Expr.prettyPrint(expr.value)
    def plainAST: String = Expr.plainAST(expr.value)
    def prettyAST: String = Expr.prettyAST(expr.value)

    def asUntyped: UntypedExpr = UntypedExpr.fromTyped(expr.value)
  }

  /** Generalizes over type classes for converting values to and from `Expr`s.
    *
    *   - `Lifting`/`Unlifting` on Scala 2
    *   - `ToExpr`/`FromExpr` on Scala 3
    *
    * @see
    *   [[https://docs.scala-lang.org/overviews/quasiquotes/lifting.html]] for Scala 2 underlying concept
    * @see
    *   [[https://docs.scala-lang.org/scala3/guides/macros/macros.html#creating-expression-from-values]] for Scala 3
    *   `ToExpr`
    * @see
    *   [[https://docs.scala-lang.org/scala3/guides/macros/macros.html#extracting-values-from-expressions]] for Scala 3
    *   `FromExpr`
    *
    * @since 0.1.0
    */
  trait ExprCodec[A] {

    def toExpr(value: A): Expr[A]
    def fromExpr(expr: Expr[A]): Option[A]
  }
  object ExprCodec {

    def apply[A](implicit codec: ExprCodec[A]): ExprCodec[A] = codec

    // TODO: implement all these:
    // TODO: - https://github.com/scala/scala/blob/master/src/reflect/scala/reflect/api/StandardLiftables.scala
    // TODO: - https://github.com/scala/scala3/blob/master/library/src/scala/quoted/ToExpr.scala
    // TODO: https://github.com/scala/scala3/blob/master/library/src/scala/quoted/FromExpr.scala
    // TODO: derivation?

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

    // implicit def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]] = Expr.ClassExprCodec[A]
    // implicit def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]] = Expr.ClassTagExprCodec[A]

    implicit def ArrayExprCodec[A: ExprCodec: Type]: ExprCodec[Array[A]] = Expr.ArrayExprCodec[A]
    implicit def SeqExprCodec[A: ExprCodec: Type]: ExprCodec[Seq[A]] = Expr.SeqExprCodec[A]
    implicit def ListExprCodec[A: ExprCodec: Type]: ExprCodec[List[A]] = Expr.ListExprCodec[A]
    implicit lazy val NilExprCodec: ExprCodec[Nil.type] = Expr.NilExprCodec
    implicit def VectorExprCodec[A: ExprCodec: Type]: ExprCodec[Vector[A]] = Expr.VectorExprCodec[A]
    implicit def MapExprCodec[K: ExprCodec: Type, V: ExprCodec: Type]: ExprCodec[Map[K, V]] = Expr.MapExprCodec[K, V]
    implicit def SetExprCodec[A: ExprCodec: Type]: ExprCodec[Set[A]] = Expr.SetExprCodec[A]
    implicit def OptionExprCodec[A: ExprCodec: Type]: ExprCodec[Option[A]] = Expr.OptionExprCodec[A]
    implicit def SomeExprCodec[A: ExprCodec: Type]: ExprCodec[Some[A]] = Expr.SomeExprCodec[A]
    implicit lazy val NoneExprCodec: ExprCodec[None.type] = Expr.NoneExprCodec
    implicit def EitherExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Either[L, R]] =
      Expr.EitherExprCodec[L, R]
    implicit def LeftExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Left[L, R]] = Expr.LeftExprCodec[L, R]
    implicit def RightExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Right[L, R]] =
      Expr.RightExprCodec[L, R]

    implicit lazy val DataCodec: ExprCodec[data.Data] = Expr.DataExprCodec
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

    def matchOn[A: Type, B: Type](toMatch: Expr[A])(cases: NonEmptyVector[MatchCase[Expr[B]]]): Expr[B]

    def traverse: fp.Traverse[MatchCase]
  }
  implicit val MatchCaseTraverse: fp.Traverse[MatchCase] = MatchCase.traverse

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
    * @since 0.1.0
    */
  type Scoped[A]

  /** Create definitions, like `val`, `var`, `lazy val`, `def`, that should be accessible only inside some scope.
    *
    * Will take care of opening and closing that scope, which makes it easier to e.g. not use the definition before it
    * was defined.
    *
    * {{{
    * Scoped.createVal(Expr(1), "a").use { (a: Expr[A]) =>
    *   createExprB(a): Expr[B] // use a
    * } // : Expr[B]
    * // a is not accessible here
    * }}}
    *
    * @since 0.1.0
    */
  val Scoped: ScopedModule
  trait ScopedModule { this: Scoped.type =>

    def createVal[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]]
    def createVar[A: Type](
        initialValue: Expr[A],
        freshName: FreshName = FreshName.FromType
    ): Scoped[(Expr[A], Expr[A] => Expr[Unit])]
    def createLazy[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]]
    def createDef[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]]

    def partition[A, B, C](scoped: Scoped[A])(f: A => Either[B, C]): Either[Scoped[B], Scoped[C]]

    def closeScope[A](scoped: Scoped[Expr[A]]): Expr[A]

    def traverse: fp.Traverse[Scoped]
  }
  implicit val ScopedTraverse: fp.Traverse[Scoped] = Scoped.traverse

  implicit final class ScopedMethods[A](private val scoped: Scoped[A]) {

    def partition[B, C](f: A => Either[B, C]): Either[Scoped[B], Scoped[C]] = Scoped.partition(scoped)(f)

    def close[B](implicit ev: A <:< Expr[B]): Expr[B] = use(ev(_))

    def use[B](f: A => Expr[B]): Expr[B] = Scoped.closeScope(scoped.map(f))
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
    * If we are not building a lambda, but a normal expression we can use [[Scoped]] with Applicative/Parallel/Traverse
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

    def of1[A: Type](
        freshA: FreshName = FreshName.FromType
    ): LambdaBuilder[A => *, Expr[A]]
    def of2[A: Type, B: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])]
    // TODO: of3, of4, ...

    def partition[From[_], A, B, C](promise: LambdaBuilder[From, A])(
        f: A => Either[B, C]
    ): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]]

    def build[From[_], To: Type](builder: LambdaBuilder[From, Expr[To]]): Expr[From[To]]

    def traverse[From[_]]: fp.Traverse[LambdaBuilder[From, *]]
  }
  implicit def LambdaBuilderTraverse[From[_]]: fp.Traverse[LambdaBuilder[From, *]] = LambdaBuilder.traverse

  implicit final class LambdaBuilderMethods[From[_], A](private val builder: LambdaBuilder[From, A]) {

    def partition[B, C](f: A => Either[B, C]): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]] =
      LambdaBuilder.partition(builder)(f)

    def buildWith[To: Type](f: A => Expr[To]): Expr[From[To]] = LambdaBuilder.build(builder.map(f))

    def build[To: Type](implicit ev: A <:< Expr[To]): Expr[From[To]] = buildWith(ev)
  }
}
