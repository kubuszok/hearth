package hearth
package typed

import hearth.fp.data.NonEmptyVector
import hearth.fp.syntax.*

import scala.language.implicitConversions

trait Exprs extends ExprsCrossQuotes { this: MacroCommons =>

  /** Platform-specific untyped type representation (`c.Expr[A]` in 2, `scala.quoted.Expr[A]` in 3) */
  type Expr[A]

  val Expr: ExprModule
  trait ExprModule extends ExprCrossQuotes { this: Expr.type =>

    def apply[A: ExprCodec](value: A): Expr[A] = ExprCodec[A].toExpr(value)

    def unapply[A: ExprCodec](expr: Expr[A]): Option[A] = ExprCodec[A].fromExpr(expr)

    def plainPrint[A](expr: Expr[A]): String = removeAnsiColors(prettyPrint(expr))
    def prettyPrint[A](expr: Expr[A]): String

    def summonImplicit[A: Type]: Option[Expr[A]]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit]

    val BooleanExprCodec: ExprCodec[Boolean]
    val IntExprCodec: ExprCodec[Int]
    val LongExprCodec: ExprCodec[Long]
    val FloatExprCodec: ExprCodec[Float]
    val DoubleExprCodec: ExprCodec[Double]
    val CharExprCodec: ExprCodec[Char]
    val StringExprCodec: ExprCodec[String]
  }

  implicit final class ExprMethods[A](private val expr: Expr[A]) {

    def value(implicit codec: ExprCodec[A]): Option[A] = codec.fromExpr(expr)

    def plainPrint: String = Expr.plainPrint(expr)
    def prettyPrint: String = Expr.prettyPrint(expr)

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

    implicit lazy val BooleanExprCodec: ExprCodec[Boolean] = Expr.BooleanExprCodec
    implicit lazy val IntExprCodec: ExprCodec[Int] = Expr.IntExprCodec
    implicit lazy val LongExprCodec: ExprCodec[Long] = Expr.LongExprCodec
    implicit lazy val FloatExprCodec: ExprCodec[Float] = Expr.FloatExprCodec
    implicit lazy val DoubleExprCodec: ExprCodec[Double] = Expr.DoubleExprCodec
    implicit lazy val CharExprCodec: ExprCodec[Char] = Expr.CharExprCodec
    implicit lazy val StringExprCodec: ExprCodec[String] = Expr.StringExprCodec
  }

  /** Provides support for building pattern-matching expressions. */
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

    def matchOn[B: Type](head: MatchCase[Expr[B]], tail: MatchCase[Expr[B]]*): Expr[B] =
      matchOn(NonEmptyVector(head, tail*))
  }

  /** To avoid name clashes, we need to generate them. This enum provides various strategies to generate fresh names. */
  sealed trait FreshName extends Product with Serializable
  object FreshName {
    case object FromType extends FreshName
    case object FromExpr extends FreshName
    final case class FromPrefix(prefix: String) extends FreshName
  }

  /** Stores one or more val/var/lazy val/def definitions, as well as the code that uses them.
    *
    * Allow us to use the val/var/lazy val/def to construct some expression, and then safely close the scope, make sure
    * that the definition that's inaccessible outside of it, won't ve visible outside of it, while returning the value
    * of a whole block.
    */
  type Scoped[A]

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

    def partition[B, C](f: A => Either[B, C]): Either[Scoped[B], Scoped[C]] =
      Scoped.partition(scoped)(f)

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
    * If we don't need to aggregate errors, we can use direct-style to combine partial results inside the lambda body.
    *
    * If we are not building a lambda but normal expression we can use [[Scoped]] with Applicative/Parallel/Traverse
    * combinators.
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
