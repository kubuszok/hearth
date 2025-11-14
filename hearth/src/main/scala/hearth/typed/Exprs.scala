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

    final def apply[A: ExprCodec](value: A): Expr[A] = ExprCodec[A].toExpr(value)

    final def unapply[A: ExprCodec](expr: Expr[A]): Option[A] = ExprCodec[A].fromExpr(expr)

    def plainPrint[A](expr: Expr[A]): String
    def prettyPrint[A](expr: Expr[A]): String
    def plainAST[A](expr: Expr[A]): String
    def prettyAST[A](expr: Expr[A]): String

    def summonImplicit[A: Type]: SummoningResult[A]
    def summonImplicitIgnoring[A: Type](excluded: UntypedMethod*): SummoningResult[A]

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

    def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]]
    def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]]

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

      private val DataType: Type[data.Data] = Type.of[data.Data]

      private val StringType: Type[String] = Type.of[String]

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

  implicit final def ExistentialExprMethods(expr: Expr_??): BoundedExistentialExprMethods[Nothing, Any] =
    new BoundedExistentialExprMethods[Nothing, Any](expr)
  implicit final def LowerBoundedExistentialExprMethods[L](expr: Expr_??>:[L]): BoundedExistentialExprMethods[L, Any] =
    new BoundedExistentialExprMethods[L, Any](expr)
  implicit final def UpperBoundedExistentialExprMethods[U](
      expr: Expr_??<:[U]
  ): BoundedExistentialExprMethods[Nothing, U] =
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
  object ExprCodec {

    def apply[A](implicit codec: ExprCodec[A]): ExprCodec[A] = codec

    // TODO: Consider implementing more of these:
    // TODO: Tuple1-Tuple22
    // TODO: BigInt, BigDecimal, StringContext
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

    implicit def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]] = Expr.ClassExprCodec[A]
    implicit def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]] = Expr.ClassTagExprCodec[A]

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

    def matchOn[A: Type, B: Type](toMatch: Expr[A])(cases: NonEmptyVector[MatchCase[Expr[B]]]): Expr[B]

    def partition[A, B, C](matchCase: MatchCase[A])(f: A => Either[B, C]): Either[MatchCase[B], MatchCase[C]]

    def traverse: fp.Traverse[MatchCase]
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
  implicit final val ScopedTraverse: fp.Traverse[Scoped] = Scoped.traverse

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

    // format: off
    def of1[A: Type](
        freshA: FreshName = FreshName.FromType
    ): LambdaBuilder[A => *, Expr[A]]
    def of2[A: Type, B: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])]
    def of3[A: Type, B: Type, C: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C) => *, (Expr[A], Expr[B], Expr[C])]
    def of4[A: Type, B: Type, C: Type, D: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D) => *, (Expr[A], Expr[B], Expr[C], Expr[D])]
    def of5[A: Type, B: Type, C: Type, D: Type, E: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E])]
    def of6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F])]
    def of7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G])]
    def of8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H])]
    def of9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType,
        freshI: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I])]
    def of10[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J])]
    def of11[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K])]
    def of12[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L])]
    def of13[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M])]
    def of14[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N])]
    def of15[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O])]
    def of16[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P])]
    def of17[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q])]
    def of18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R])]
    def of19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S])]
    def of20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T])]
    def of21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U])]
    def of22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V])]
    // format: on

    def build[From[_], To: Type](builder: LambdaBuilder[From, Expr[To]]): Expr[From[To]]

    def partition[From[_], A, B, C](builder: LambdaBuilder[From, A])(
        f: A => Either[B, C]
    ): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]]

    def traverse[From[_]]: fp.Traverse[LambdaBuilder[From, *]]
  }
  implicit final def LambdaBuilderTraverse[From[_]]: fp.Traverse[LambdaBuilder[From, *]] = LambdaBuilder.traverse

  implicit final class LambdaBuilderMethods[From[_], A](private val builder: LambdaBuilder[From, A]) {

    def partition[B, C](f: A => Either[B, C]): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]] =
      LambdaBuilder.partition(builder)(f)

    def buildWith[To: Type](f: A => Expr[To]): Expr[From[To]] = LambdaBuilder.build(builder.map(f))

    def build[To: Type](implicit ev: A <:< Expr[To]): Expr[From[To]] = buildWith(ev)
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
    * DefBuilder.of1[A, Return]("a").buildWith { (self: Expr[A] => Expr[Return], a: Expr[A]) =>
    *   // use self and a
    *   createExprB(self, a): Expr[B]
    * } // : Scoped[Expr[A] => Expr[Return]]
    *
    * // More complex case (where builder is needed to aggregate errors)
    * DefBuilder.of1[A, Return]("a").traverse[MIO, Return] { (self: Expr[A] => Expr[Return], a: Expr[A]) =>
    *   // use a
    *   createExprBOrError(self, a): MIO[Expr[Return]]
    * }.map(_.build) // : MIO[Scoped[Expr[A] => Expr[Return]]]
    * }}}
    *
    * If we are not taking arguments, do not need recursion nor error aggregation, we can use [[Scoped]] to create a
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
  type DefBuilder[Signature, Return, Value]

  val DefBuilder: DefBuilderModule
  trait DefBuilderModule { this: DefBuilder.type =>

    // format: off
    def of1[A: Type, Return: Type](
        freshName: FreshName,
        freshA: FreshName = FreshName.FromType,
    ): DefBuilder[Expr[A] => Expr[Return], Return, (Expr[A] => Expr[Return], Expr[A])]
    // format: on

    def build[Signature, Return](builder: DefBuilder[Signature, Return, Expr[Return]]): Scoped[Signature]

    def buildCached[Signature, Return](
        cache: DefCache,
        key: String,
        builder: DefBuilder[Signature, Return, Expr[Return]]
    ): DefCache

    def forwardDeclare[Signature, Return, Value](
        cache: DefCache,
        key: String,
        builder: DefBuilder[Signature, Return, Value]
    ): DefCache

    def partition[Signature, Return, A, B, C](builder: DefBuilder[Signature, Return, A])(
        f: A => Either[B, C]
    ): Either[DefBuilder[Signature, Return, B], DefBuilder[Signature, Return, C]]

    def traverse[Signature, Return]: fp.Traverse[DefBuilder[Signature, Return, *]]
  }
  implicit final def DefBuilderTraverse[Signature, Return]: fp.Traverse[DefBuilder[Signature, Return, *]] =
    DefBuilder.traverse

  implicit final class DefBuilderMethods[Signature, Returned, A](
      private val builder: DefBuilder[Signature, Returned, A]
  ) {

    def partition[B, C](
        f: A => Either[B, C]
    ): Either[DefBuilder[Signature, Returned, B], DefBuilder[Signature, Returned, C]] =
      DefBuilder.partition(builder)(f)

    def buildWith(f: A => Expr[Returned]): Scoped[Signature] = DefBuilder.build(builder.map(f))

    def build(implicit ev: A <:< Expr[Returned]): Scoped[Signature] = buildWith(ev)

    def buildCachedWith(cache: DefCache, key: String)(f: A => Expr[Returned]): DefCache =
      DefBuilder.buildCached(cache, key, builder.map(f))

    def buildCached(cache: DefCache, key: String)(implicit ev: A <:< Expr[Returned]): DefCache =
      buildCachedWith(cache, key)(ev)
  }

  /** Cache for defs, so that you can avoid computing everything inside nested [[Scoped]]s when you need to construct
    * multiple defs.
    *
    * @since 0.2.0
    */
  type DefCache

  val DefCache: DefCacheModule
  trait DefCacheModule { this: DefCache.type =>

    def empty: DefCache

    def get1[A: Type, Returned: Type](cache: DefCache, key: String): Option[Expr[A] => Expr[Returned]]

    def toScoped(cache: DefCache): Scoped[Unit]
  }
  implicit final class DefCacheMethods(private val cache: DefCache) {

    def get1[A: Type, Returned: Type](key: String): Option[Expr[A] => Expr[Returned]] = DefCache.get1(cache, key)

    def toScoped: Scoped[Unit] = DefCache.toScoped(cache)
  }
}
