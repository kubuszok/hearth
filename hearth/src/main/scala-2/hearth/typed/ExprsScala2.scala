package hearth
package typed

import hearth.fp.data.NonEmptyVector
import hearth.fp.syntax.*

trait ExprsScala2 extends Exprs { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type Expr[A] = c.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      final class ExprCodecImpl[A](typeCodec: Option[TypeCodec[A]])(implicit
          val to: Liftable[A],
          val from: Unliftable[A]
      ) extends ExprCodec[A] {

        override def toExpr(value: A): Expr[A] = typeCodec match {
          case Some(codec) if Environment.currentScalaVersion != ScalaVersion.Scala2_12 =>
            val aType = codec.toType(value).as_??
            import aType.Underlying as B
            c.Expr[B](to.apply(value)).asInstanceOf[Expr[A]]
          case _ => c.Expr[A](to.apply(value))
        }

        override def fromExpr(expr: Expr[A]): Option[A] = from.unapply(expr.tree)
      }

      implicit class ExprCodecCompanionOps(private val self: ExprCodec.type) {

        def make[A: Liftable: Unliftable]: ExprCodec[A] =
          new ExprCodecImpl[A](None)

        def withTypeCodec[A: Liftable: Unliftable: TypeCodec]: ExprCodec[A] =
          new ExprCodecImpl[A](Some(TypeCodec[A]))
      }

      object freshTerm {

        def apply(prefix: String): TermName =
          // Scala 3 generate prefix$macro$[n] while Scala 2 prefix[n] and we want to align the behavior
          c.internal.reificationSupport.freshTermName(prefix + "$macro$")

        def apply[A: Type](freshName: FreshName, expr: Expr[A]): TermName = freshName match {
          case FreshName.FromPrefix(prefix)         => apply(prefix)
          case FreshName.FromExpr if (expr != null) => apply(expr.tree.toString)
          case _ => apply(UntypedType.fromTyped[A].typeSymbol.name.decodedName.toString.toLowerCase)
        }
      }

      object implicits {

        implicit def ExprCodecLiftable[A: ExprCodec]: Liftable[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.to
          case unknown                => value => unknown.toExpr(value).tree
        }

        implicit def ExprCodecUnliftable[A: ExprCodec]: Unliftable[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.from
          case unknown                => tree => unknown.fromExpr(c.Expr[A](tree))
        }
      }
    }
    import platformSpecific.*

    override def prettyPrint[A](expr: Expr[A]): String =
      expr.tree
        .toString()
        // removes $macro$n from freshterms to make it easier to test and read
        .replaceAll("\\$macro", "")
        .replaceAll("\\$\\d+", "")
        // color expression for better UX (not as good as Scala 3 coloring but better than none)
        .split('\n')
        .map(line => Console.MAGENTA + line + Console.RESET)
        .mkString("\n")

    override def summonImplicit[A: Type]: Option[Expr[A]] =
      scala.util
        .Try(c.inferImplicitValue(Type[A].tpe, silent = true, withMacrosDisabled = false))
        .toOption
        .filterNot(_ == EmptyTree)
        .map(c.Expr[A](_))

    override def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      if (Type[A] =:= Type[B]) expr.asInstanceOf[Expr[B]] // types are identical in practice, we can just cast
      else c.Expr[B](q"($expr : ${Type[B]})") // check A <:< B AND add a syntax to force upcasting
    }

    override def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit] =
      // In Scala 2.12 suppressing two variables at once resulted in "_ is already defined as value _" error
      if (Environment.isScala2_12) c.Expr[Unit](q"_root_.scala.Predef.locally { val _ = $expr }")
      else c.Expr[Unit](q"val _ = $expr")

    override val BooleanExprCodec: ExprCodec[Boolean] = ExprCodec.withTypeCodec[Boolean]
    override val IntExprCodec: ExprCodec[Int] = ExprCodec.withTypeCodec[Int]
    override val LongExprCodec: ExprCodec[Long] = ExprCodec.withTypeCodec[Long]
    override val FloatExprCodec: ExprCodec[Float] = ExprCodec.withTypeCodec[Float]
    override val DoubleExprCodec: ExprCodec[Double] = ExprCodec.withTypeCodec[Double]
    override val CharExprCodec: ExprCodec[Char] = ExprCodec.withTypeCodec[Char]
    override val StringExprCodec: ExprCodec[String] = ExprCodec.withTypeCodec[String]
  }
  import Expr.platformSpecific.*

  sealed trait MatchCase[A] extends Product with Serializable

  object MatchCase extends MatchCaseModule {

    final private case class TypeMatch[A](name: TermName, expr: Expr_??, result: A) extends MatchCase[A]

    override def typeMatch[A: Type](freshName: FreshName = FreshName.FromType): MatchCase[Expr[A]] = {
      val name = freshTerm[A](freshName, null)
      val expr: Expr[A] = c.Expr[A](q"$name")
      TypeMatch(name, expr.as_??, expr)
    }

    override def matchOn[A: Type, B: Type](toMatch: Expr[A])(cases: NonEmptyVector[MatchCase[Expr[B]]]): Expr[B] = {
      val caseTrees = cases.toVector.map { case TypeMatch(name, expr, result) =>
        import expr.{Underlying as Matched, value as toSuppress}
        cq"""$name : $Matched => { val _ = $toSuppress; $result }"""
      }.toList
      c.Expr[B](q"$toMatch match { case ..$caseTrees }")
    }

    override val traverse: fp.Traverse[MatchCase] = new fp.Traverse[MatchCase] {

      override def traverse[G[_]: fp.Applicative, A, B](fa: MatchCase[A])(f: A => G[B]): G[MatchCase[B]] =
        fa match {
          case TypeMatch(name, expr, a) => f(a).map(b => TypeMatch(name, expr, b))
        }

      override def parTraverse[G[_]: fp.Parallel, A, B](fa: MatchCase[A])(f: A => G[B]): G[MatchCase[B]] =
        fa match {
          case TypeMatch(name, expr, a) => f(a).map(b => TypeMatch(name, expr, b))
        }
    }
  }

  final class Scoped[A] private (private val definitions: NonEmptyVector[ValOrDefDef], private val value: A)

  object Scoped extends ScopedModule {

    override def createVal[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]] = {
      val name = freshTerm[A](freshName, value)
      new Scoped[Expr[A]](NonEmptyVector(q"val $name = $value"), c.Expr[A](q"$name"))
    }
    override def createVar[A: Type](
        initialValue: Expr[A],
        freshName: FreshName = FreshName.FromType
    ): Scoped[(Expr[A], Expr[A] => Expr[Unit])] = {
      val name = freshTerm[A](freshName, initialValue)
      new Scoped[(Expr[A], Expr[A] => Expr[Unit])](
        NonEmptyVector.one(q"var $name = $initialValue"),
        (c.Expr[A](q"$name"), (expr: Expr[A]) => c.Expr[Unit](q"$name = $expr"))
      )
    }
    override def createLazy[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]] = {
      val name = freshTerm[A](freshName, value)
      new Scoped[Expr[A]](NonEmptyVector.one(q"lazy val $name = $value"), c.Expr[A](q"$name"))
    }
    override def createDef[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]] = {
      val name = freshTerm[A](freshName, value)
      new Scoped[Expr[A]](NonEmptyVector.one(q"def $name = $value"), c.Expr[A](q"$name"))
    }

    override def partition[A, B, C](scoped: Scoped[A])(f: A => Either[B, C]): Either[Scoped[B], Scoped[C]] =
      f(scoped.value) match {
        case Left(value)  => Left(new Scoped[B](scoped.definitions, value))
        case Right(value) => Right(new Scoped[C](scoped.definitions, value))
      }

    override def closeScope[A](scoped: Scoped[Expr[A]]): Expr[A] =
      c.Expr[A](q"..${scoped.definitions.toVector}; ${scoped.value}")

    override val traverse: fp.Traverse[Scoped] = new fp.Traverse[Scoped] {

      override def traverse[G[_]: fp.Applicative, A, B](fa: Scoped[A])(f: A => G[B]): G[Scoped[B]] =
        f(fa.value).map(b => new Scoped[B](fa.definitions, b))

      override def parTraverse[G[_]: fp.Parallel, A, B](fa: Scoped[A])(f: A => G[B]): G[Scoped[B]] =
        f(fa.value).map(b => new Scoped[B](fa.definitions, b))
    }
  }

  final class LambdaBuilder[From[_], To] private (private val mk: LambdaBuilder.Mk[From], private val value: To)

  object LambdaBuilder extends LambdaBuilderModule {

    private trait Mk[From[_]] {

      def apply[To: Type](body: Expr[To]): Expr[From[To]]
    }

    override def of1[A: Type](
        freshA: FreshName = FreshName.FromType
    ): LambdaBuilder[A => *, Expr[A]] = {
      val a = freshTerm[A](freshA, null)
      new LambdaBuilder[A => *, Expr[A]](
        new Mk[A => *] {
          override def apply[To: Type](body: Expr[To]): Expr[A => To] =
            c.Expr[A => To](q"($a: ${Type[A]}) => $body")
        },
        c.Expr[A](q"$a")
      )
    }
    override def of2[A: Type, B: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])] = {
      val a = freshTerm[A](freshA, null)
      val b = freshTerm[B](freshB, null)
      new LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])](
        new Mk[(A, B) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B) => To] =
            c.Expr[(A, B) => To](q"($a: ${Type[A]}, $b: ${Type[B]}) => $body")
        },
        (c.Expr[A](q"$a"), c.Expr[B](q"$b"))
      )
    }

    override def partition[From[_], A, B, C](promise: LambdaBuilder[From, A])(
        f: A => Either[B, C]
    ): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]] =
      f(promise.value) match {
        case Left(value)  => Left(new LambdaBuilder[From, B](promise.mk, value))
        case Right(value) => Right(new LambdaBuilder[From, C](promise.mk, value))
      }

    override def build[From[_], To: Type](builder: LambdaBuilder[From, Expr[To]]): Expr[From[To]] =
      builder.mk(builder.value)

    override def traverse[From[_]]: fp.Traverse[LambdaBuilder[From, *]] = new fp.Traverse[LambdaBuilder[From, *]] {

      override def traverse[G[_]: fp.Applicative, A, B](fa: LambdaBuilder[From, A])(
          f: A => G[B]
      ): G[LambdaBuilder[From, B]] =
        f(fa.value).map(b => new LambdaBuilder[From, B](fa.mk, b))

      override def parTraverse[G[_]: fp.Parallel, A, B](fa: LambdaBuilder[From, A])(
          f: A => G[B]
      ): G[LambdaBuilder[From, B]] =
        f(fa.value).map(b => new LambdaBuilder[From, B](fa.mk, b))
    }
  }
}
