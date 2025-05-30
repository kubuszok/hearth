package hearth
package typed

import hearth.fp.data.NonEmptyVector
import hearth.fp.syntax.*

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Expr[A] = scala.quoted.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      final class ExprCodecImpl[A](using val from: scala.quoted.FromExpr[A], val to: scala.quoted.ToExpr[A])
          extends ExprCodec[A] {
        def toExpr(value: A): Expr[A] = to(value)
        def fromExpr(expr: Expr[A]): Option[A] = from.unapply(expr)
      }

      extension (self: ExprCodec.type) {

        def make[A: scala.quoted.FromExpr: scala.quoted.ToExpr]: ExprCodec[A] =
          new ExprCodecImpl[A]
      }

      extension [A](expr: Expr[A]) {
        // Required by -Xcheck-macros to pass.
        def resetOwner(using Type[A]): Expr[A] = expr.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[A]
      }

      object freshTerm {
        // Workaround to contain @experimental from polluting the whole codebase
        private val impl = quotes.reflect.Symbol.getClass.getMethod("freshName", classOf[String])

        def apply(prefix: String): String = impl.invoke(quotes.reflect.Symbol, prefix).asInstanceOf[String]

        def apply[A: Type](freshName: FreshName, expr: Expr[A]): String = freshName match {
          case FreshName.FromPrefix(prefix)       => apply(prefix)
          case FreshName.FromExpr if expr != null => apply(expr.asTerm.show(using Printer.TreeCode))
          case _ => apply(unapplyTypes[A].show(using Printer.TypeReprShortCode).toLowerCase)
        }

        def bind[A: Type](freshName: FreshName, flags: Flags): Symbol =
          Symbol.newBind(Symbol.spliceOwner, apply[A](freshName, null), flags, TypeRepr.of[A])

        def valdef[A: Type](freshName: FreshName, expr: Expr[A], flags: Flags): Symbol =
          Symbol.newVal(Symbol.spliceOwner, apply[A](freshName, expr), TypeRepr.of[A], flags, Symbol.noSymbol)

        // To keep things consistent with Scala 2 for e.g. "Some[String]" we should generate "some" rather than
        // "some[string], so we need to remove types applied to type constructor.
        private def unapplyTypes[A: Type]: TypeRepr =
          TypeRepr.of[A] match {
            case AppliedType(repr, _) => repr
            case otherwise            => otherwise
          }
      }

      object implicits {

        given ExprCodecIsFromExpr[A: ExprCodec]: scala.quoted.FromExpr[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.from
          case unknown                =>
            new scala.quoted.FromExpr[A] {
              override def unapply(expr: Expr[A])(using scala.quoted.Quotes): Option[A] = unknown.fromExpr(expr)
            }
        }

        given ExprCodecIsToExpr[A: ExprCodec]: scala.quoted.ToExpr[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.to
          case unknown                =>
            new scala.quoted.ToExpr[A] {
              override def apply(value: A)(using scala.quoted.Quotes): Expr[A] = unknown.toExpr(value)
            }
        }
      }
    }
    import platformSpecific.*

    def prettyPrint[A](expr: Expr[A]): String =
      expr.asTerm
        .show(using Printer.TreeAnsiCode)
        // remove $macro$n from freshterms to make it easier to test and read
        .replaceAll("\\$macro", "")
        .replaceAll("\\$\\d+", "")

    def summonImplicit[A: Type]: Option[Expr[A]] = scala.quoted.Expr.summon[A]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      expr.asInstanceOf[Expr[B]] // check that A <:< B without upcasting in code (Scala 3 should get away without it)
    }

    override def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit] = '{ val _ = ${ expr } }

    override val BooleanExprCodec: ExprCodec[Boolean] = ExprCodec.make[Boolean]
    override val IntExprCodec: ExprCodec[Int] = ExprCodec.make[Int]
    override val LongExprCodec: ExprCodec[Long] = ExprCodec.make[Long]
    override val FloatExprCodec: ExprCodec[Float] = ExprCodec.make[Float]
    override val DoubleExprCodec: ExprCodec[Double] = ExprCodec.make[Double]
    override val CharExprCodec: ExprCodec[Char] = ExprCodec.make[Char]
    override val StringExprCodec: ExprCodec[String] = ExprCodec.make[String]
  }
  import Expr.platformSpecific.*

  sealed trait MatchCase[A] extends Product with Serializable

  object MatchCase extends MatchCaseModule {

    final private case class TypeMatch[A](name: Symbol, expr: Expr_??, result: A) extends MatchCase[A]

    override def typeMatch[A: Type](freshName: FreshName = FreshName.FromType): MatchCase[Expr[A]] = {
      val name = freshTerm.bind[A](freshName, Flags.EmptyFlags)
      val expr: Expr[A] = Ref(name).asExprOf[A]
      TypeMatch(name, expr.as_??, expr)
    }

    override def matchOn[A: Type, B: Type](toMatch: Expr[A])(cases: NonEmptyVector[MatchCase[Expr[B]]]): Expr[B] = {
      val uncheckedToMatch = '{ ${ toMatch.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[A] }: @scala.unchecked }

      val caseTrees = cases
        .map { case TypeMatch(name, expr, result) =>
          import expr.{Underlying as Matched, value as toSuppress}

          val body = '{ val _ = $toSuppress; $result }

          val sym = TypeRepr.of[Matched].typeSymbol
          if sym.flags.is(Flags.Enum) && (sym.flags.is(Flags.JavaStatic) || sym.flags.is(Flags.StableRealizable)) then
          // Scala 3's enums' parameterless cases are vals with type erased, so we have to match them by value
          // case arg @ Enum.Value => ...
          CaseDef(Bind(name, Ident(sym.termRef)), None, body.asTerm)
          // case arg : Enum.Value => ...
          else CaseDef(Bind(name, Typed(Wildcard(), TypeTree.of[Matched])), None, body.asTerm)
        }
        .toVector
        .toList

      Match(uncheckedToMatch.asTerm, caseTrees).asExprOf[B]
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

  final class Scoped[A] private (private val definitions: NonEmptyVector[Statement], private val value: A)

  object Scoped extends ScopedModule {

    override def createVal[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]] = {
      val name = freshTerm.valdef[A](freshName, value, Flags.EmptyFlags)
      new Scoped[Expr[A]](NonEmptyVector.one(ValDef(name, Some(value.asTerm.changeOwner(name)))), Ref(name).asExprOf[A])
    }
    override def createVar[A: Type](
        initialValue: Expr[A],
        freshName: FreshName = FreshName.FromType
    ): Scoped[(Expr[A], Expr[A] => Expr[Unit])] = {
      val name = freshTerm.valdef[A](freshName, initialValue, Flags.Mutable)
      new Scoped[(Expr[A], Expr[A] => Expr[Unit])](
        NonEmptyVector.one(ValDef(name, Some(initialValue.asTerm.changeOwner(name)))),
        (Ref(name).asExprOf[A], expr => Assign(Ref(name), expr.asTerm).asExprOf[Unit])
      )
    }
    override def createLazy[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]] = {
      val name = freshTerm.valdef[A](freshName, value, Flags.Lazy)
      new Scoped[Expr[A]](NonEmptyVector.one(ValDef(name, Some(value.asTerm.changeOwner(name)))), Ref(name).asExprOf[A])
    }
    override def createDef[A: Type](value: Expr[A], freshName: FreshName = FreshName.FromType): Scoped[Expr[A]] = {
      val name = freshTerm.valdef[A](freshName, value, Flags.EmptyFlags)
      new Scoped[Expr[A]](
        NonEmptyVector.one(DefDef(name, _ => Some(value.asTerm.changeOwner(name)))),
        Ref(name).asExprOf[A]
      )
    }

    override def partition[A, B, C](scoped: Scoped[A])(f: A => Either[B, C]): Either[Scoped[B], Scoped[C]] =
      f(scoped.value) match {
        case Left(value)  => Left(new Scoped[B](scoped.definitions, value))
        case Right(value) => Right(new Scoped[C](scoped.definitions, value))
      }

    override def closeScope[A](scoped: Scoped[Expr[A]]): Expr[A] =
      Block(scoped.definitions.toVector.toList, scoped.value.asTerm).asExpr.asInstanceOf[Expr[A]]

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
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      new LambdaBuilder[A => *, Expr[A]](
        new Mk[A => *] {
          override def apply[To: Type](body: Expr[To]): Expr[A => To] = {

            def mkBody(a: Expr[A]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A) => ${ mkBody('a) } }
          }
        },
        a1Expr
      )
    }
    override def of2[A: Type, B: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      new LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])](
        new Mk[(A, B) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B) => To] = {

            def mkBody(a: Expr[A], b: Expr[B]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B) => ${ mkBody('a, 'b) } }
          }
        },
        (a1Expr, b1Expr)
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
