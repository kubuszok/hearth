package hearth
package typed

import hearth.fp.data.NonEmptyVector
import hearth.fp.syntax.*

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*
  import scala.quoted.{Exprs, FromExpr, ToExpr, Varargs}

  final override type Expr[A] = scala.quoted.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      final class ExprCodecImpl[A](using val from: FromExpr[A], val to: ToExpr[A]) extends ExprCodec[A] {
        def toExpr(value: A): Expr[A] = to(value)
        def fromExpr(expr: Expr[A]): Option[A] = from.unapply(expr)
      }

      extension (self: ExprCodec.type) {

        def make[A: FromExpr: ToExpr]: ExprCodec[A] = new ExprCodecImpl[A]
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

        given ExprCodecIsFromExpr[A: ExprCodec]: FromExpr[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.from
          case unknown                =>
            new scala.quoted.FromExpr[A] {
              override def unapply(expr: Expr[A])(using scala.quoted.Quotes): Option[A] = unknown.fromExpr(expr)
            }
        }

        given ExprCodecIsToExpr[A: ExprCodec]: ToExpr[A] = ExprCodec[A] match {
          case impl: ExprCodecImpl[A] => impl.to
          case unknown                =>
            new ToExpr[A] {
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

    override lazy val NullExprCodec: ExprCodec[Null] = {
      given ToExpr[Null] = new {
        override def apply(value: Null)(using scala.quoted.Quotes): Expr[Null] = '{ null }
      }
      ExprCodec.make[Null]
    }
    override lazy val UnitExprCodec: ExprCodec[Unit] = {
      given FromExpr[Unit] = new {
        override def unapply(expr: Expr[Unit])(using scala.quoted.Quotes): Option[Unit] = expr match {
          case '{ () } => Some(())
          case _       => None
        }
      }
      given ToExpr[Unit] = new {
        override def apply(value: Unit)(using scala.quoted.Quotes): Expr[Unit] = '{ () }
      }
      ExprCodec.make[Unit]
    }
    override lazy val BooleanExprCodec: ExprCodec[Boolean] = ExprCodec.make[Boolean]
    override lazy val ByteExprCodec: ExprCodec[Byte] = ExprCodec.make[Byte]
    override lazy val ShortExprCodec: ExprCodec[Short] = {
      given FromExpr[Short] = new {
        override def unapply(expr: Expr[Short])(using scala.quoted.Quotes): Option[Short] = expr match {
          case '{ (${ inner }: Int).toShort } => scala.quoted.Expr.unapply(inner).map(_.toShort)
          case _                              => None
        }
      }
      ExprCodec.make[Short]
    }
    override lazy val IntExprCodec: ExprCodec[Int] = ExprCodec.make[Int]
    override lazy val LongExprCodec: ExprCodec[Long] = ExprCodec.make[Long]
    override lazy val FloatExprCodec: ExprCodec[Float] = ExprCodec.make[Float]
    override lazy val DoubleExprCodec: ExprCodec[Double] = ExprCodec.make[Double]
    override lazy val CharExprCodec: ExprCodec[Char] = ExprCodec.make[Char]
    override lazy val StringExprCodec: ExprCodec[String] = ExprCodec.make[String]

    // For now, assume that all ExprCodecs below are of PoC quality. It was needed to unblock some other work.
    // But each should have unit tests which would make sure that Scala 2 and Scala 3 are in sync (which would
    // require adding missing implementations to both sides, and then expanding the build-in FromExpr and ToExpr
    // with more cases).

    def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]] = {
      import platformSpecific.implicits.given
      given FromExpr[java.lang.Class[A]] = new {
        override def unapply(expr: Expr[java.lang.Class[A]])(using scala.quoted.Quotes): Option[java.lang.Class[A]] =
          expr.asTerm match {
            case Applied(ref: Ref, List(typeTree: TypeTree))
                if ref.symbol == defn.Predef_classOf && typeTree.tpe =:= TypeRepr.of[A] =>
              Type.classOfType[A]
            case _ => None
          }
      }
      ExprCodec.make[java.lang.Class[A]]
    }
    def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]] = {
      import platformSpecific.implicits.given
      given FromExpr[scala.reflect.ClassTag[A]] = new {
        override def unapply(
            expr: Expr[scala.reflect.ClassTag[A]]
        )(using scala.quoted.Quotes): Option[scala.reflect.ClassTag[A]] = expr match {
          case '{ scala.reflect.ClassTag[a](${ _ }) } if TypeRepr.of[a] =:= TypeRepr.of[A] =>
            Type.classOfType[A].map(scala.reflect.ClassTag(_))
          case _ => None
        }
      }
      ExprCodec.make[scala.reflect.ClassTag[A]]
    }

    def ArrayExprCodec[A: ExprCodec: Type]: ExprCodec[Array[A]] = {
      import platformSpecific.implicits.given
      given ExprCodec[scala.reflect.ClassTag[A]] = ClassTagExprCodec[A]
      given FromExpr[Array[A]] = new {
        override def unapply(expr: Expr[Array[A]])(using scala.quoted.Quotes): Option[Array[A]] = expr match {
          // TODO: just one possible case, but we need more
          case '{ Array[A]((${ Varargs(Exprs(inner)) })*)(using (${ Expr(ct) }: scala.reflect.ClassTag[A])) } =>
            Some(inner.toArray(using ct))
          case _ => None
        }
      }
      given ToExpr[Array[A]] =
        if Type[A] =:= Type[Boolean] then ToExpr.ArrayOfBooleanToExpr.asInstanceOf[ToExpr[Array[A]]]
        else if Type[A] =:= Type[Byte] then ToExpr.ArrayOfByteToExpr.asInstanceOf[ToExpr[Array[A]]]
        else if Type[A] =:= Type[Short] then ToExpr.ArrayOfShortToExpr.asInstanceOf[ToExpr[Array[A]]]
        else if Type[A] =:= Type[Int] then ToExpr.ArrayOfIntToExpr.asInstanceOf[ToExpr[Array[A]]]
        else if Type[A] =:= Type[Long] then ToExpr.ArrayOfLongToExpr.asInstanceOf[ToExpr[Array[A]]]
        else if Type[A] =:= Type[Float] then ToExpr.ArrayOfFloatToExpr.asInstanceOf[ToExpr[Array[A]]]
        else if Type[A] =:= Type[Double] then ToExpr.ArrayOfDoubleToExpr.asInstanceOf[ToExpr[Array[A]]]
        else if Type[A] =:= Type[Char] then ToExpr.ArrayOfCharToExpr.asInstanceOf[ToExpr[Array[A]]]
        else
          new ToExpr[Array[A]] {
            override def apply(value: Array[A])(using scala.quoted.Quotes): Expr[Array[A]] =
              Type
                .classOfType[A]
                .map { clazz =>
                  given scala.reflect.ClassTag[A] = scala.reflect.ClassTag(clazz)
                  ToExpr.ArrayToExpr[A]
                }
                .getOrElse {
                  assertionFailed(
                    s"Could not figure out ClassTag[${Type.prettyPrint[A]}] - support for such cases is still experimental"
                  )
                  // given scala.reflect.ClassTag[A] = scala.reflect.ClassTag(classOf[Any]).asInstanceOf[scala.reflect.ClassTag[A]]
                  // ToExpr.ArrayToExpr[A]
                }
                .apply(value)
          }
      ExprCodec.make[Array[A]]
    }
    def SeqExprCodec[A: ExprCodec: Type]: ExprCodec[Seq[A]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Seq[A]]
    }
    def ListExprCodec[A: ExprCodec: Type]: ExprCodec[List[A]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[List[A]]
    }
    lazy val NilExprCodec: ExprCodec[Nil.type] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Nil.type]
    }
    def VectorExprCodec[A: ExprCodec: Type]: ExprCodec[Vector[A]] = {
      import platformSpecific.implicits.given
      given FromExpr[Vector[A]] = new {
        override def unapply(expr: Expr[Vector[A]])(using scala.quoted.Quotes): Option[Vector[A]] = expr match {
          // TODO: just one possible case, but we need more
          case '{ Vector((${ Varargs(Exprs(inner)) }: Seq[A])*) } => Some(inner.toVector)
          case _                                                  => None
        }
      }
      given ToExpr[Vector[A]] = new {
        override def apply(value: Vector[A])(using scala.quoted.Quotes): Expr[Vector[A]] = '{
          Vector[A]((${ Varargs(value.map(Expr(_)).toSeq) })*)
        }
      }
      ExprCodec.make[Vector[A]]
    }
    def MapExprCodec[K: ExprCodec: Type, V: ExprCodec: Type]: ExprCodec[Map[K, V]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Map[K, V]]
    }
    def SetExprCodec[A: ExprCodec: Type]: ExprCodec[Set[A]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Set[A]]
    }
    def OptionExprCodec[A: ExprCodec: Type]: ExprCodec[Option[A]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Option[A]]
    }
    def SomeExprCodec[A: ExprCodec: Type]: ExprCodec[Some[A]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Some[A]]
    }
    lazy val NoneExprCodec: ExprCodec[None.type] = {
      import platformSpecific.implicits.given
      ExprCodec.make[None.type]
    }
    def EitherExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Either[L, R]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Either[L, R]]
    }
    def LeftExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Left[L, R]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Left[L, R]]
    }
    def RightExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Right[L, R]] = {
      import platformSpecific.implicits.given
      ExprCodec.make[Right[L, R]]
    }
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
