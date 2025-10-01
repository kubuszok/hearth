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
        override def toExpr(value: A): Expr[A] = to(value)
        override def fromExpr(expr: Expr[A]): Option[A] = from.unapply(expr)
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

        def defdef[A: Type](freshName: FreshName, expr: Expr[A]): Symbol =
          Symbol.newMethod(
            Symbol.spliceOwner,
            apply[A](freshName, expr),
            MethodType(Nil)(_ => Nil, _ => TypeRepr.of[A])
          )

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

    override def prettyPrint[A](expr: Expr[A]): String = expr.asTerm
      .show(using Printer.TreeAnsiCode)
      // remove $macro$n from freshterms to make it easier to test and read
      .replaceAll("\\$macro", "")
      .replaceAll("\\$\\d+", "")

    override def prettyAST[A](expr: Expr[A]): String = expr.asTerm
      .show(using Printer.TreeStructure)
      // color expression for better UX
      .split('\n')
      .view
      .map(line => Console.MAGENTA + line + Console.RESET)
      .mkString("\n")

    override def summonImplicit[A: Type]: Option[Expr[A]] = scala.quoted.Expr.summon[A]

    override def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      expr.asInstanceOf[Expr[B]] // check that A <:< B without upcasting in code (Scala 3 should get away without it)
    }

    override def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit] = '{ val _ = ${ expr }; () }

    override lazy val NullExprCodec: ExprCodec[Null] = {
      given FromExpr[Null] = new {
        override def unapply(expr: Expr[Null])(using scala.quoted.Quotes): Option[Null] = expr match {
          case '{ null } => Some(null)
          case _         => None
        }
      }
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
          case _                              =>
            expr.asTerm match {
              case Literal(ShortConstant(value)) => Some(value)
              case _                             => None
            }
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
    // require adding missing implementations to both sides, and then expanding the built-in FromExpr and ToExpr
    // with more cases).

    override def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]] = {
      given FromExpr[java.lang.Class[A]] = new {
        override def unapply(expr: Expr[java.lang.Class[A]])(using scala.quoted.Quotes): Option[java.lang.Class[A]] = {
          def matchTerm(tree: Tree): Option[java.lang.Class[A]] = tree match {
            case Inlined(_, _, tree)                                               => matchTerm(tree)
            case Literal(ClassOfConstant(typeRepr)) if typeRepr =:= TypeRepr.of[A] => Type.classOfType[A]
            case Applied(ref: Ref, List(typeTree: TypeTree))
                if ref.symbol == defn.Predef_classOf && typeTree.tpe =:= TypeRepr.of[A] =>
              Type.classOfType[A]
            case TypeApply(Ident("classOf"), List(typeTree)) if typeTree.tpe =:= TypeRepr.of[A] => Type.classOfType[A]
            case _                                                                              => None
          }
          matchTerm(expr.asTerm)
        }
      }
      ExprCodec.make[java.lang.Class[A]]
    }
    override def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]] = {
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

    // In the code below we cannot just `import platformSpecific.implicits.given`, because Expr.make[Coll[A]] would use
    // implicit ExprCodec[Coll[A]] from the companion object, which would create a circular dependency. Instead, we
    // want to extract the implicit ToExpr[A] and FromExpr[A] from the ExprCodec[A], and then use it in the code below.

    override def ArrayExprCodec[A: ExprCodec: Type]: ExprCodec[Array[A]] = {
      given ExprCodec[scala.reflect.ClassTag[A]] = ClassTagExprCodec[A]
      given FromExpr[A] = platformSpecific.implicits.ExprCodecIsFromExpr[A]
      given FromExpr[Array[A]] = new {
        override def unapply(expr: Expr[Array[A]])(using scala.quoted.Quotes): Option[Array[A]] = expr match {
          // TODO: just one possible case, but we need more
          case '{ Array[A]((${ Varargs(Exprs(inner)) })*)(using (${ Expr(ct) }: scala.reflect.ClassTag[A])) } =>
            Some(inner.toArray(using ct))
          case _ => None
        }
      }
      given ToExpr[A] = platformSpecific.implicits.ExprCodecIsToExpr[A]
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
                  hearthAssertionFailed(
                    s"Could not figure out ClassTag[${Type.prettyPrint[A]}] - support for such cases is still experimental"
                  )
                  // given scala.reflect.ClassTag[A] = scala.reflect.ClassTag(classOf[Any]).asInstanceOf[scala.reflect.ClassTag[A]]
                  // ToExpr.ArrayToExpr[A]
                }
                .apply(value)
          }
      ExprCodec.make[Array[A]]
    }
    override def SeqExprCodec[A: ExprCodec: Type]: ExprCodec[Seq[A]] = {
      given FromExpr[A] = platformSpecific.implicits.ExprCodecIsFromExpr[A]
      given ToExpr[A] = platformSpecific.implicits.ExprCodecIsToExpr[A]
      // Default implementation of ToExpr[Seq[A]] is not correct, uses VarArgs so for e.g.
      //   Seq(1)
      // we would print expression generating...
      //   1
      given ToExpr[Seq[A]] = new {
        override def apply(value: Seq[A])(using scala.quoted.Quotes): Expr[Seq[A]] = {
          val xs = value.map(summon[ToExpr[A]].apply)
          if xs.isEmpty then '{ Seq.empty[A] } else '{ Seq(${ Varargs(xs) }*) }
        }
      }
      ExprCodec.make[Seq[A]]
    }
    override def ListExprCodec[A: ExprCodec: Type]: ExprCodec[List[A]] = {
      given FromExpr[A] = platformSpecific.implicits.ExprCodecIsFromExpr[A]
      given ToExpr[A] = platformSpecific.implicits.ExprCodecIsToExpr[A]
      ExprCodec.make[List[A]]
    }
    override lazy val NilExprCodec: ExprCodec[Nil.type] =
      ExprCodec.make[Nil.type]
    override def VectorExprCodec[A: ExprCodec: Type]: ExprCodec[Vector[A]] = {
      given FromExpr[A] = platformSpecific.implicits.ExprCodecIsFromExpr[A]
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
    override def MapExprCodec[K: ExprCodec: Type, V: ExprCodec: Type]: ExprCodec[Map[K, V]] = {
      given FromExpr[K] = platformSpecific.implicits.ExprCodecIsFromExpr[K]
      given FromExpr[V] = platformSpecific.implicits.ExprCodecIsFromExpr[V]
      given ToExpr[K] = platformSpecific.implicits.ExprCodecIsToExpr[K]
      given ToExpr[V] = platformSpecific.implicits.ExprCodecIsToExpr[V]
      ExprCodec.make[Map[K, V]]
    }
    override def SetExprCodec[A: ExprCodec: Type]: ExprCodec[Set[A]] = {
      given FromExpr[A] = platformSpecific.implicits.ExprCodecIsFromExpr[A]
      given ToExpr[A] = platformSpecific.implicits.ExprCodecIsToExpr[A]
      ExprCodec.make[Set[A]]
    }
    override def OptionExprCodec[A: ExprCodec: Type]: ExprCodec[Option[A]] = {
      given FromExpr[A] = platformSpecific.implicits.ExprCodecIsFromExpr[A]
      given ToExpr[A] = platformSpecific.implicits.ExprCodecIsToExpr[A]
      ExprCodec.make[Option[A]]
    }
    override def SomeExprCodec[A: ExprCodec: Type]: ExprCodec[Some[A]] = {
      given FromExpr[A] = platformSpecific.implicits.ExprCodecIsFromExpr[A]
      given ToExpr[A] = platformSpecific.implicits.ExprCodecIsToExpr[A]
      ExprCodec.make[Some[A]]
    }
    override lazy val NoneExprCodec: ExprCodec[None.type] =
      ExprCodec.make[None.type]
    override def EitherExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Either[L, R]] = {
      given FromExpr[L] = platformSpecific.implicits.ExprCodecIsFromExpr[L]
      given FromExpr[R] = platformSpecific.implicits.ExprCodecIsFromExpr[R]
      given ToExpr[L] = platformSpecific.implicits.ExprCodecIsToExpr[L]
      given ToExpr[R] = platformSpecific.implicits.ExprCodecIsToExpr[R]
      ExprCodec.make[Either[L, R]]
    }
    override def LeftExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Left[L, R]] = {
      given FromExpr[L] = platformSpecific.implicits.ExprCodecIsFromExpr[L]
      given ToExpr[L] = platformSpecific.implicits.ExprCodecIsToExpr[L]
      ExprCodec.make[Left[L, R]]
    }
    override def RightExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Right[L, R]] = {
      given FromExpr[R] = platformSpecific.implicits.ExprCodecIsFromExpr[R]
      given ToExpr[R] = platformSpecific.implicits.ExprCodecIsToExpr[R]
      ExprCodec.make[Right[L, R]]
    }
  }

  final override type VarArgs[A] = Expr[Seq[A]]

  object VarArgs extends VarArgsModule {
    override def toIterable[A](args: VarArgs[A]): Iterable[Expr[A]] = scala.quoted.Varargs.unapply(args).getOrElse(Nil)
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

          // val body = '{ val _ = $toSuppress; $result }
          // We're constructing:
          // '{ val fromName = bindName; val _ = fromName; ${ usage } }
          val body = Block(
            List(
              // ValDef(fromName, Some(Ref(bindName))), // not a Term, so we cannot use Expr.block
              Expr.suppressUnused(toSuppress).asTerm
            ),
            result.asTerm
          )

          val sym = TypeRepr.of[Matched].typeSymbol
          if sym.flags.is(Flags.Enum) && (sym.flags.is(Flags.JavaStatic) || sym.flags.is(Flags.StableRealizable)) then
          // Scala 3's enums' parameterless cases are vals with type erased, so we have to match them by value
          // case arg @ Enum.Value => ...
          CaseDef(Bind(name, Ident(sym.termRef)), None, body)
          // case arg : Enum.Value => ...
          else CaseDef(Bind(name, Typed(Wildcard(), TypeTree.of[Matched])), None, body)
        }
        .toVector
        .toList

      Match(uncheckedToMatch.asTerm, caseTrees).asExprOf[B]
    }

    override def partition[A, B, C](matchCase: MatchCase[A])(f: A => Either[B, C]): Either[MatchCase[B], MatchCase[C]] =
      matchCase match {
        case TypeMatch(name, expr, result) =>
          f(result) match {
            case Left(value)  => Left(TypeMatch(name, expr, value))
            case Right(value) => Right(TypeMatch(name, expr, value))
          }
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
      val name = freshTerm.defdef[A](freshName, value)
      new Scoped[Expr[A]](
        NonEmptyVector.one(DefDef(name, _ => Some(value.asTerm.changeOwner(name)))),
        Ref(name).appliedToArgss(List(Nil)).asExprOf[A]
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

    // format: off
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
    def of3[A: Type, B: Type, C: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C) => *, (Expr[A], Expr[B], Expr[C])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      new LambdaBuilder[(A, B, C) => *, (Expr[A], Expr[B], Expr[C])](
        new Mk[(A, B, C) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C) => ${ mkBody('a, 'b, 'c) } }
          }
        },
        (a1Expr, b1Expr, c1Expr)
      )
    }
    def of4[A: Type, B: Type, C: Type, D: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D) => *, (Expr[A], Expr[B], Expr[C], Expr[D])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      new LambdaBuilder[(A, B, C, D) => *, (Expr[A], Expr[B], Expr[C], Expr[D])](
        new Mk[(A, B, C, D) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D) => ${ mkBody('a, 'b, 'c, 'd) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr)
      )
    }
    def of5[A: Type, B: Type, C: Type, D: Type, E: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      new LambdaBuilder[(A, B, C, D, E) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E])](
        new Mk[(A, B, C, D, E) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E) => ${ mkBody('a, 'b, 'c, 'd, 'e) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr)
      )
    }
    def of6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      new LambdaBuilder[(A, B, C, D, E, F) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F])](
        new Mk[(A, B, C, D, E, F) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr)
      )
    }
    def of7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      new LambdaBuilder[(A, B, C, D, E, F, G) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G])](
        new Mk[(A, B, C, D, E, F, G) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr)
      )
    }
    def of8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      new LambdaBuilder[(A, B, C, D, E, F, G, H) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H])](
        new Mk[(A, B, C, D, E, F, G, H) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I])](
        new Mk[(A, B, C, D, E, F, G, H, I) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J])](
        new Mk[(A, B, C, D, E, F, G, H, I, J) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val k1Expr = Ref(k1).asExprOf[K]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val k1Expr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val l1Expr = Ref(l1).asExprOf[L]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val k1Expr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val l1Expr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val m1Expr = Ref(m1).asExprOf[M]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val k1Expr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val l1Expr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val m1Expr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val n1Expr = Ref(n1).asExprOf[N]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val k1Expr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val l1Expr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val m1Expr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val n1Expr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val o1Expr = Ref(o1).asExprOf[O]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val k1Expr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val l1Expr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val m1Expr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val n1Expr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val o1Expr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags)
      val p1Expr = Ref(p1).asExprOf[P]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm,
                ValDef(p1, Some(p.asTerm)),
                '{ val _ = $p1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr, p1Expr)
      )
    }
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val b1Expr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val c1Expr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val d1Expr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val e1Expr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val f1Expr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val g1Expr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val h1Expr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val i1Expr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val j1Expr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val k1Expr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val l1Expr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val m1Expr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val n1Expr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val o1Expr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags)
      val p1Expr = Ref(p1).asExprOf[P]
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags)
      val q1Expr = Ref(q1).asExprOf[Q]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm,
                ValDef(p1, Some(p.asTerm)),
                '{ val _ = $p1Expr }.asTerm,
                ValDef(q1, Some(q.asTerm)),
                '{ val _ = $q1Expr }.asTerm
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr, p1Expr, q1Expr)
      )
    }
    def of18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type](
        freshA: FreshName = FreshName.FromType, freshB: FreshName = FreshName.FromType, freshC: FreshName = FreshName.FromType, freshD: FreshName = FreshName.FromType, freshE: FreshName = FreshName.FromType, freshF: FreshName = FreshName.FromType, freshG: FreshName = FreshName.FromType, freshH: FreshName = FreshName.FromType, freshI: FreshName = FreshName.FromType, freshJ: FreshName = FreshName.FromType, freshK: FreshName = FreshName.FromType, freshL: FreshName = FreshName.FromType, freshM: FreshName = FreshName.FromType, freshN: FreshName = FreshName.FromType, freshO: FreshName = FreshName.FromType, freshP: FreshName = FreshName.FromType, freshQ: FreshName = FreshName.FromType, freshR: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags)
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags)
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1Expr = Ref(b1).asExprOf[B]
      val c1Expr = Ref(c1).asExprOf[C]
      val d1Expr = Ref(d1).asExprOf[D]
      val e1Expr = Ref(e1).asExprOf[E]
      val f1Expr = Ref(f1).asExprOf[F]
      val g1Expr = Ref(g1).asExprOf[G]
      val h1Expr = Ref(h1).asExprOf[H]
      val i1Expr = Ref(i1).asExprOf[I]
      val j1Expr = Ref(j1).asExprOf[J]
      val k1Expr = Ref(k1).asExprOf[K]
      val l1Expr = Ref(l1).asExprOf[L]
      val m1Expr = Ref(m1).asExprOf[M]
      val n1Expr = Ref(n1).asExprOf[N]
      val o1Expr = Ref(o1).asExprOf[O]
      val p1Expr = Ref(p1).asExprOf[P]
      val q1Expr = Ref(q1).asExprOf[Q]
      val r1Expr = Ref(r1).asExprOf[R]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm,
                ValDef(p1, Some(p.asTerm)),
                '{ val _ = $p1Expr }.asTerm,
                ValDef(q1, Some(q.asTerm)),
                '{ val _ = $q1Expr }.asTerm,
                ValDef(r1, Some(r.asTerm)),
                '{ val _ = $r1Expr }.asTerm,
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q, 'r) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr, p1Expr, q1Expr, r1Expr)
      )
    }

    def of19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type](
        freshA: FreshName = FreshName.FromType, freshB: FreshName = FreshName.FromType, freshC: FreshName = FreshName.FromType, freshD: FreshName = FreshName.FromType, freshE: FreshName = FreshName.FromType, freshF: FreshName = FreshName.FromType, freshG: FreshName = FreshName.FromType, freshH: FreshName = FreshName.FromType, freshI: FreshName = FreshName.FromType, freshJ: FreshName = FreshName.FromType, freshK: FreshName = FreshName.FromType, freshL: FreshName = FreshName.FromType, freshM: FreshName = FreshName.FromType, freshN: FreshName = FreshName.FromType, freshO: FreshName = FreshName.FromType, freshP: FreshName = FreshName.FromType, freshQ: FreshName = FreshName.FromType, freshR: FreshName = FreshName.FromType, freshS: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags)
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags)
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags)
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1Expr = Ref(b1).asExprOf[B]
      val c1Expr = Ref(c1).asExprOf[C]
      val d1Expr = Ref(d1).asExprOf[D]
      val e1Expr = Ref(e1).asExprOf[E]
      val f1Expr = Ref(f1).asExprOf[F]
      val g1Expr = Ref(g1).asExprOf[G]
      val h1Expr = Ref(h1).asExprOf[H]
      val i1Expr = Ref(i1).asExprOf[I]
      val j1Expr = Ref(j1).asExprOf[J]
      val k1Expr = Ref(k1).asExprOf[K]
      val l1Expr = Ref(l1).asExprOf[L]
      val m1Expr = Ref(m1).asExprOf[M]
      val n1Expr = Ref(n1).asExprOf[N]
      val o1Expr = Ref(o1).asExprOf[O]
      val p1Expr = Ref(p1).asExprOf[P]
      val q1Expr = Ref(q1).asExprOf[Q]
      val r1Expr = Ref(r1).asExprOf[R]
      val s1Expr = Ref(s1).asExprOf[S]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm,
                ValDef(p1, Some(p.asTerm)),
                '{ val _ = $p1Expr }.asTerm,
                ValDef(q1, Some(q.asTerm)),
                '{ val _ = $q1Expr }.asTerm,
                ValDef(r1, Some(r.asTerm)),
                '{ val _ = $r1Expr }.asTerm,
                ValDef(s1, Some(s.asTerm)),
                '{ val _ = $s1Expr }.asTerm,
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q, 'r, 's) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr, p1Expr, q1Expr, r1Expr, s1Expr)
      )
    }

    def of20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type](
        freshA: FreshName = FreshName.FromType, freshB: FreshName = FreshName.FromType, freshC: FreshName = FreshName.FromType, freshD: FreshName = FreshName.FromType, freshE: FreshName = FreshName.FromType, freshF: FreshName = FreshName.FromType, freshG: FreshName = FreshName.FromType, freshH: FreshName = FreshName.FromType, freshI: FreshName = FreshName.FromType, freshJ: FreshName = FreshName.FromType, freshK: FreshName = FreshName.FromType, freshL: FreshName = FreshName.FromType, freshM: FreshName = FreshName.FromType, freshN: FreshName = FreshName.FromType, freshO: FreshName = FreshName.FromType, freshP: FreshName = FreshName.FromType, freshQ: FreshName = FreshName.FromType, freshR: FreshName = FreshName.FromType, freshS: FreshName = FreshName.FromType, freshT: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags)
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags)
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags)
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags)
      val t1 = freshTerm.valdef[T](freshT, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1Expr = Ref(b1).asExprOf[B]
      val c1Expr = Ref(c1).asExprOf[C]
      val d1Expr = Ref(d1).asExprOf[D]
      val e1Expr = Ref(e1).asExprOf[E]
      val f1Expr = Ref(f1).asExprOf[F]
      val g1Expr = Ref(g1).asExprOf[G]
      val h1Expr = Ref(h1).asExprOf[H]
      val i1Expr = Ref(i1).asExprOf[I]
      val j1Expr = Ref(j1).asExprOf[J]
      val k1Expr = Ref(k1).asExprOf[K]
      val l1Expr = Ref(l1).asExprOf[L]
      val m1Expr = Ref(m1).asExprOf[M]
      val n1Expr = Ref(n1).asExprOf[N]
      val o1Expr = Ref(o1).asExprOf[O]
      val p1Expr = Ref(p1).asExprOf[P]
      val q1Expr = Ref(q1).asExprOf[Q]
      val r1Expr = Ref(r1).asExprOf[R]
      val s1Expr = Ref(s1).asExprOf[S]
      val t1Expr = Ref(t1).asExprOf[T]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm,
                ValDef(p1, Some(p.asTerm)),
                '{ val _ = $p1Expr }.asTerm,
                ValDef(q1, Some(q.asTerm)),
                '{ val _ = $q1Expr }.asTerm,
                ValDef(r1, Some(r.asTerm)),
                '{ val _ = $r1Expr }.asTerm,
                ValDef(s1, Some(s.asTerm)),
                '{ val _ = $s1Expr }.asTerm,
                ValDef(t1, Some(t.asTerm)),
                '{ val _ = $t1Expr }.asTerm,
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q, 'r, 's, 't) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr, p1Expr, q1Expr, r1Expr, s1Expr, t1Expr)
      )
    }

    def of21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type](
        freshA: FreshName = FreshName.FromType, freshB: FreshName = FreshName.FromType, freshC: FreshName = FreshName.FromType, freshD: FreshName = FreshName.FromType, freshE: FreshName = FreshName.FromType, freshF: FreshName = FreshName.FromType, freshG: FreshName = FreshName.FromType, freshH: FreshName = FreshName.FromType, freshI: FreshName = FreshName.FromType, freshJ: FreshName = FreshName.FromType, freshK: FreshName = FreshName.FromType, freshL: FreshName = FreshName.FromType, freshM: FreshName = FreshName.FromType, freshN: FreshName = FreshName.FromType, freshO: FreshName = FreshName.FromType, freshP: FreshName = FreshName.FromType, freshQ: FreshName = FreshName.FromType, freshR: FreshName = FreshName.FromType, freshS: FreshName = FreshName.FromType, freshT: FreshName = FreshName.FromType, freshU: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags)
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags)
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags)
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags)
      val t1 = freshTerm.valdef[T](freshT, null, Flags.EmptyFlags)
      val u1 = freshTerm.valdef[U](freshU, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1Expr = Ref(b1).asExprOf[B]
      val c1Expr = Ref(c1).asExprOf[C]
      val d1Expr = Ref(d1).asExprOf[D]
      val e1Expr = Ref(e1).asExprOf[E]
      val f1Expr = Ref(f1).asExprOf[F]
      val g1Expr = Ref(g1).asExprOf[G]
      val h1Expr = Ref(h1).asExprOf[H]
      val i1Expr = Ref(i1).asExprOf[I]
      val j1Expr = Ref(j1).asExprOf[J]
      val k1Expr = Ref(k1).asExprOf[K]
      val l1Expr = Ref(l1).asExprOf[L]
      val m1Expr = Ref(m1).asExprOf[M]
      val n1Expr = Ref(n1).asExprOf[N]
      val o1Expr = Ref(o1).asExprOf[O]
      val p1Expr = Ref(p1).asExprOf[P]
      val q1Expr = Ref(q1).asExprOf[Q]
      val r1Expr = Ref(r1).asExprOf[R]
      val s1Expr = Ref(s1).asExprOf[S]
      val t1Expr = Ref(t1).asExprOf[T]
      val u1Expr = Ref(u1).asExprOf[U]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm,
                ValDef(p1, Some(p.asTerm)),
                '{ val _ = $p1Expr }.asTerm,
                ValDef(q1, Some(q.asTerm)),
                '{ val _ = $q1Expr }.asTerm,
                ValDef(r1, Some(r.asTerm)),
                '{ val _ = $r1Expr }.asTerm,
                ValDef(s1, Some(s.asTerm)),
                '{ val _ = $s1Expr }.asTerm,
                ValDef(t1, Some(t.asTerm)),
                '{ val _ = $t1Expr }.asTerm,
                ValDef(u1, Some(u.asTerm)),
                '{ val _ = $u1Expr }.asTerm,
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q, 'r, 's, 't, 'u) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr, p1Expr, q1Expr, r1Expr, s1Expr, t1Expr, u1Expr)
      )
    }

    def of22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type](
        freshA: FreshName = FreshName.FromType, freshB: FreshName = FreshName.FromType, freshC: FreshName = FreshName.FromType, freshD: FreshName = FreshName.FromType, freshE: FreshName = FreshName.FromType, freshF: FreshName = FreshName.FromType, freshG: FreshName = FreshName.FromType, freshH: FreshName = FreshName.FromType, freshI: FreshName = FreshName.FromType, freshJ: FreshName = FreshName.FromType, freshK: FreshName = FreshName.FromType, freshL: FreshName = FreshName.FromType, freshM: FreshName = FreshName.FromType, freshN: FreshName = FreshName.FromType, freshO: FreshName = FreshName.FromType, freshP: FreshName = FreshName.FromType, freshQ: FreshName = FreshName.FromType, freshR: FreshName = FreshName.FromType, freshS: FreshName = FreshName.FromType, freshT: FreshName = FreshName.FromType, freshU: FreshName = FreshName.FromType, freshV: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V])] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags)
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags)
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags)
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags)
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags)
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags)
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags)
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags)
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags)
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags)
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags)
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags)
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags)
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags)
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags)
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags)
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags)
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags)
      val t1 = freshTerm.valdef[T](freshT, null, Flags.EmptyFlags)
      val u1 = freshTerm.valdef[U](freshU, null, Flags.EmptyFlags)
      val v1 = freshTerm.valdef[V](freshV, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      val b1Expr = Ref(b1).asExprOf[B]
      val c1Expr = Ref(c1).asExprOf[C]
      val d1Expr = Ref(d1).asExprOf[D]
      val e1Expr = Ref(e1).asExprOf[E]
      val f1Expr = Ref(f1).asExprOf[F]
      val g1Expr = Ref(g1).asExprOf[G]
      val h1Expr = Ref(h1).asExprOf[H]
      val i1Expr = Ref(i1).asExprOf[I]
      val j1Expr = Ref(j1).asExprOf[J]
      val k1Expr = Ref(k1).asExprOf[K]
      val l1Expr = Ref(l1).asExprOf[L]
      val m1Expr = Ref(m1).asExprOf[M]
      val n1Expr = Ref(n1).asExprOf[N]
      val o1Expr = Ref(o1).asExprOf[O]
      val p1Expr = Ref(p1).asExprOf[P]
      val q1Expr = Ref(q1).asExprOf[Q]
      val r1Expr = Ref(r1).asExprOf[R]
      val s1Expr = Ref(s1).asExprOf[S]
      val t1Expr = Ref(t1).asExprOf[T]
      val u1Expr = Ref(u1).asExprOf[U]
      val v1Expr = Ref(v1).asExprOf[V]
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => To] = {

            def mkBody(a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U], v: Expr[V]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm,
                ValDef(b1, Some(b.asTerm)),
                '{ val _ = $b1Expr }.asTerm,
                ValDef(c1, Some(c.asTerm)),
                '{ val _ = $c1Expr }.asTerm,
                ValDef(d1, Some(d.asTerm)),
                '{ val _ = $d1Expr }.asTerm,
                ValDef(e1, Some(e.asTerm)),
                '{ val _ = $e1Expr }.asTerm,
                ValDef(f1, Some(f.asTerm)),
                '{ val _ = $f1Expr }.asTerm,
                ValDef(g1, Some(g.asTerm)),
                '{ val _ = $g1Expr }.asTerm,
                ValDef(h1, Some(h.asTerm)),
                '{ val _ = $h1Expr }.asTerm,
                ValDef(i1, Some(i.asTerm)),
                '{ val _ = $i1Expr }.asTerm,
                ValDef(j1, Some(j.asTerm)),
                '{ val _ = $j1Expr }.asTerm,
                ValDef(k1, Some(k.asTerm)),
                '{ val _ = $k1Expr }.asTerm,
                ValDef(l1, Some(l.asTerm)),
                '{ val _ = $l1Expr }.asTerm,
                ValDef(m1, Some(m.asTerm)),
                '{ val _ = $m1Expr }.asTerm,
                ValDef(n1, Some(n.asTerm)),
                '{ val _ = $n1Expr }.asTerm,
                ValDef(o1, Some(o.asTerm)),
                '{ val _ = $o1Expr }.asTerm,
                ValDef(p1, Some(p.asTerm)),
                '{ val _ = $p1Expr }.asTerm,
                ValDef(q1, Some(q.asTerm)),
                '{ val _ = $q1Expr }.asTerm,
                ValDef(r1, Some(r.asTerm)),
                '{ val _ = $r1Expr }.asTerm,
                ValDef(s1, Some(s.asTerm)),
                '{ val _ = $s1Expr }.asTerm,
                ValDef(t1, Some(t.asTerm)),
                '{ val _ = $t1Expr }.asTerm,
                ValDef(u1, Some(u.asTerm)),
                '{ val _ = $u1Expr }.asTerm,
                ValDef(v1, Some(v.asTerm)),
                '{ val _ = $v1Expr }.asTerm,
              ),
              body.asTerm
            ).asExprOf[To]

            '{ (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V) => ${ mkBody('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q, 'r, 's, 't, 'u, 'v) } }
          }
        },
        (a1Expr, b1Expr, c1Expr, d1Expr, e1Expr, f1Expr, g1Expr, h1Expr, i1Expr, j1Expr, k1Expr, l1Expr, m1Expr, n1Expr, o1Expr, p1Expr, q1Expr, r1Expr, s1Expr, t1Expr, u1Expr, v1Expr)
      )
    }
    // format: on

    override def build[From[_], To: Type](builder: LambdaBuilder[From, Expr[To]]): Expr[From[To]] =
      builder.mk(builder.value)

    override def partition[From[_], A, B, C](promise: LambdaBuilder[From, A])(
        f: A => Either[B, C]
    ): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]] =
      f(promise.value) match {
        case Left(value)  => Left(new LambdaBuilder[From, B](promise.mk, value))
        case Right(value) => Right(new LambdaBuilder[From, C](promise.mk, value))
      }

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
