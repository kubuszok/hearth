package hearth
package typed

import hearth.fp.data.NonEmptyVector
import hearth.fp.syntax.*

import scala.collection.immutable.ListMap

trait ExprsScala3 extends Exprs { this: MacroCommonsScala3 =>

  import scala.quoted.{Exprs, FromExpr, Quotes, ToExpr, Varargs}

  final override type Expr[A] = scala.quoted.Expr[A]

  object Expr extends ExprModule {
    import quotes.*, quotes.reflect.*

    object platformSpecific {

      def withQuotes[A](thunk: scala.quoted.Quotes ?=> A): A =
        thunk(using CrossQuotes.ctx[Quotes])

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

        def valdef[A: Type](
            freshName: FreshName,
            expr: Expr[A],
            flags: Flags,
            owner: Symbol = Symbol.spliceOwner
        ): Symbol =
          Symbol.newVal(owner, apply[A](freshName, expr), TypeRepr.of[A], flags, Symbol.noSymbol)

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

    override def plainPrint[A](expr: Expr[A]): String = removeMacroSuffix(expr.asTerm.show(using Printer.TreeCode))
    override def prettyPrint[A](expr: Expr[A]): String = removeMacroSuffix(expr.asTerm.show(using Printer.TreeAnsiCode))

    override def plainAST[A](expr: Expr[A]): String = expr.asTerm.show(using FormattedTreeStructure)
    override def prettyAST[A](expr: Expr[A]): String = expr.asTerm.show(using FormattedTreeStructureAnsi)

    override def summonImplicit[A: Type]: SummoningResult[A] = parseImplicitSearchResult {
      Implicits.search(TypeRepr.of[A])
    }
    override def summonImplicitIgnoring[A: Type](excluded: UntypedMethod*): SummoningResult[A] =
      searchIgnoringOption.fold[SummoningResult[A]] {
        // $COVERAGE-OFF$
        hearthRequirementFailed(
          """Expr.summonImplicitIgnoring on Scala 3 relies on Implicits.searchIgnoring method, which is available since Scala 3.7.0.
            |Use Environment.currentScalaVersion to check if this method is available, or raise the minimum required Scala version for the library.""".stripMargin
        )
        // $COVERAGE-ON$
      } { searchIgnoring =>
        parseImplicitSearchResult {
          searchIgnoring
            .invoke(
              Implicits,
              /* tpe = */ TypeRepr.of[A],
              /* ignored = */ excluded.map(_.symbol)
            )
            .asInstanceOf[ImplicitSearchResult]
        }
      }
    private def parseImplicitSearchResult[A: Type](thunk: ImplicitSearchResult): SummoningResult[A] =
      thunk match {
        case iss: ImplicitSearchSuccess => SummoningResult.Found(iss.tree.asExprOf[A])
        case isf: ImplicitSearchFailure =>
          // TODO: consider parsing the message to get the list of ambiguous implicit values
          isf match {
            case _: AmbiguousImplicits => SummoningResult.Ambiguous(Type[A])
            case _: DivergingImplicit  => SummoningResult.Diverging(Type[A])
            case _                     => SummoningResult.NotFound(Type[A])
          }
      }
    // $COVERAGE-OFF$
    private lazy val searchIgnoringOption =
      quotes.reflect.Implicits.getClass.getMethods.find(_.getName == "searchIgnoring")
    // $COVERAGE-ON$

    override def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      expr.asInstanceOf[Expr[B]] // check that A <:< B without upcasting in code (Scala 3 should get away without it)
    }

    override def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit] = '{ val _ = $expr; () }

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
                // $COVERAGE-OFF$
                .getOrElse {
                  hearthAssertionFailed(
                    s"Could not figure out ClassTag[${Type.prettyPrint[A]}] - support for such cases is still experimental"
                  )
                  // given scala.reflect.ClassTag[A] = scala.reflect.ClassTag(classOf[Any]).asInstanceOf[scala.reflect.ClassTag[A]]
                  // ToExpr.ArrayToExpr[A]
                }
                // $COVERAGE-ON$
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
    override def from[A: Type](iterable: Iterable[Expr[A]]): Expr[Seq[A]] = scala.quoted.Varargs(iterable.toSeq)
  }

  import Expr.platformSpecific.*

  sealed trait MatchCase[A] extends Product with Serializable

  object MatchCase extends MatchCaseModule {
    import quotes.*, quotes.reflect.{MatchCase as _, *}

    final private case class TypeMatch[A](name: Symbol, expr: Expr_??, result: A) extends MatchCase[A]

    override def typeMatch[A: Type](freshName: FreshName): MatchCase[Expr[A]] = {
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

  final class ValDefs[A] private[typed] (
      private val definitions: Vector[quotes.reflect.Statement],
      private val value: A
  )

  object ValDefs extends ValDefsModule {
    import quotes.*, quotes.reflect.*

    override def createVal[A: Type](value: Expr[A], freshName: FreshName): ValDefs[Expr[A]] = {
      val name = freshTerm.valdef[A](freshName, value, Flags.EmptyFlags)
      new ValDefs[Expr[A]](
        Vector(ValDef(name, Some(value.asTerm.changeOwner(name)))),
        Ref(name).asExprOf[A]
      )
    }
    override def createVar[A: Type](
        initialValue: Expr[A],
        freshName: FreshName
    ): ValDefs[(Expr[A], Expr[A] => Expr[Unit])] = {
      val name = freshTerm.valdef[A](freshName, initialValue, Flags.Mutable)
      new ValDefs[(Expr[A], Expr[A] => Expr[Unit])](
        Vector(ValDef(name, Some(initialValue.asTerm.changeOwner(name)))),
        (Ref(name).asExprOf[A], expr => Assign(Ref(name), expr.asTerm).asExprOf[Unit])
      )
    }
    override def createLazy[A: Type](value: Expr[A], freshName: FreshName): ValDefs[Expr[A]] = {
      val name = freshTerm.valdef[A](freshName, value, Flags.Lazy)
      new ValDefs[Expr[A]](
        Vector(ValDef(name, Some(value.asTerm.changeOwner(name)))),
        Ref(name).asExprOf[A]
      )
    }
    override def createDef[A: Type](value: Expr[A], freshName: FreshName): ValDefs[Expr[A]] = {
      val name = freshTerm.defdef[A](freshName, value)
      new ValDefs[Expr[A]](
        Vector(DefDef(name, _ => Some(value.asTerm.changeOwner(name)))),
        Ref(name).appliedToArgss(List(Nil)).asExprOf[A]
      )
    }

    override def partition[A, B, C](scoped: ValDefs[A])(f: A => Either[B, C]): Either[ValDefs[B], ValDefs[C]] =
      f(scoped.value) match {
        case Left(value)  => Left(new ValDefs[B](scoped.definitions, value))
        case Right(value) => Right(new ValDefs[C](scoped.definitions, value))
      }

    override def closeScope[A](scoped: ValDefs[Expr[A]]): Expr[A] =
      if scoped.definitions.isEmpty then scoped.value
      else
        Block(scoped.definitions.toList, scoped.value.asTerm).asExpr.asInstanceOf[Expr[A]]

    override val traverse: fp.ApplicativeTraverse[ValDefs] = new fp.ApplicativeTraverse[ValDefs] {

      override def pure[A](a: A): ValDefs[A] = new ValDefs[A](Vector.empty, a)

      override def map2[A, B, C](fa: ValDefs[A], fb: => ValDefs[B])(f: (A, B) => C): ValDefs[C] =
        new ValDefs[C](fa.definitions ++ fb.definitions, f(fa.value, fb.value))

      override def traverse[G[_]: fp.Applicative, A, B](fa: ValDefs[A])(f: A => G[B]): G[ValDefs[B]] =
        f(fa.value).map(b => new ValDefs[B](fa.definitions, b))

      override def parTraverse[G[_]: fp.Parallel, A, B](fa: ValDefs[A])(f: A => G[B]): G[ValDefs[B]] =
        f(fa.value).map(b => new ValDefs[B](fa.definitions, b))
    }
  }

  final class ValDefBuilder[Signature, Returned, Value] private (
      private val mk: ValDefBuilder.Mk[Signature, Returned],
      private val value: Value
  )

  object ValDefBuilder extends ValDefBuilderModule {
    import quotes.*, quotes.reflect.*

    sealed private[typed] trait Mk[Signature, Returned] private[typed] {

      def build(body: Expr[Returned]): ValDefs[Signature]

      def buildCached(cache: ValDefsCache, key: String, body: Expr[Returned]): ValDefsCache

      def forwardDeclare(cache: ValDefsCache, key: String): ValDefsCache
    }

    final private[typed] class MkValDef[Signature, Returned] private[typed] (
        signature: Signature,
        mkKey: String => ValDefsCache.Key,
        buildValDef: Expr[Returned] => Statement
    ) extends Mk[Signature, Returned] {

      def build(body: Expr[Returned]): ValDefs[Signature] =
        new ValDefs[Signature](Vector(buildValDef(body)), signature)

      def buildCached(cache: ValDefsCache, key: String, body: Expr[Returned]): ValDefsCache =
        cache.set(mkKey(key), signature, buildValDef(body))

      def forwardDeclare(cache: ValDefsCache, key: String): ValDefsCache =
        cache.forwardDeclare(mkKey(key), signature)
    }

    final private[typed] class MkVar[Signature, Returned] private[typed] (
        getter: Signature,
        setter: Expr[Returned] => Expr[Unit],
        mkGetterKey: String => ValDefsCache.Key,
        mkSetterKey: String => ValDefsCache.Key,
        buildVar: Expr[Returned] => Statement
    ) extends Mk[Signature, Returned] {

      def build(body: Expr[Returned]): ValDefs[Signature] =
        new ValDefs[Signature](Vector(buildVar(body)), getter)

      def buildCached(cache: ValDefsCache, key: String, body: Expr[Returned]): ValDefsCache =
        cache.set(mkGetterKey(key), getter, buildVar(body)).set(mkSetterKey(key), setter, null.asInstanceOf[Statement])

      def forwardDeclare(cache: ValDefsCache, key: String): ValDefsCache =
        cache.forwardDeclare(mkGetterKey(key), getter).forwardDeclare(mkSetterKey(key), setter)
    }

    override def ofVal[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Unit] = {
      val name = freshTerm.valdef[Returned](freshName, null, Flags.EmptyFlags)
      val self = Ref(name).asExprOf[Returned]
      new ValDefBuilder[Expr[Returned], Returned, Unit](
        new MkValDef[Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => ValDef(name, Some(body.asTerm.changeOwner(name)))
        ),
        ()
      )
    }
    override def ofVar[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned] => Expr[Unit]] = {
      val name = freshTerm.valdef[Returned](freshName, null, Flags.Mutable)
      val self = Ref(name).asExprOf[Returned]
      val setter = (body: Expr[Returned]) => Assign(Ref(name), body.asTerm).asExprOf[Unit]
      new ValDefBuilder[Expr[Returned], Returned, Expr[Returned] => Expr[Unit]](
        new MkVar[Expr[Returned], Returned](
          getter = self,
          setter = setter,
          mkGetterKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          mkSetterKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[Returned].asUntyped), Type[Unit].asUntyped),
          buildVar = (body: Expr[Returned]) => ValDef(name, Some(body.asTerm.changeOwner(name)))
        ),
        setter
      )
    }
    override def ofLazy[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned]] = {
      val name = freshTerm.valdef[Returned](freshName, null, Flags.Lazy)
      val self = Ref(name).asExprOf[Returned]
      new ValDefBuilder[Expr[Returned], Returned, Expr[Returned]](
        new MkValDef[Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => ValDef(name, Some(body.asTerm.changeOwner(name)))
        ),
        self
      )
    }

    // format: off
    override def ofDef0[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned]] = {
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List())(_ => List(), _ => TypeRepr.of[Returned])
      )
      val self = Ref(name).appliedToArgss(List(Nil)).asExprOf[Returned]
      new ValDefBuilder[Expr[Returned], Returned, Expr[Returned]](
        new MkValDef[Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(Nil) =>
                  Some {
                    body.asTerm.changeOwner(name)
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 0 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        self
      )
    }
    override def ofDef1[A: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName
    ): ValDefBuilder[Expr[A] => Expr[Returned], Returned, (Expr[A] => Expr[Returned], Expr[A])] = {
      val a0 = freshTerm[A](freshA, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0))(_ => List(TypeRepr.of[A]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A]) => Ref(name).appliedToArgss(List(List(a.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      new ValDefBuilder[Expr[A] => Expr[Returned], Returned, (Expr[A] => Expr[Returned], Expr[A])](
        new MkValDef[Expr[A] => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 1 Term argument, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, aExpr)
      )
    }
    override def ofDef2[A: Type, B: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B]) => Expr[Returned], Returned, ((Expr[A], Expr[B]) => Expr[Returned], (Expr[A], Expr[B]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0))(_ => List(TypeRepr.of[A], TypeRepr.of[B]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      new ValDefBuilder[(Expr[A], Expr[B]) => Expr[Returned], Returned, ((Expr[A], Expr[B]) => Expr[Returned], (Expr[A], Expr[B]))](
        new MkValDef[(Expr[A], Expr[B]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 2 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr))
      )
    }
    override def ofDef3[A: Type, B: Type, C: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C]) => Expr[Returned], (Expr[A], Expr[B], Expr[C]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C]) => Expr[Returned], (Expr[A], Expr[B], Expr[C]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 3 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr))
      )
    }
    override def ofDef4[A: Type, B: Type, C: Type, D: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 4 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr))
      )
    }
    override def ofDef5[A: Type, B: Type, C: Type, D: Type, E: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 5 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr))
      )
    }
    override def ofDef6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 6 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr))
      )
    }
    override def ofDef7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 7 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr))
      )
    }
    override def ofDef8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 8 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr))
      )
    }
    override def ofDef9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 9 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr))
      )
    }
    override def ofDef10[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 10 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr))
      )
    }
    override def ofDef11[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 11 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr))
      )
    }
    override def ofDef12[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 12 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr))
      )
    }
    override def ofDef13[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 13 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr))
      )
    }
    override def ofDef14[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 14 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr))
      )
    }
    override def ofDef15[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 15 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr))
      )
    }
    override def ofDef16[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val p0 = freshTerm[P](freshP, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O], TypeRepr.of[P]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm, p.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags, name)
      val pExpr = Ref(p1).asExprOf[P]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term, p: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm,
                        ValDef(p1, Some(p)),
                        '{ val _ = $pExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 16 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr, pExpr))
      )
    }
    override def ofDef17[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName,
        freshQ: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val p0 = freshTerm[P](freshP, null)
      val q0 = freshTerm[Q](freshQ, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0, q0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O], TypeRepr.of[P], TypeRepr.of[Q]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm, p.asTerm, q.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags, name)
      val pExpr = Ref(p1).asExprOf[P]
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags, name)
      val qExpr = Ref(q1).asExprOf[Q]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term, p: Term, q: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm,
                        ValDef(p1, Some(p)),
                        '{ val _ = $pExpr }.asTerm,
                        ValDef(q1, Some(q)),
                        '{ val _ = $qExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 17 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr, pExpr, qExpr))
      )
    }
    override def ofDef18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName,
        freshQ: FreshName,
        freshR: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val p0 = freshTerm[P](freshP, null)
      val q0 = freshTerm[Q](freshQ, null)
      val r0 = freshTerm[R](freshR, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0, q0, r0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O], TypeRepr.of[P], TypeRepr.of[Q], TypeRepr.of[R]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm, p.asTerm, q.asTerm, r.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags, name)
      val pExpr = Ref(p1).asExprOf[P]
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags, name)
      val qExpr = Ref(q1).asExprOf[Q]
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags, name)
      val rExpr = Ref(r1).asExprOf[R]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term, p: Term, q: Term, r: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm,
                        ValDef(p1, Some(p)),
                        '{ val _ = $pExpr }.asTerm,
                        ValDef(q1, Some(q)),
                        '{ val _ = $qExpr }.asTerm,
                        ValDef(r1, Some(r)),
                        '{ val _ = $rExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 18 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr, pExpr, qExpr, rExpr))
      )
    }
    override def ofDef19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName,
        freshQ: FreshName,
        freshR: FreshName,
        freshS: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val p0 = freshTerm[P](freshP, null)
      val q0 = freshTerm[Q](freshQ, null)
      val r0 = freshTerm[R](freshR, null)
      val s0 = freshTerm[S](freshS, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0, q0, r0, s0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O], TypeRepr.of[P], TypeRepr.of[Q], TypeRepr.of[R], TypeRepr.of[S]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm, p.asTerm, q.asTerm, r.asTerm, s.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags, name)
      val pExpr = Ref(p1).asExprOf[P]
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags, name)
      val qExpr = Ref(q1).asExprOf[Q]
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags, name)
      val rExpr = Ref(r1).asExprOf[R]
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags, name)
      val sExpr = Ref(s1).asExprOf[S]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term, p: Term, q: Term, r: Term, s: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm,
                        ValDef(p1, Some(p)),
                        '{ val _ = $pExpr }.asTerm,
                        ValDef(q1, Some(q)),
                        '{ val _ = $qExpr }.asTerm,
                        ValDef(r1, Some(r)),
                        '{ val _ = $rExpr }.asTerm,
                        ValDef(s1, Some(s)),
                        '{ val _ = $sExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 19 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr, pExpr, qExpr, rExpr, sExpr))
      )
    }
    override def ofDef20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName,
        freshQ: FreshName,
        freshR: FreshName,
        freshS: FreshName,
        freshT: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val p0 = freshTerm[P](freshP, null)
      val q0 = freshTerm[Q](freshQ, null)
      val r0 = freshTerm[R](freshR, null)
      val s0 = freshTerm[S](freshS, null)
      val t0 = freshTerm[T](freshT, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0, q0, r0, s0, t0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O], TypeRepr.of[P], TypeRepr.of[Q], TypeRepr.of[R], TypeRepr.of[S], TypeRepr.of[T]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm, p.asTerm, q.asTerm, r.asTerm, s.asTerm, t.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags, name)
      val pExpr = Ref(p1).asExprOf[P]
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags, name)
      val qExpr = Ref(q1).asExprOf[Q]
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags, name)
      val rExpr = Ref(r1).asExprOf[R]
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags, name)
      val sExpr = Ref(s1).asExprOf[S]
      val t1 = freshTerm.valdef[T](freshT, null, Flags.EmptyFlags, name)
      val tExpr = Ref(t1).asExprOf[T]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term, p: Term, q: Term, r: Term, s: Term, t: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm,
                        ValDef(p1, Some(p)),
                        '{ val _ = $pExpr }.asTerm,
                        ValDef(q1, Some(q)),
                        '{ val _ = $qExpr }.asTerm,
                        ValDef(r1, Some(r)),
                        '{ val _ = $rExpr }.asTerm,
                        ValDef(s1, Some(s)),
                        '{ val _ = $sExpr }.asTerm,
                        ValDef(t1, Some(t)),
                        '{ val _ = $tExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 20 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr, pExpr, qExpr, rExpr, sExpr, tExpr))
      )
    }
    override def ofDef21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName,
        freshQ: FreshName,
        freshR: FreshName,
        freshS: FreshName,
        freshT: FreshName,
        freshU: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val p0 = freshTerm[P](freshP, null)
      val q0 = freshTerm[Q](freshQ, null)
      val r0 = freshTerm[R](freshR, null)
      val s0 = freshTerm[S](freshS, null)
      val t0 = freshTerm[T](freshT, null)
      val u0 = freshTerm[U](freshU, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0, q0, r0, s0, t0, u0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O], TypeRepr.of[P], TypeRepr.of[Q], TypeRepr.of[R], TypeRepr.of[S], TypeRepr.of[T], TypeRepr.of[U]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm, p.asTerm, q.asTerm, r.asTerm, s.asTerm, t.asTerm, u.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags, name)
      val pExpr = Ref(p1).asExprOf[P]
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags, name)
      val qExpr = Ref(q1).asExprOf[Q]
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags, name)
      val rExpr = Ref(r1).asExprOf[R]
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags, name)
      val sExpr = Ref(s1).asExprOf[S]
      val t1 = freshTerm.valdef[T](freshT, null, Flags.EmptyFlags, name)
      val tExpr = Ref(t1).asExprOf[T]
      val u1 = freshTerm.valdef[U](freshU, null, Flags.EmptyFlags, name)
      val uExpr = Ref(u1).asExprOf[U]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped, Type[U].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term, p: Term, q: Term, r: Term, s: Term, t: Term, u: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm,
                        ValDef(p1, Some(p)),
                        '{ val _ = $pExpr }.asTerm,
                        ValDef(q1, Some(q)),
                        '{ val _ = $qExpr }.asTerm,
                        ValDef(r1, Some(r)),
                        '{ val _ = $rExpr }.asTerm,
                        ValDef(s1, Some(s)),
                        '{ val _ = $sExpr }.asTerm,
                        ValDef(t1, Some(t)),
                        '{ val _ = $tExpr }.asTerm,
                        ValDef(u1, Some(u)),
                        '{ val _ = $uExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 21 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr, pExpr, qExpr, rExpr, sExpr, tExpr, uExpr))
      )
    }
    override def ofDef22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName,
        freshQ: FreshName,
        freshR: FreshName,
        freshS: FreshName,
        freshT: FreshName,
        freshU: FreshName,
        freshV: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]))] = {
      val a0 = freshTerm[A](freshA, null)
      val b0 = freshTerm[B](freshB, null)
      val c0 = freshTerm[C](freshC, null)
      val d0 = freshTerm[D](freshD, null)
      val e0 = freshTerm[E](freshE, null)
      val f0 = freshTerm[F](freshF, null)
      val g0 = freshTerm[G](freshG, null)
      val h0 = freshTerm[H](freshH, null)
      val i0 = freshTerm[I](freshI, null)
      val j0 = freshTerm[J](freshJ, null)
      val k0 = freshTerm[K](freshK, null)
      val l0 = freshTerm[L](freshL, null)
      val m0 = freshTerm[M](freshM, null)
      val n0 = freshTerm[N](freshN, null)
      val o0 = freshTerm[O](freshO, null)
      val p0 = freshTerm[P](freshP, null)
      val q0 = freshTerm[Q](freshQ, null)
      val r0 = freshTerm[R](freshR, null)
      val s0 = freshTerm[S](freshS, null)
      val t0 = freshTerm[T](freshT, null)
      val u0 = freshTerm[U](freshU, null)
      val v0 = freshTerm[V](freshV, null)
      val name = Symbol.newMethod(
        Symbol.spliceOwner,
        freshTerm[Returned](freshName, null),
        MethodType(List(a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0, q0, r0, s0, t0, u0, v0))(_ => List(TypeRepr.of[A], TypeRepr.of[B], TypeRepr.of[C], TypeRepr.of[D], TypeRepr.of[E], TypeRepr.of[F], TypeRepr.of[G], TypeRepr.of[H], TypeRepr.of[I], TypeRepr.of[J], TypeRepr.of[K], TypeRepr.of[L], TypeRepr.of[M], TypeRepr.of[N], TypeRepr.of[O], TypeRepr.of[P], TypeRepr.of[Q], TypeRepr.of[R], TypeRepr.of[S], TypeRepr.of[T], TypeRepr.of[U], TypeRepr.of[V]), _ => TypeRepr.of[Returned])
      )
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U], v: Expr[V]) => Ref(name).appliedToArgss(List(List(a.asTerm, b.asTerm, c.asTerm, d.asTerm, e.asTerm, f.asTerm, g.asTerm, h.asTerm, i.asTerm, j.asTerm, k.asTerm, l.asTerm, m.asTerm, n.asTerm, o.asTerm, p.asTerm, q.asTerm, r.asTerm, s.asTerm, t.asTerm, u.asTerm, v.asTerm))).asExprOf[Returned]
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags, name)
      val aExpr = Ref(a1).asExprOf[A]
      val b1 = freshTerm.valdef[B](freshB, null, Flags.EmptyFlags, name)
      val bExpr = Ref(b1).asExprOf[B]
      val c1 = freshTerm.valdef[C](freshC, null, Flags.EmptyFlags, name)
      val cExpr = Ref(c1).asExprOf[C]
      val d1 = freshTerm.valdef[D](freshD, null, Flags.EmptyFlags, name)
      val dExpr = Ref(d1).asExprOf[D]
      val e1 = freshTerm.valdef[E](freshE, null, Flags.EmptyFlags, name)
      val eExpr = Ref(e1).asExprOf[E]
      val f1 = freshTerm.valdef[F](freshF, null, Flags.EmptyFlags, name)
      val fExpr = Ref(f1).asExprOf[F]
      val g1 = freshTerm.valdef[G](freshG, null, Flags.EmptyFlags, name)
      val gExpr = Ref(g1).asExprOf[G]
      val h1 = freshTerm.valdef[H](freshH, null, Flags.EmptyFlags, name)
      val hExpr = Ref(h1).asExprOf[H]
      val i1 = freshTerm.valdef[I](freshI, null, Flags.EmptyFlags, name)
      val iExpr = Ref(i1).asExprOf[I]
      val j1 = freshTerm.valdef[J](freshJ, null, Flags.EmptyFlags, name)
      val jExpr = Ref(j1).asExprOf[J]
      val k1 = freshTerm.valdef[K](freshK, null, Flags.EmptyFlags, name)
      val kExpr = Ref(k1).asExprOf[K]
      val l1 = freshTerm.valdef[L](freshL, null, Flags.EmptyFlags, name)
      val lExpr = Ref(l1).asExprOf[L]
      val m1 = freshTerm.valdef[M](freshM, null, Flags.EmptyFlags, name)
      val mExpr = Ref(m1).asExprOf[M]
      val n1 = freshTerm.valdef[N](freshN, null, Flags.EmptyFlags, name)
      val nExpr = Ref(n1).asExprOf[N]
      val o1 = freshTerm.valdef[O](freshO, null, Flags.EmptyFlags, name)
      val oExpr = Ref(o1).asExprOf[O]
      val p1 = freshTerm.valdef[P](freshP, null, Flags.EmptyFlags, name)
      val pExpr = Ref(p1).asExprOf[P]
      val q1 = freshTerm.valdef[Q](freshQ, null, Flags.EmptyFlags, name)
      val qExpr = Ref(q1).asExprOf[Q]
      val r1 = freshTerm.valdef[R](freshR, null, Flags.EmptyFlags, name)
      val rExpr = Ref(r1).asExprOf[R]
      val s1 = freshTerm.valdef[S](freshS, null, Flags.EmptyFlags, name)
      val sExpr = Ref(s1).asExprOf[S]
      val t1 = freshTerm.valdef[T](freshT, null, Flags.EmptyFlags, name)
      val tExpr = Ref(t1).asExprOf[T]
      val u1 = freshTerm.valdef[U](freshU, null, Flags.EmptyFlags, name)
      val uExpr = Ref(u1).asExprOf[U]
      val v1 = freshTerm.valdef[V](freshV, null, Flags.EmptyFlags, name)
      val vExpr = Ref(v1).asExprOf[V]
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped, Type[U].asUntyped, Type[V].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) =>
            DefDef(
              name,
              {
                case List(List(a: Term, b: Term, c: Term, d: Term, e: Term, f: Term, g: Term, h: Term, i: Term, j: Term, k: Term, l: Term, m: Term, n: Term, o: Term, p: Term, q: Term, r: Term, s: Term, t: Term, u: Term, v: Term)) =>
                  Some {
                    Block(
                      List(
                        ValDef(a1, Some(a)),
                        '{ val _ = $aExpr }.asTerm,
                        ValDef(b1, Some(b)),
                        '{ val _ = $bExpr }.asTerm,
                        ValDef(c1, Some(c)),
                        '{ val _ = $cExpr }.asTerm,
                        ValDef(d1, Some(d)),
                        '{ val _ = $dExpr }.asTerm,
                        ValDef(e1, Some(e)),
                        '{ val _ = $eExpr }.asTerm,
                        ValDef(f1, Some(f)),
                        '{ val _ = $fExpr }.asTerm,
                        ValDef(g1, Some(g)),
                        '{ val _ = $gExpr }.asTerm,
                        ValDef(h1, Some(h)),
                        '{ val _ = $hExpr }.asTerm,
                        ValDef(i1, Some(i)),
                        '{ val _ = $iExpr }.asTerm,
                        ValDef(j1, Some(j)),
                        '{ val _ = $jExpr }.asTerm,
                        ValDef(k1, Some(k)),
                        '{ val _ = $kExpr }.asTerm,
                        ValDef(l1, Some(l)),
                        '{ val _ = $lExpr }.asTerm,
                        ValDef(m1, Some(m)),
                        '{ val _ = $mExpr }.asTerm,
                        ValDef(n1, Some(n)),
                        '{ val _ = $nExpr }.asTerm,
                        ValDef(o1, Some(o)),
                        '{ val _ = $oExpr }.asTerm,
                        ValDef(p1, Some(p)),
                        '{ val _ = $pExpr }.asTerm,
                        ValDef(q1, Some(q)),
                        '{ val _ = $qExpr }.asTerm,
                        ValDef(r1, Some(r)),
                        '{ val _ = $rExpr }.asTerm,
                        ValDef(s1, Some(s)),
                        '{ val _ = $sExpr }.asTerm,
                        ValDef(t1, Some(t)),
                        '{ val _ = $tExpr }.asTerm,
                        ValDef(u1, Some(u)),
                        '{ val _ = $uExpr }.asTerm,
                        ValDef(v1, Some(v)),
                        '{ val _ = $vExpr }.asTerm
                      ),
                      body.asTerm.changeOwner(name)
                    )
                  }
                // $COVERAGE-OFF$
                case args =>
                  val preview =
                    args.map(_.map(_.show(using FormattedTreeStructureAnsi).mkString("(", ", ", ")"))).mkString("\n")
                  hearthAssertionFailed(s"Expected 22 Term arguments, got:\n$preview")
                // $COVERAGE-ON$
              }
            )
        ),
        (self, (aExpr, bExpr, cExpr, dExpr, eExpr, fExpr, gExpr, hExpr, iExpr, jExpr, kExpr, lExpr, mExpr, nExpr, oExpr, pExpr, qExpr, rExpr, sExpr, tExpr, uExpr, vExpr))
      )
    }
    // format: on

    override def build[Signature, Returned](
        builder: ValDefBuilder[Signature, Returned, Expr[Returned]]
    ): ValDefs[Signature] =
      builder.mk.build(builder.value)

    override def buildCached[Signature, Returned](
        cache: ValDefsCache,
        key: String,
        builder: ValDefBuilder[Signature, Returned, Expr[Returned]]
    ): ValDefsCache =
      builder.mk.buildCached(cache, key, builder.value)

    override def forwardDeclare[Signature, Returned, Value](
        cache: ValDefsCache,
        key: String,
        builder: ValDefBuilder[Signature, Returned, Value]
    ): ValDefsCache =
      builder.mk.forwardDeclare(cache, key)

    override def partition[Signature, Returned, A, B, C](
        builder: ValDefBuilder[Signature, Returned, A]
    )(f: A => Either[B, C]): Either[ValDefBuilder[Signature, Returned, B], ValDefBuilder[Signature, Returned, C]] =
      f(builder.value) match {
        case Left(value)  => Left(new ValDefBuilder[Signature, Returned, B](builder.mk, value))
        case Right(value) => Right(new ValDefBuilder[Signature, Returned, C](builder.mk, value))
      }

    override def traverse[Signature, Returned]: fp.Traverse[ValDefBuilder[Signature, Returned, *]] =
      new fp.Traverse[ValDefBuilder[Signature, Returned, *]] {

        override def traverse[G[_]: fp.Applicative, A, B](fa: ValDefBuilder[Signature, Returned, A])(
            f: A => G[B]
        ): G[ValDefBuilder[Signature, Returned, B]] =
          f(fa.value).map(b => new ValDefBuilder[Signature, Returned, B](fa.mk, b))

        override def parTraverse[G[_]: fp.Parallel, A, B](fa: ValDefBuilder[Signature, Returned, A])(
            f: A => G[B]
        ): G[ValDefBuilder[Signature, Returned, B]] =
          f(fa.value).map(b => new ValDefBuilder[Signature, Returned, B](fa.mk, b))
      }
  }

  final class ValDefsCache private[typed] (val definitions: ListMap[ValDefsCache.Key, ValDefsCache.Value]) {

    private[typed] def forwardDeclare(key: ValDefsCache.Key, signature: Any): ValDefsCache =
      new ValDefsCache(definitions.updated(key, new ValDefsCache.Value(signature, None)))

    private[typed] def set(
        key: ValDefsCache.Key,
        signature: Any,
        definition: quotes.reflect.Statement
    ): ValDefsCache = {
      // $COVERAGE-OFF$
      if definitions.get(key).exists(_.signature != signature) then {
        hearthRequirementFailed(
          s"Def with key $key already exists with different signature, you probably created it twice in 2 branches, without noticing"
        )
      }
      // $COVERAGE-ON$
      new ValDefsCache(definitions.updated(key, new ValDefsCache.Value(signature, Some(definition))))
    }

    private[typed] def get[Signature](key: ValDefsCache.Key): Option[Signature] =
      definitions.get(key).map(_.signature.asInstanceOf[Signature])
  }

  object ValDefsCache extends ValDefsCacheModule {
    import quotes.*, quotes.reflect.*

    final private[typed] case class Key(name: String, args: Seq[UntypedType], returned: UntypedType) {

      override def hashCode(): Int = name.hashCode()

      override def equals(other: Any): Boolean = other match {
        case that: Key =>
          name == that.name && args.length == that.args.length && {
            val length = args.length
            var i = 0
            while i < length && args(i) =:= that.args(i) do i += 1
            i == length
          } && returned =:= that.returned
        case _ => false
      }

      override def toString: String =
        s"def $name(${args.view.map(_.prettyPrint).mkString(", ")}): ${returned.prettyPrint}"
    }

    final private[typed] case class Value(signature: Any, definition: Option[Statement])

    override def empty: ValDefsCache = new ValDefsCache(ListMap.empty)

    override def merge(cache1: ValDefsCache, cache2: ValDefsCache): ValDefsCache = {
      val keys = scala.collection.immutable.ListSet.from(cache1.definitions.keys ++ cache2.definitions.keys)
      val result = keys.view.map { key =>
        (cache1.definitions.get(key), cache2.definitions.get(key)) match {
          case (Some(value1), Some(value2)) =>
            // $COVERAGE-OFF$
            if value1.signature != value2.signature then {
              hearthRequirementFailed(
                s"Def with key $key already exists with different signature, you probably created it twice in 2 branches, without noticing"
              )
            }
            // $COVERAGE-ON$
            (value1.definition, value2.definition) match {
              // Forwarding after declaration should be a safe no-op
              case (Some(_), None) => (key, value1)
              // Declaration after forwarding should keep declaration
              case (None, Some(_)) => (key, value2)
              case _               =>
                // $COVERAGE-OFF$
                if value1.definition != value2.definition then {
                  hearthRequirementFailed(
                    s"Def with key $key already exists with different definition, you probably created it twice in 2 branches, without noticing"
                  )
                }
                // $COVERAGE-ON$
                (key, value1)
            }
          case (Some(value), None) => (key, value)
          case (None, Some(value)) => (key, value)
          case (None, None)        => ??? // impossible
        }
      }
      new ValDefsCache(ListMap.from(result))
    }

    // format: off
    override def get0Ary[Returned: Type](cache: ValDefsCache, key: String): Option[Expr[Returned]] =
      cache.get[Expr[Returned]](new Key(key, Seq.empty, Type[Returned].asUntyped))
    override def get1Ary[A: Type, Returned: Type](cache: ValDefsCache, key: String): Option[Expr[A] => Expr[Returned]] =
      cache.get[Expr[A] => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped), Type[Returned].asUntyped))
    override def get2Ary[A: Type, B: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped), Type[Returned].asUntyped))
    override def get3Ary[A: Type, B: Type, C: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped), Type[Returned].asUntyped))
    override def get4Ary[A: Type, B: Type, C: Type, D: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped), Type[Returned].asUntyped))
    override def get5Ary[A: Type, B: Type, C: Type, D: Type, E: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped), Type[Returned].asUntyped))
    override def get6Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped), Type[Returned].asUntyped))
    override def get7Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped), Type[Returned].asUntyped))
    override def get8Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped), Type[Returned].asUntyped))
    override def get9Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped), Type[Returned].asUntyped))
    override def get10Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped), Type[Returned].asUntyped))
    override def get11Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped), Type[Returned].asUntyped))
    override def get12Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped), Type[Returned].asUntyped))
    override def get13Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped), Type[Returned].asUntyped))
    override def get14Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped), Type[Returned].asUntyped))
    override def get15Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped), Type[Returned].asUntyped))
    override def get16Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped), Type[Returned].asUntyped))
    override def get17Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped), Type[Returned].asUntyped))
    override def get18Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped), Type[Returned].asUntyped))
    override def get19Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped), Type[Returned].asUntyped))
    override def get20Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped), Type[Returned].asUntyped))
    override def get21Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped, Type[U].asUntyped), Type[Returned].asUntyped))
    override def get22Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Returned: Type](cache: ValDefsCache, key: String): Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned]] =
      cache.get[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned]](new Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped, Type[U].asUntyped, Type[V].asUntyped), Type[Returned].asUntyped))
    // format: on

    override def toValDefs(cache: ValDefsCache): ValDefs[Unit] = {
      // filter out setters (we are forward declaring them, but they are built together with their setters)
      val (pending, definitions) = cache.definitions.filter(_._2.definition != Some(null)).partitionMap {
        case (_, ValDefsCache.Value(_, Some(definition))) => Right(definition)
        case (key, ValDefsCache.Value(_, None))           => Left(key)
      }
      if pending.nonEmpty then {
        hearthRequirementFailed(
          s"""Definitions were forward declared, but not built:
             |${pending.map(p => "  " + p.toString).mkString("\n")}
             |Make sure, that you built all the forwarded definitions.
             |Also, make sure, that you build forwrded definitions as a part of the ValDefsCache, not outside of it.definitions
             |Otherwise you would leak some definition outside if the scope it is available in which this check prevents.
             |""".stripMargin
        )
      } else {
        new ValDefs[Unit](definitions.toVector, ())
      }
    }
  }

  final class LambdaBuilder[From[_], To] private (private val mk: LambdaBuilder.Mk[From], private val value: To)

  object LambdaBuilder extends LambdaBuilderModule {
    import quotes.*, quotes.reflect.*

    import Expr.platformSpecific.*

    private trait Mk[From[_]] {

      def apply[To: Type](body: Expr[To]): Expr[From[To]]
    }

    // format: off
    @scala.annotation.nowarn
    override def of1[A: Type](
        freshA: FreshName
    ): LambdaBuilder[A => *, Expr[A]] = {
      val a1 = freshTerm.valdef[A](freshA, null, Flags.EmptyFlags)
      val a1Expr = Ref(a1).asExprOf[A]
      new LambdaBuilder[A => *, Expr[A]](
        new Mk[A => *] {
          // TODO: we need to create tests for each such utility,
          // reproducing the issue and then fixing it.
          override def apply[To: Type](body: Expr[To]): Expr[A => To] = withQuotes {

            // The problem is already with the Type[To] :/, maybe we should use cross quotes here
            println("executing the problematic code version=4")
            def mkBody(a: Expr[A]) = Block(
              List(
                ValDef(a1, Some(a.asTerm)),
                '{ val _ = $a1Expr }.asTerm
              ),
              body.resetOwner.asTerm
            ).asExprOf[To]

            '{ (a: A) => ${ mkBody('a) } }
          }
        },
        a1Expr
      )
    }
    override def of2[A: Type, B: Type](
        freshA: FreshName,
        freshB: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName,
        freshI: FreshName,
        freshJ: FreshName,
        freshK: FreshName,
        freshL: FreshName,
        freshM: FreshName,
        freshN: FreshName,
        freshO: FreshName,
        freshP: FreshName,
        freshQ: FreshName
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
        freshA: FreshName, freshB: FreshName, freshC: FreshName, freshD: FreshName, freshE: FreshName, freshF: FreshName, freshG: FreshName, freshH: FreshName, freshI: FreshName, freshJ: FreshName, freshK: FreshName, freshL: FreshName, freshM: FreshName, freshN: FreshName, freshO: FreshName, freshP: FreshName, freshQ: FreshName, freshR: FreshName
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
        freshA: FreshName, freshB: FreshName, freshC: FreshName, freshD: FreshName, freshE: FreshName, freshF: FreshName, freshG: FreshName, freshH: FreshName, freshI: FreshName, freshJ: FreshName, freshK: FreshName, freshL: FreshName, freshM: FreshName, freshN: FreshName, freshO: FreshName, freshP: FreshName, freshQ: FreshName, freshR: FreshName, freshS: FreshName
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
        freshA: FreshName, freshB: FreshName, freshC: FreshName, freshD: FreshName, freshE: FreshName, freshF: FreshName, freshG: FreshName, freshH: FreshName, freshI: FreshName, freshJ: FreshName, freshK: FreshName, freshL: FreshName, freshM: FreshName, freshN: FreshName, freshO: FreshName, freshP: FreshName, freshQ: FreshName, freshR: FreshName, freshS: FreshName, freshT: FreshName
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
        freshA: FreshName, freshB: FreshName, freshC: FreshName, freshD: FreshName, freshE: FreshName, freshF: FreshName, freshG: FreshName, freshH: FreshName, freshI: FreshName, freshJ: FreshName, freshK: FreshName, freshL: FreshName, freshM: FreshName, freshN: FreshName, freshO: FreshName, freshP: FreshName, freshQ: FreshName, freshR: FreshName, freshS: FreshName, freshT: FreshName, freshU: FreshName
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
        freshA: FreshName, freshB: FreshName, freshC: FreshName, freshD: FreshName, freshE: FreshName, freshF: FreshName, freshG: FreshName, freshH: FreshName, freshI: FreshName, freshJ: FreshName, freshK: FreshName, freshL: FreshName, freshM: FreshName, freshN: FreshName, freshO: FreshName, freshP: FreshName, freshQ: FreshName, freshR: FreshName, freshS: FreshName, freshT: FreshName, freshU: FreshName, freshV: FreshName
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

    override def partition[From[_], A, B, C](builder: LambdaBuilder[From, A])(
        f: A => Either[B, C]
    ): Either[LambdaBuilder[From, B], LambdaBuilder[From, C]] =
      f(builder.value) match {
        case Left(value)  => Left(new LambdaBuilder[From, B](builder.mk, value))
        case Right(value) => Right(new LambdaBuilder[From, C](builder.mk, value))
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
