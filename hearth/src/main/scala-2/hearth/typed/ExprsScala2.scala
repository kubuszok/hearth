package hearth
package typed

import hearth.fp.data.NonEmptyVector
import hearth.fp.syntax.*
import hearth.treeprinter.SyntaxHighlight

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
          case Some(codec) =>
            val aType = codec.toType(value).as_??
            import aType.Underlying as B
            c.Expr[B](to.apply(value)).asInstanceOf[Expr[A]]
          case _ => c.Expr[A](to.apply(value))
        }

        override def fromExpr(expr: Expr[A]): Option[A] = from.unapply(expr.tree)
      }

      implicit final class ExprCodecCompanionOps(private val self: ExprCodec.type) {

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

    override def plainPrint[A](expr: Expr[A]): String = removeMacroSuffix(
      showCodePretty(expr.tree, SyntaxHighlight.plain)
    )
    override def prettyPrint[A](expr: Expr[A]): String = removeMacroSuffix(
      showCodePretty(expr.tree, SyntaxHighlight.ANSI)
    )

    override def plainAST[A](expr: Expr[A]): String = showRawPretty(expr.tree, SyntaxHighlight.plain)
    override def prettyAST[A](expr: Expr[A]): String = showRawPretty(expr.tree, SyntaxHighlight.ANSI)

    override def summonImplicit[A: Type]: SummoningResult[A] = parseImplicitSearchResult {
      c.inferImplicitValue(Type[A].tpe, silent = true, withMacrosDisabled = false)
    }
    override def summonImplicitIgnoring[A: Type](excluded: UntypedMethod*): SummoningResult[A] =
      inferImplicitValueIgnoringOption.fold[SummoningResult[A]] {
        // $COVERAGE-OFF$
        hearthRequirementFailed(
          """Expr.summonImplicitIgnoring on Scala 2 relies on c.inferImplicitValueIgnoring method, which is available since Scala 2.13.17.
            |Use Environment.currentScalaVersion to check if this method is available, or raise the minimum required Scala version for the library.""".stripMargin
        )
        // $COVERAGE-ON$
      } { inferImplicitValueIgnoring =>
        parseImplicitSearchResult {
          inferImplicitValueIgnoring
            .invoke(
              c,
              /* tp = */ Type[A].tpe,
              /* silent = */ true,
              /* withMacrosDisabled = */ false,
              /* position = */ c.enclosingPosition,
              /* excluded = */ excluded.map(_.symbol)
            )
            .asInstanceOf[c.Tree]
        }
      }
    private def parseImplicitSearchResult[A: Type](thunk: => c.Tree): SummoningResult[A] = try {
      val result = thunk
      if (result != EmptyTree) SummoningResult.Found(c.Expr[A](result))
      else SummoningResult.NotFound(Type[A])
    } catch {
      case exception: scala.reflect.macros.TypecheckException =>
        // TODO: consider parsing the message to get the list of ambiguous implicit values
        if (exception.getMessage.startsWith("ambiguous implicit values:")) SummoningResult.Ambiguous(Type[A])
        else if (exception.getMessage.startsWith("diverging implicit expansion for type"))
          SummoningResult.Diverging(Type[A])
        else SummoningResult.NotFound(Type[A])
    }
    // $COVERAGE-OFF$
    private lazy val inferImplicitValueIgnoringOption =
      c.getClass.getDeclaredMethods.find(_.getName == "inferImplicitValueIgnoring")
    // $COVERAGE-ON$

    override def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      if (Type[A] =:= Type[B]) expr.asInstanceOf[Expr[B]] // types are identical in practice, we can just cast
      else c.Expr[B](q"($expr : ${Type[B]})") // check A <:< B AND add a syntax to force upcasting
    }

    override def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit] = c.Expr[Unit](q"val _ = $expr; ()")

    override lazy val NullExprCodec: ExprCodec[Null] = {
      implicit val liftable: Liftable[Null] = Liftable[Null](_ => q"null")
      implicit val unliftable: Unliftable[Null] = Unliftable[Null] { case _ => null }
      ExprCodec.withTypeCodec[Null]
    }
    override lazy val UnitExprCodec: ExprCodec[Unit] = ExprCodec.withTypeCodec[Unit]
    override lazy val BooleanExprCodec: ExprCodec[Boolean] = ExprCodec.withTypeCodec[Boolean]
    override lazy val ByteExprCodec: ExprCodec[Byte] = ExprCodec.withTypeCodec[Byte]
    override lazy val ShortExprCodec: ExprCodec[Short] = ExprCodec.withTypeCodec[Short]
    override lazy val IntExprCodec: ExprCodec[Int] = ExprCodec.withTypeCodec[Int]
    override lazy val LongExprCodec: ExprCodec[Long] = ExprCodec.withTypeCodec[Long]
    override lazy val FloatExprCodec: ExprCodec[Float] = ExprCodec.withTypeCodec[Float]
    override lazy val DoubleExprCodec: ExprCodec[Double] = ExprCodec.withTypeCodec[Double]
    override lazy val CharExprCodec: ExprCodec[Char] = ExprCodec.withTypeCodec[Char]
    override lazy val StringExprCodec: ExprCodec[String] = ExprCodec.withTypeCodec[String]

    // For now, assume that all ExprCodecs below are of PoC quality. It was needed to unblock some other work.
    // But each should have unit tests which would make sure that Scala 2 and Scala 3 are in sync (which would
    // require adding missing implementations to both sides, and then expanding the built-in Liftable and Unliftable
    // with more cases).

    override def ClassExprCodec[A: Type]: ExprCodec[java.lang.Class[A]] = {
      implicit val liftable: Liftable[java.lang.Class[A]] = Liftable[java.lang.Class[A]] { _ =>
        q"scala.Predef.classOf[${Type[A]}]"
      }
      implicit val unliftable: Unliftable[java.lang.Class[A]] = new Unliftable[java.lang.Class[A]] {
        def unapply(tree: Tree): Option[java.lang.Class[A]] = Type[A].getRuntimeClass
      }
      ExprCodec.make[java.lang.Class[A]]
    }
    override def ClassTagExprCodec[A: Type]: ExprCodec[scala.reflect.ClassTag[A]] = {
      implicit val liftable: Liftable[scala.reflect.ClassTag[A]] = Liftable[scala.reflect.ClassTag[A]] { _ =>
        q"scala.reflect.classTag[${Type[A]}]"
      }
      implicit val unliftable: Unliftable[scala.reflect.ClassTag[A]] = new Unliftable[scala.reflect.ClassTag[A]] {
        def unapply(tree: Tree): Option[scala.reflect.ClassTag[A]] =
          Type[A].getRuntimeClass.map(scala.reflect.ClassTag(_))
      }
      ExprCodec.make[scala.reflect.ClassTag[A]]
    }

    // In the code below we cannot just `import platformSpecific.implicits.given`, because Expr.make[Coll[A]] would use
    // implicit ExprCodec[Coll[A]] from the companion object, which would create a circular dependency. Instead, we
    // want to extract the implicit ToExpr[A] and FromExpr[A] from the ExprCodec[A], and then use it in the code below.

    override def ArrayExprCodec[A: ExprCodec: Type]: ExprCodec[Array[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftable: Unliftable[Array[A]] = Unliftable[Array[A]](PartialFunction.empty) // TODO
      ExprCodec.make[Array[A]]
    }
    override def SeqExprCodec[A: ExprCodec: Type]: ExprCodec[Seq[A]] = {
      implicit val liftable: Liftable[Seq[A]] = Liftable[Seq[A]] { seq =>
        q"scala.collection.immutable.Seq(..${seq.map(ExprCodec[A].toExpr)})"
      }
      implicit val unliftable: Unliftable[Seq[A]] = Unliftable[Seq[A]](PartialFunction.empty) // TODO
      ExprCodec.make[Seq[A]]
    }
    override def ListExprCodec[A: ExprCodec: Type]: ExprCodec[List[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftable: Unliftable[List[A]] = Unliftable[List[A]](PartialFunction.empty) // TODO
      ExprCodec.make[List[A]]
    }
    override lazy val NilExprCodec: ExprCodec[Nil.type] = {
      implicit val unliftable: Unliftable[Nil.type] = Unliftable[Nil.type](PartialFunction.empty) // TODO
      ExprCodec.make[Nil.type]
    }
    override def VectorExprCodec[A: ExprCodec: Type]: ExprCodec[Vector[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftable: Unliftable[Vector[A]] = Unliftable[Vector[A]](PartialFunction.empty) // TODO
      ExprCodec.make[Vector[A]]
    }
    override def MapExprCodec[K: ExprCodec: Type, V: ExprCodec: Type]: ExprCodec[Map[K, V]] = {
      implicit val liftableK: Liftable[K] = platformSpecific.implicits.ExprCodecLiftable[K]
      implicit val liftableV: Liftable[V] = platformSpecific.implicits.ExprCodecLiftable[V]
      implicit val unliftable: Unliftable[Map[K, V]] = Unliftable[Map[K, V]](PartialFunction.empty) // TODO
      ExprCodec.make[Map[K, V]]
    }
    override def SetExprCodec[A: ExprCodec: Type]: ExprCodec[Set[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftable: Unliftable[Set[A]] = Unliftable[Set[A]](PartialFunction.empty) // TODO
      ExprCodec.make[Set[A]]
    }
    override def OptionExprCodec[A: ExprCodec: Type]: ExprCodec[Option[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftable: Unliftable[Option[A]] = Unliftable[Option[A]](PartialFunction.empty) // TODO
      ExprCodec.make[Option[A]]
    }
    override def SomeExprCodec[A: ExprCodec: Type]: ExprCodec[Some[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftable: Unliftable[Some[A]] = Unliftable[Some[A]](PartialFunction.empty) // TODO
      ExprCodec.make[Some[A]]
    }
    override lazy val NoneExprCodec: ExprCodec[None.type] = {
      implicit val unliftable: Unliftable[None.type] = Unliftable[None.type](PartialFunction.empty) // TODO
      ExprCodec.make[None.type]
    }
    override def EitherExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Either[L, R]] = {
      implicit val liftableL: Liftable[L] = platformSpecific.implicits.ExprCodecLiftable[L]
      implicit val liftableR: Liftable[R] = platformSpecific.implicits.ExprCodecLiftable[R]
      implicit val unliftable: Unliftable[Either[L, R]] = Unliftable[Either[L, R]](PartialFunction.empty) // TODO
      ExprCodec.make[Either[L, R]]
    }
    override def LeftExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Left[L, R]] = {
      implicit val liftableL: Liftable[L] = platformSpecific.implicits.ExprCodecLiftable[L]
      implicit val unliftable: Unliftable[Left[L, R]] = Unliftable[Left[L, R]](PartialFunction.empty) // TODO
      ExprCodec.make[Left[L, R]]
    }
    override def RightExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Right[L, R]] = {
      implicit val liftableR: Liftable[R] = platformSpecific.implicits.ExprCodecLiftable[R]
      implicit val unliftable: Unliftable[Right[L, R]] = Unliftable[Right[L, R]](PartialFunction.empty) // TODO
      ExprCodec.make[Right[L, R]]
    }
  }

  final override type VarArgs[A] = Seq[Expr[A]]

  object VarArgs extends VarArgsModule {
    override def toIterable[A](args: VarArgs[A]): Iterable[Expr[A]] = args
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

    // format: off
    override def of1[A: Type](
        freshA: FreshName = FreshName.FromType
    ): LambdaBuilder[A => *, Expr[A]] = {
      val a1 = freshTerm[A](freshA, null)
      new LambdaBuilder[A => *, Expr[A]](
        new Mk[A => *] {
          override def apply[To: Type](body: Expr[To]): Expr[A => To] =
            c.Expr[A => To](q"($a1: ${Type[A]}) => $body")
        },
        c.Expr[A](q"$a1")
      )
    }
    override def of2[A: Type, B: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      new LambdaBuilder[(A, B) => *, (Expr[A], Expr[B])](
        new Mk[(A, B) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B) => To] =
            c.Expr[(A, B) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"))
      )
    }
    override def of3[A: Type, B: Type, C: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C) => *, (Expr[A], Expr[B], Expr[C])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      new LambdaBuilder[(A, B, C) => *, (Expr[A], Expr[B], Expr[C])](
        new Mk[(A, B, C) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C) => To] =
            c.Expr[(A, B, C) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"))
      )
    }
    override def of4[A: Type, B: Type, C: Type, D: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D) => *, (Expr[A], Expr[B], Expr[C], Expr[D])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      new LambdaBuilder[(A, B, C, D) => *, (Expr[A], Expr[B], Expr[C], Expr[D])](
        new Mk[(A, B, C, D) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D) => To] =
            c.Expr[(A, B, C, D) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"))
      )
    }
    override def of5[A: Type, B: Type, C: Type, D: Type, E: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      new LambdaBuilder[(A, B, C, D, E) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E])](
        new Mk[(A, B, C, D, E) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E) => To] =
            c.Expr[(A, B, C, D, E) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"))
      )
    }
    override def of6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      new LambdaBuilder[(A, B, C, D, E, F) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F])](
        new Mk[(A, B, C, D, E, F) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F) => To] =
            c.Expr[(A, B, C, D, E, F) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"))
      )
    }
    override def of7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      new LambdaBuilder[(A, B, C, D, E, F, G) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G])](
        new Mk[(A, B, C, D, E, F, G) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G) => To] =
            c.Expr[(A, B, C, D, E, F, G) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"))
      )
    }
    override def of8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type](
        freshA: FreshName = FreshName.FromType,
        freshB: FreshName = FreshName.FromType,
        freshC: FreshName = FreshName.FromType,
        freshD: FreshName = FreshName.FromType,
        freshE: FreshName = FreshName.FromType,
        freshF: FreshName = FreshName.FromType,
        freshG: FreshName = FreshName.FromType,
        freshH: FreshName = FreshName.FromType
    ): LambdaBuilder[(A, B, C, D, E, F, G, H) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H])](
        new Mk[(A, B, C, D, E, F, G, H) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H) => To] =
            c.Expr[(A, B, C, D, E, F, G, H) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"))
      )
    }
    override def of9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I])](
        new Mk[(A, B, C, D, E, F, G, H, I) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"))
      )
    }
    override def of10[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J])](
        new Mk[(A, B, C, D, E, F, G, H, I, J) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"))
      )
    }
    override def of11[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"))
      )
    }
    override def of12[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"))
      )
    }
    override def of13[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"))
      )
    }
    override def of14[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"))
      )
    }
    override def of15[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"))
      )
    }
    override def of16[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      val p1 = freshTerm[P](freshP, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"), c.Expr[P](q"$p1"))
      )
    }
    override def of17[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type](
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
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      val p1 = freshTerm[P](freshP, null)
      val q1 = freshTerm[Q](freshQ, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"), c.Expr[P](q"$p1"), c.Expr[Q](q"$q1"))
      )
    }
    override def of18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      val p1 = freshTerm[P](freshP, null)
      val q1 = freshTerm[Q](freshQ, null)
      val r1 = freshTerm[R](freshR, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"), c.Expr[P](q"$p1"), c.Expr[Q](q"$q1"), c.Expr[R](q"$r1"))
      )
    }
    override def of19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      val p1 = freshTerm[P](freshP, null)
      val q1 = freshTerm[Q](freshQ, null)
      val r1 = freshTerm[R](freshR, null)
      val s1 = freshTerm[S](freshS, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"), c.Expr[P](q"$p1"), c.Expr[Q](q"$q1"), c.Expr[R](q"$r1"), c.Expr[S](q"$s1"))
      )
    }
    override def of20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      val p1 = freshTerm[P](freshP, null)
      val q1 = freshTerm[Q](freshQ, null)
      val r1 = freshTerm[R](freshR, null)
      val s1 = freshTerm[S](freshS, null)
      val t1 = freshTerm[T](freshT, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}, $t1: ${Type[T]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"), c.Expr[P](q"$p1"), c.Expr[Q](q"$q1"), c.Expr[R](q"$r1"), c.Expr[S](q"$s1"), c.Expr[T](q"$t1"))
      )
    }
    override def of21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      val p1 = freshTerm[P](freshP, null)
      val q1 = freshTerm[Q](freshQ, null)
      val r1 = freshTerm[R](freshR, null)
      val s1 = freshTerm[S](freshS, null)
      val t1 = freshTerm[T](freshT, null)
      val u1 = freshTerm[U](freshU, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}, $t1: ${Type[T]}, $u1: ${Type[U]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"), c.Expr[P](q"$p1"), c.Expr[Q](q"$q1"), c.Expr[R](q"$r1"), c.Expr[S](q"$s1"), c.Expr[T](q"$t1"), c.Expr[U](q"$u1"))
      )
    }
    override def of22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type](
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
    ): LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V])] = {
      val a1 = freshTerm[A](freshA, null)
      val b1 = freshTerm[B](freshB, null)
      val c1 = freshTerm[C](freshC, null)
      val d1 = freshTerm[D](freshD, null)
      val e1 = freshTerm[E](freshE, null)
      val f1 = freshTerm[F](freshF, null)
      val g1 = freshTerm[G](freshG, null)
      val h1 = freshTerm[H](freshH, null)
      val i1 = freshTerm[I](freshI, null)
      val j1 = freshTerm[J](freshJ, null)
      val k1 = freshTerm[K](freshK, null)
      val l1 = freshTerm[L](freshL, null)
      val m1 = freshTerm[M](freshM, null)
      val n1 = freshTerm[N](freshN, null)
      val o1 = freshTerm[O](freshO, null)
      val p1 = freshTerm[P](freshP, null)
      val q1 = freshTerm[Q](freshQ, null)
      val r1 = freshTerm[R](freshR, null)
      val s1 = freshTerm[S](freshS, null)
      val t1 = freshTerm[T](freshT, null)
      val u1 = freshTerm[U](freshU, null)
      val v1 = freshTerm[V](freshV, null)
      new LambdaBuilder[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *, (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V])](
        new Mk[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => *] {
          override def apply[To: Type](body: Expr[To]): Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => To] =
            c.Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => To](q"($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}, $t1: ${Type[T]}, $u1: ${Type[U]}, $v1: ${Type[V]}) => $body")
        },
        (c.Expr[A](q"$a1"), c.Expr[B](q"$b1"), c.Expr[C](q"$c1"), c.Expr[D](q"$d1"), c.Expr[E](q"$e1"), c.Expr[F](q"$f1"), c.Expr[G](q"$g1"), c.Expr[H](q"$h1"), c.Expr[I](q"$i1"), c.Expr[J](q"$j1"), c.Expr[K](q"$k1"), c.Expr[L](q"$l1"), c.Expr[M](q"$m1"), c.Expr[N](q"$n1"), c.Expr[O](q"$o1"), c.Expr[P](q"$p1"), c.Expr[Q](q"$q1"), c.Expr[R](q"$r1"), c.Expr[S](q"$s1"), c.Expr[T](q"$t1"), c.Expr[U](q"$u1"), c.Expr[V](q"$v1"))
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
