package hearth
package typed

import hearth.fp.data.NonEmptyVector
import hearth.fp.syntax.*
import hearth.treeprinter.SyntaxHighlight

import scala.collection.immutable.ListMap

trait ExprsScala2 extends Exprs { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type Expr[A] = c.Expr[A]

  object Expr extends ExprModule {

    object platformSpecific {

      // when there is some c variable c.Expr[A] becomes not available
      def asExpr[A](tree: Tree): Expr[A] = c.Expr[A](tree)

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
      c.getClass.getMethods.find(_.getName == "inferImplicitValueIgnoring")
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

    override lazy val BigIntExprCodec: ExprCodec[BigInt] = {
      implicit val liftable: Liftable[BigInt] = Liftable[BigInt] { value =>
        q"scala.math.BigInt(${value.toString})"
      }
      implicit val unliftable: Unliftable[BigInt] = new Unliftable[BigInt] {
        private def fromArg(arg: Tree): Option[BigInt] = arg match {
          case Literal(Constant(s: String)) => Some(BigInt(s))
          case Literal(Constant(i: Int))    => Some(BigInt(i))
          case Literal(Constant(l: Long))   => Some(BigInt(l))
          case _                            => None
        }
        def unapply(tree: Tree): Option[BigInt] = tree match {
          case q"scala.math.BigInt.apply($arg)"      => fromArg(arg)
          case q"scala.math.BigInt($arg)"            => fromArg(arg)
          case q"scala.`package`.BigInt.apply($arg)" => fromArg(arg)
          case q"scala.`package`.BigInt($arg)"       => fromArg(arg)
          case q"scala.BigInt.apply($arg)"           => fromArg(arg)
          case q"scala.BigInt($arg)"                 => fromArg(arg)
          case q"BigInt.apply($arg)"                 => fromArg(arg)
          case q"BigInt($arg)"                       => fromArg(arg)
          case _                                     => None
        }
      }
      ExprCodec.make[BigInt]
    }

    override lazy val BigDecimalExprCodec: ExprCodec[BigDecimal] = {
      implicit val liftable: Liftable[BigDecimal] = Liftable[BigDecimal] { value =>
        q"scala.math.BigDecimal(${value.toString})"
      }
      implicit val unliftable: Unliftable[BigDecimal] = new Unliftable[BigDecimal] {
        private def fromArg(arg: Tree): Option[BigDecimal] = arg match {
          case Literal(Constant(s: String)) => Some(BigDecimal(s))
          case Literal(Constant(i: Int))    => Some(BigDecimal(i))
          case Literal(Constant(l: Long))   => Some(BigDecimal(l))
          case Literal(Constant(d: Double)) => Some(BigDecimal(d))
          case _                            => None
        }
        def unapply(tree: Tree): Option[BigDecimal] = tree match {
          case q"scala.math.BigDecimal.apply($arg)"      => fromArg(arg)
          case q"scala.math.BigDecimal($arg)"            => fromArg(arg)
          case q"scala.`package`.BigDecimal.apply($arg)" => fromArg(arg)
          case q"scala.`package`.BigDecimal($arg)"       => fromArg(arg)
          case q"scala.BigDecimal.apply($arg)"           => fromArg(arg)
          case q"scala.BigDecimal($arg)"                 => fromArg(arg)
          case q"BigDecimal.apply($arg)"                 => fromArg(arg)
          case q"BigDecimal($arg)"                       => fromArg(arg)
          case _                                         => None
        }
      }
      ExprCodec.make[BigDecimal]
    }

    override lazy val StringContextExprCodec: ExprCodec[StringContext] = {
      implicit val liftable: Liftable[StringContext] = Liftable[StringContext] { value =>
        q"scala.StringContext(..${value.parts.toList.map(s => q"$s")})"
      }
      implicit val unliftable: Unliftable[StringContext] = new Unliftable[StringContext] {
        def unapply(tree: Tree): Option[StringContext] = tree match {
          case q"scala.StringContext.apply(..$parts)" =>
            val strings = parts.collect { case Literal(Constant(s: String)) => s }
            if (strings.size == parts.size) Some(StringContext(strings*)) else None
          case q"scala.StringContext(..$parts)" =>
            val strings = parts.collect { case Literal(Constant(s: String)) => s }
            if (strings.size == parts.size) Some(StringContext(strings*)) else None
          case q"new scala.StringContext(..$parts)" =>
            val strings = parts.collect { case Literal(Constant(s: String)) => s }
            if (strings.size == parts.size) Some(StringContext(strings*)) else None
          case q"StringContext.apply(..$parts)" =>
            val strings = parts.collect { case Literal(Constant(s: String)) => s }
            if (strings.size == parts.size) Some(StringContext(strings*)) else None
          case q"StringContext(..$parts)" =>
            val strings = parts.collect { case Literal(Constant(s: String)) => s }
            if (strings.size == parts.size) Some(StringContext(strings*)) else None
          case q"new StringContext(..$parts)" =>
            val strings = parts.collect { case Literal(Constant(s: String)) => s }
            if (strings.size == parts.size) Some(StringContext(strings*)) else None
          case _ => None
        }
      }
      ExprCodec.make[StringContext]
    }

    // In the code below we cannot just `import platformSpecific.implicits.given`, because Expr.make[Coll[A]] would use
    // implicit ExprCodec[Coll[A]] from the companion object, which would create a circular dependency. Instead, we
    // want to extract the implicit ToExpr[A] and FromExpr[A] from the ExprCodec[A], and then use it in the code below.

    override def ArrayExprCodec[A: ExprCodec: Type]: ExprCodec[Array[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftableA: Unliftable[A] = platformSpecific.implicits.ExprCodecUnliftable[A]
      implicit val unliftable: Unliftable[Array[A]] = new Unliftable[Array[A]] {
        private def extractElems(elems: List[Tree]): Option[Array[A]] = {
          val decoded = elems.collect { case unliftableA(a) => a }
          if (decoded.size == elems.size) {
            val ct = Type[A].getRuntimeClass
              .map(c => scala.reflect.ClassTag[A](c.asInstanceOf[java.lang.Class[A]]))
              .getOrElse(scala.reflect.ClassTag[A](classOf[AnyRef].asInstanceOf[java.lang.Class[A]]))
            Some(decoded.toArray(ct))
          } else None
        }
        def unapply(tree: Tree): Option[Array[A]] = tree match {
          case q"scala.Array.apply(..$elems)" => extractElems(elems)
          case q"scala.Array(..$elems)"       => extractElems(elems)
          case q"Array.apply(..$elems)"       => extractElems(elems)
          case q"Array(..$elems)"             => extractElems(elems)
          case _                              => None
        }
      }
      ExprCodec.make[Array[A]]
    }
    override def SeqExprCodec[A: ExprCodec: Type]: ExprCodec[Seq[A]] = {
      implicit val liftable: Liftable[Seq[A]] = Liftable[Seq[A]] { seq =>
        q"scala.collection.immutable.Seq(..${seq.map(ExprCodec[A].toExpr)})"
      }
      implicit val unliftableA: Unliftable[A] = platformSpecific.implicits.ExprCodecUnliftable[A]
      implicit val unliftable: Unliftable[Seq[A]] = new Unliftable[Seq[A]] {
        private def extractElems(elems: List[Tree]): Option[Seq[A]] = {
          val decoded = elems.collect { case unliftableA(a) => a }
          if (decoded.size == elems.size) Some(decoded) else None
        }
        def unapply(tree: Tree): Option[Seq[A]] = tree match {
          case q"scala.collection.immutable.Seq.apply(..$elems)" => extractElems(elems)
          case q"scala.collection.immutable.Seq(..$elems)"       => extractElems(elems)
          case q"Seq.apply(..$elems)"                            => extractElems(elems)
          case q"Seq(..$elems)"                                  => extractElems(elems)
          // subtype specialization: match List patterns
          case q"scala.collection.immutable.List.apply(..$elems)" => extractElems(elems)
          case q"scala.collection.immutable.List(..$elems)"       => extractElems(elems)
          case q"List.apply(..$elems)"                            => extractElems(elems)
          case q"List(..$elems)"                                  => extractElems(elems)
          case q"scala.collection.immutable.Nil"                  => Some(Nil)
          case q"Nil"                                             => Some(Nil)
          // subtype specialization: match Vector patterns
          case q"scala.collection.immutable.Vector.apply(..$elems)" => extractElems(elems)
          case q"scala.collection.immutable.Vector(..$elems)"       => extractElems(elems)
          case q"Vector.apply(..$elems)"                            => extractElems(elems)
          case q"Vector(..$elems)"                                  => extractElems(elems)
          case _                                                    => None
        }
      }
      ExprCodec.make[Seq[A]]
    }
    override def ListExprCodec[A: ExprCodec: Type]: ExprCodec[List[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftableA: Unliftable[A] = platformSpecific.implicits.ExprCodecUnliftable[A]
      implicit val unliftable: Unliftable[List[A]] = new Unliftable[List[A]] {
        private def extractElems(elems: List[Tree]): Option[List[A]] = {
          val decoded = elems.collect { case unliftableA(a) => a }
          if (decoded.size == elems.size) Some(decoded) else None
        }
        def unapply(tree: Tree): Option[List[A]] = tree match {
          case q"scala.collection.immutable.List.apply(..$elems)" => extractElems(elems)
          case q"scala.collection.immutable.List(..$elems)"       => extractElems(elems)
          case q"List.apply(..$elems)"                            => extractElems(elems)
          case q"List(..$elems)"                                  => extractElems(elems)
          case q"scala.collection.immutable.Nil"                  => Some(Nil)
          case q"Nil"                                             => Some(Nil)
          case _                                                  => None
        }
      }
      ExprCodec.make[List[A]]
    }
    override lazy val NilExprCodec: ExprCodec[Nil.type] = {
      implicit val unliftable: Unliftable[Nil.type] = new Unliftable[Nil.type] {
        def unapply(tree: Tree): Option[Nil.type] = tree match {
          case q"scala.collection.immutable.Nil"          => Some(Nil)
          case q"Nil"                                     => Some(Nil)
          case q"scala.collection.immutable.List()"       => Some(Nil)
          case q"scala.collection.immutable.List.apply()" => Some(Nil)
          case q"List()"                                  => Some(Nil)
          case q"List.apply()"                            => Some(Nil)
          case _                                          => None
        }
      }
      ExprCodec.make[Nil.type]
    }
    override def VectorExprCodec[A: ExprCodec: Type]: ExprCodec[Vector[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftableA: Unliftable[A] = platformSpecific.implicits.ExprCodecUnliftable[A]
      implicit val unliftable: Unliftable[Vector[A]] = new Unliftable[Vector[A]] {
        private def extractElems(elems: List[Tree]): Option[Vector[A]] = {
          val decoded = elems.collect { case unliftableA(a) => a }
          if (decoded.size == elems.size) Some(decoded.toVector) else None
        }
        def unapply(tree: Tree): Option[Vector[A]] = tree match {
          case q"scala.collection.immutable.Vector.apply(..$elems)" => extractElems(elems)
          case q"scala.collection.immutable.Vector(..$elems)"       => extractElems(elems)
          case q"Vector.apply(..$elems)"                            => extractElems(elems)
          case q"Vector(..$elems)"                                  => extractElems(elems)
          case _                                                    => None
        }
      }
      ExprCodec.make[Vector[A]]
    }
    override def MapExprCodec[K: ExprCodec: Type, V: ExprCodec: Type]: ExprCodec[Map[K, V]] = {
      implicit val liftableK: Liftable[K] = platformSpecific.implicits.ExprCodecLiftable[K]
      implicit val liftableV: Liftable[V] = platformSpecific.implicits.ExprCodecLiftable[V]
      implicit val unliftableK: Unliftable[K] = platformSpecific.implicits.ExprCodecUnliftable[K]
      implicit val unliftableV: Unliftable[V] = platformSpecific.implicits.ExprCodecUnliftable[V]
      implicit val unliftable: Unliftable[Map[K, V]] = new Unliftable[Map[K, V]] {
        private def extractPair(tree: Tree): Option[(K, V)] = tree match {
          case q"scala.Tuple2.apply($k, $v)" =>
            for (kv <- unliftableK.unapply(k); vv <- unliftableV.unapply(v)) yield (kv, vv)
          case q"scala.Tuple2($k, $v)" =>
            for (kv <- unliftableK.unapply(k); vv <- unliftableV.unapply(v)) yield (kv, vv)
          case q"scala.Predef.ArrowAssoc($k).->($v)" =>
            for (kv <- unliftableK.unapply(k); vv <- unliftableV.unapply(v)) yield (kv, vv)
          case q"($k, $v)" => for (kv <- unliftableK.unapply(k); vv <- unliftableV.unapply(v)) yield (kv, vv)
          case _           => None
        }
        private def extractPairs(elems: List[Tree]): Option[Map[K, V]] = {
          val decoded = elems.flatMap(extractPair)
          if (decoded.size == elems.size) Some(Map.from(decoded)) else None
        }
        def unapply(tree: Tree): Option[Map[K, V]] = tree match {
          case q"scala.collection.immutable.Map.apply(..$elems)" => extractPairs(elems)
          case q"scala.collection.immutable.Map(..$elems)"       => extractPairs(elems)
          case q"scala.Predef.Map.apply(..$elems)"               => extractPairs(elems)
          case q"scala.Predef.Map(..$elems)"                     => extractPairs(elems)
          case q"Map.apply(..$elems)"                            => extractPairs(elems)
          case q"Map(..$elems)"                                  => extractPairs(elems)
          case _                                                 => None
        }
      }
      ExprCodec.make[Map[K, V]]
    }
    override def SetExprCodec[A: ExprCodec: Type]: ExprCodec[Set[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftableA: Unliftable[A] = platformSpecific.implicits.ExprCodecUnliftable[A]
      implicit val unliftable: Unliftable[Set[A]] = new Unliftable[Set[A]] {
        private def extractElems(elems: List[Tree]): Option[Set[A]] = {
          val decoded = elems.collect { case unliftableA(a) => a }
          if (decoded.size == elems.size) Some(decoded.toSet) else None
        }
        def unapply(tree: Tree): Option[Set[A]] = tree match {
          case q"scala.collection.immutable.Set.apply(..$elems)" => extractElems(elems)
          case q"scala.collection.immutable.Set(..$elems)"       => extractElems(elems)
          case q"scala.Predef.Set.apply(..$elems)"               => extractElems(elems)
          case q"scala.Predef.Set(..$elems)"                     => extractElems(elems)
          case q"Set.apply(..$elems)"                            => extractElems(elems)
          case q"Set(..$elems)"                                  => extractElems(elems)
          case _                                                 => None
        }
      }
      ExprCodec.make[Set[A]]
    }
    override def OptionExprCodec[A: ExprCodec: Type]: ExprCodec[Option[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftableA: Unliftable[A] = platformSpecific.implicits.ExprCodecUnliftable[A]
      implicit val unliftable: Unliftable[Option[A]] = new Unliftable[Option[A]] {
        def unapply(tree: Tree): Option[Option[A]] = tree match {
          case q"scala.Some.apply($elem)" => unliftableA.unapply(elem).map(Some(_))
          case q"scala.Some($elem)"       => unliftableA.unapply(elem).map(Some(_))
          case q"Some.apply($elem)"       => unliftableA.unapply(elem).map(Some(_))
          case q"Some($elem)"             => unliftableA.unapply(elem).map(Some(_))
          case q"new scala.Some($elem)"   => unliftableA.unapply(elem).map(Some(_))
          case q"scala.None"              => Some(None)
          case q"None"                    => Some(None)
          case _                          => None
        }
      }
      ExprCodec.make[Option[A]]
    }
    override def SomeExprCodec[A: ExprCodec: Type]: ExprCodec[Some[A]] = {
      implicit val liftable: Liftable[A] = platformSpecific.implicits.ExprCodecLiftable[A]
      implicit val unliftableA: Unliftable[A] = platformSpecific.implicits.ExprCodecUnliftable[A]
      implicit val unliftable: Unliftable[Some[A]] = new Unliftable[Some[A]] {
        def unapply(tree: Tree): Option[Some[A]] = tree match {
          case q"scala.Some.apply($elem)" => unliftableA.unapply(elem).map(Some(_))
          case q"scala.Some($elem)"       => unliftableA.unapply(elem).map(Some(_))
          case q"Some.apply($elem)"       => unliftableA.unapply(elem).map(Some(_))
          case q"Some($elem)"             => unliftableA.unapply(elem).map(Some(_))
          case q"new scala.Some($elem)"   => unliftableA.unapply(elem).map(Some(_))
          case _                          => None
        }
      }
      ExprCodec.make[Some[A]]
    }
    override lazy val NoneExprCodec: ExprCodec[None.type] = {
      implicit val unliftable: Unliftable[None.type] = new Unliftable[None.type] {
        def unapply(tree: Tree): Option[None.type] = tree match {
          case q"scala.None" => Some(None)
          case q"None"       => Some(None)
          case _             => None
        }
      }
      ExprCodec.make[None.type]
    }
    override def EitherExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Either[L, R]] = {
      implicit val liftableL: Liftable[L] = platformSpecific.implicits.ExprCodecLiftable[L]
      implicit val liftableR: Liftable[R] = platformSpecific.implicits.ExprCodecLiftable[R]
      implicit val unliftableL: Unliftable[L] = platformSpecific.implicits.ExprCodecUnliftable[L]
      implicit val unliftableR: Unliftable[R] = platformSpecific.implicits.ExprCodecUnliftable[R]
      implicit val unliftable: Unliftable[Either[L, R]] = new Unliftable[Either[L, R]] {
        def unapply(tree: Tree): Option[Either[L, R]] = tree match {
          case q"scala.util.Left.apply($elem)"  => unliftableL.unapply(elem).map(Left(_))
          case q"scala.util.Left($elem)"        => unliftableL.unapply(elem).map(Left(_))
          case q"Left.apply($elem)"             => unliftableL.unapply(elem).map(Left(_))
          case q"Left($elem)"                   => unliftableL.unapply(elem).map(Left(_))
          case q"scala.util.Right.apply($elem)" => unliftableR.unapply(elem).map(Right(_))
          case q"scala.util.Right($elem)"       => unliftableR.unapply(elem).map(Right(_))
          case q"Right.apply($elem)"            => unliftableR.unapply(elem).map(Right(_))
          case q"Right($elem)"                  => unliftableR.unapply(elem).map(Right(_))
          case _                                => None
        }
      }
      ExprCodec.make[Either[L, R]]
    }
    override def LeftExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Left[L, R]] = {
      implicit val liftableL: Liftable[L] = platformSpecific.implicits.ExprCodecLiftable[L]
      implicit val unliftableL: Unliftable[L] = platformSpecific.implicits.ExprCodecUnliftable[L]
      implicit val unliftable: Unliftable[Left[L, R]] = new Unliftable[Left[L, R]] {
        def unapply(tree: Tree): Option[Left[L, R]] = tree match {
          case q"scala.util.Left.apply($elem)" => unliftableL.unapply(elem).map(Left(_))
          case q"scala.util.Left($elem)"       => unliftableL.unapply(elem).map(Left(_))
          case q"Left.apply($elem)"            => unliftableL.unapply(elem).map(Left(_))
          case q"Left($elem)"                  => unliftableL.unapply(elem).map(Left(_))
          case q"new scala.util.Left($elem)"   => unliftableL.unapply(elem).map(Left(_))
          case _                               => None
        }
      }
      ExprCodec.make[Left[L, R]]
    }
    override def RightExprCodec[L: ExprCodec: Type, R: ExprCodec: Type]: ExprCodec[Right[L, R]] = {
      implicit val liftableR: Liftable[R] = platformSpecific.implicits.ExprCodecLiftable[R]
      implicit val unliftableR: Unliftable[R] = platformSpecific.implicits.ExprCodecUnliftable[R]
      implicit val unliftable: Unliftable[Right[L, R]] = new Unliftable[Right[L, R]] {
        def unapply(tree: Tree): Option[Right[L, R]] = tree match {
          case q"scala.util.Right.apply($elem)" => unliftableR.unapply(elem).map(Right(_))
          case q"scala.util.Right($elem)"       => unliftableR.unapply(elem).map(Right(_))
          case q"Right.apply($elem)"            => unliftableR.unapply(elem).map(Right(_))
          case q"Right($elem)"                  => unliftableR.unapply(elem).map(Right(_))
          case q"new scala.util.Right($elem)"   => unliftableR.unapply(elem).map(Right(_))
          case _                                => None
        }
      }
      ExprCodec.make[Right[L, R]]
    }
  }

  final override type VarArgs[A] = Seq[Expr[A]]

  object VarArgs extends VarArgsModule {
    override def toIterable[A](args: VarArgs[A]): Iterable[Expr[A]] = args
    override def from[A: Type](iterable: Iterable[Expr[A]]): Expr[Seq[A]] =
      c.Expr[Seq[A]](q"_root_.scala.collection.immutable.Seq(..${iterable.map(_.tree).toSeq})")
  }

  import Expr.platformSpecific.*

  sealed trait MatchCase[A] extends Product with Serializable

  object MatchCase extends MatchCaseModule {

    final private case class TypeMatch[A](name: TermName, expr: Expr_??, result: A) extends MatchCase[A]

    override def typeMatch[A: Type](freshName: FreshName): MatchCase[Expr[A]] = {
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

    override val directStyle: fp.DirectStyle[MatchCase] = new fp.DirectStyle[MatchCase] {
      private val saved = scala.collection.mutable.Map.empty[Any, (TermName, Expr_??)]

      override protected def scopedUnsafe[A](owner: fp.DirectStyle.ScopeOwner[MatchCase])(thunk: => A): MatchCase[A] = {
        val result = fp.effect.DirectStyleExecutor(thunk)
        val (name, expr) = saved
          .remove(owner)
          // $COVERAGE-OFF$
          .getOrElse(
            hearthRequirementFailed("MatchCase.directStyle: runSafe was not called inside scoped")
          )
        // $COVERAGE-ON$
        TypeMatch(name, expr, result)
      }

      override protected def runUnsafe[A](owner: fp.DirectStyle.ScopeOwner[MatchCase])(value: => MatchCase[A]): A =
        fp.effect.DirectStyleExecutor(value) match {
          case TypeMatch(name, expr, result) =>
            saved(owner) = (name, expr)
            result.asInstanceOf[A]
        }
    }
  }

  final class ValDefs[A] private[typed] (private val definitions: Vector[ValOrDefDef], private val value: A)

  object ValDefs extends ValDefsModule {

    override def createVal[A: Type](value: Expr[A], freshName: FreshName): ValDefs[Expr[A]] = {
      val name = freshTerm[A](freshName, value)
      new ValDefs[Expr[A]](Vector(q"val $name: ${Type[A]} = $value"), c.Expr[A](q"$name"))
    }
    override def createVar[A: Type](
        initialValue: Expr[A],
        freshName: FreshName
    ): ValDefs[(Expr[A], Expr[A] => Expr[Unit])] = {
      val name = freshTerm[A](freshName, initialValue)
      new ValDefs[(Expr[A], Expr[A] => Expr[Unit])](
        Vector(q"var $name: ${Type[A]} = $initialValue"),
        (c.Expr[A](q"$name"), (expr: Expr[A]) => c.Expr[Unit](q"$name = $expr"))
      )
    }
    override def createLazy[A: Type](value: Expr[A], freshName: FreshName): ValDefs[Expr[A]] = {
      val name = freshTerm[A](freshName, value)
      new ValDefs[Expr[A]](Vector(q"lazy val $name: ${Type[A]} = $value"), c.Expr[A](q"$name"))
    }
    override def createDef[A: Type](value: Expr[A], freshName: FreshName): ValDefs[Expr[A]] = {
      val name = freshTerm[A](freshName, value)
      new ValDefs[Expr[A]](Vector(q"def $name: ${Type[A]} = $value"), c.Expr[A](q"$name"))
    }

    override def partition[A, B, C](scoped: ValDefs[A])(f: A => Either[B, C]): Either[ValDefs[B], ValDefs[C]] =
      f(scoped.value) match {
        case Left(value)  => Left(new ValDefs[B](scoped.definitions, value))
        case Right(value) => Right(new ValDefs[C](scoped.definitions, value))
      }

    override def closeScope[A](scoped: ValDefs[Expr[A]]): Expr[A] =
      if (scoped.definitions.isEmpty) scoped.value
      else
        c.Expr[A](q"..${scoped.definitions}; ${scoped.value}")

    override val traverse: fp.ApplicativeTraverse[ValDefs] = new fp.ApplicativeTraverse[ValDefs] {

      override def pure[A](a: A): ValDefs[A] = new ValDefs[A](Vector.empty, a)

      override def map2[A, B, C](fa: ValDefs[A], fb: => ValDefs[B])(f: (A, B) => C): ValDefs[C] =
        new ValDefs[C](fa.definitions ++ fb.definitions, f(fa.value, fb.value))

      override def traverse[G[_]: fp.Applicative, A, B](fa: ValDefs[A])(f: A => G[B]): G[ValDefs[B]] =
        f(fa.value).map(b => new ValDefs[B](fa.definitions, b))

      override def parTraverse[G[_]: fp.Parallel, A, B](fa: ValDefs[A])(f: A => G[B]): G[ValDefs[B]] =
        f(fa.value).map(b => new ValDefs[B](fa.definitions, b))
    }

    override val directStyle: fp.DirectStyle[ValDefs] = new fp.DirectStyle[ValDefs] {
      private val saved = scala.collection.mutable.Map.empty[Any, Vector[ValOrDefDef]]

      override protected def scopedUnsafe[A](owner: fp.DirectStyle.ScopeOwner[ValDefs])(thunk: => A): ValDefs[A] = {
        val result = fp.effect.DirectStyleExecutor(thunk)
        val defs = saved.remove(owner).getOrElse(Vector.empty)
        new ValDefs[A](defs, result)
      }

      override protected def runUnsafe[A](owner: fp.DirectStyle.ScopeOwner[ValDefs])(value: => ValDefs[A]): A = {
        val vd = fp.effect.DirectStyleExecutor(value)
        saved(owner) = saved.getOrElse(owner, Vector.empty) ++ vd.definitions
        vd.value
      }
    }
  }

  final class ValDefBuilder[Signature, Returned, Value] private (
      private val mk: ValDefBuilder.Mk[Signature, Returned],
      private val value: Value
  )

  object ValDefBuilder extends ValDefBuilderModule {

    sealed private[typed] trait Mk[Signature, Returned] {

      def build(body: Expr[Returned]): ValDefs[Signature]

      def buildCached(cache: ValDefsCache, key: String, body: Expr[Returned]): ValDefsCache

      def forwardDeclare(cache: ValDefsCache, key: String): ValDefsCache
    }

    final private[typed] class MkValDef[Signature, Returned] private[typed] (
        signature: Signature,
        mkKey: String => ValDefsCache.Key,
        buildValDef: Expr[Returned] => ValOrDefDef
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
        buildVar: Expr[Returned] => ValOrDefDef
    ) extends Mk[Signature, Returned] {

      def build(body: Expr[Returned]): ValDefs[Signature] =
        new ValDefs[Signature](Vector(buildVar(body)), getter)

      def buildCached(cache: ValDefsCache, key: String, body: Expr[Returned]): ValDefsCache =
        cache
          .set(mkGetterKey(key), getter, buildVar(body))
          .set(mkSetterKey(key), setter, null.asInstanceOf[ValOrDefDef])

      def forwardDeclare(cache: ValDefsCache, key: String): ValDefsCache =
        cache.forwardDeclare(mkGetterKey(key), getter).forwardDeclare(mkSetterKey(key), setter)
    }

    override def ofVal[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Unit] = {
      val name = freshTerm[Returned](freshName, null)
      val self = c.Expr[Returned](q"$name")
      new ValDefBuilder[Expr[Returned], Returned, Unit](
        new MkValDef[Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"val $name: ${Type[Returned]} = $body"
        ),
        ()
      )
    }
    override def ofVar[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned] => Expr[Unit]] = {
      val name = freshTerm[Returned](freshName, null)
      val self = c.Expr[Returned](q"$name")
      val setter = (expr: Expr[Returned]) => c.Expr[Unit](q"$name = $expr")
      new ValDefBuilder[Expr[Returned], Returned, Expr[Returned] => Expr[Unit]](
        new MkVar[Expr[Returned], Returned](
          getter = self,
          setter = setter,
          mkGetterKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          mkSetterKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[Returned].asUntyped), Type[Unit].asUntyped),
          buildVar = (body: Expr[Returned]) => q"var $name: ${Type[Returned]} = $body"
        ),
        setter
      )
    }
    override def ofLazy[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned]] = {
      val name = freshTerm[Returned](freshName, null)
      val self = c.Expr[Returned](q"$name")
      new ValDefBuilder[Expr[Returned], Returned, Expr[Returned]](
        new MkValDef[Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"lazy val $name: ${Type[Returned]} = $body"
        ),
        self
      )
    }

    // format: off
    override def ofDef0[Returned: Type](
        freshName: FreshName
    ): ValDefBuilder[Expr[Returned], Returned, Expr[Returned]] = {
      val name = freshTerm[Returned](freshName, null)
      val self = asExpr[Returned](q"$name")
      new ValDefBuilder[Expr[Returned], Returned, Expr[Returned]](
        new MkValDef[Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq.empty, Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name: ${Type[Returned]} = $body"
        ),
        self
      )
    }
    override def ofDef1[A: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName
    ): ValDefBuilder[Expr[A] => Expr[Returned], Returned, (Expr[A] => Expr[Returned], Expr[A])] = {
      val a1 = freshTerm[A](freshA, null)
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A]) => asExpr[Returned](q"$name($a)")
      new ValDefBuilder[Expr[A] => Expr[Returned], Returned, (Expr[A] => Expr[Returned], Expr[A])](
        new MkValDef[Expr[A] => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}): ${Type[Returned]} = $body"
        ),
        (self, c.Expr[A](q"$a1"))
      )
    }
    override def ofDef2[A: Type, B: Type, Returned: Type](
        freshName: FreshName,
        freshA: FreshName,
        freshB: FreshName
    ): ValDefBuilder[(Expr[A], Expr[B]) => Expr[Returned], Returned, ((Expr[A], Expr[B]) => Expr[Returned], (Expr[A], Expr[B]))] = {
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B]) => asExpr[Returned](q"$name($a, $b)")
      new ValDefBuilder[(Expr[A], Expr[B]) => Expr[Returned], Returned, ((Expr[A], Expr[B]) => Expr[Returned], (Expr[A], Expr[B]))](
        new MkValDef[(Expr[A], Expr[B]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C]) => asExpr[Returned](q"$name($a, $b, $c)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C]) => Expr[Returned], (Expr[A], Expr[B], Expr[C]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D]) => asExpr[Returned](q"$name($a, $b, $c, $d)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val p1 = freshTerm[P](freshP, null)
      val pExpr = c.Expr[P](q"$p1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val p1 = freshTerm[P](freshP, null)
      val pExpr = c.Expr[P](q"$p1")
      val q1 = freshTerm[Q](freshQ, null)
      val qExpr = c.Expr[Q](q"$q1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val p1 = freshTerm[P](freshP, null)
      val pExpr = c.Expr[P](q"$p1")
      val q1 = freshTerm[Q](freshQ, null)
      val qExpr = c.Expr[Q](q"$q1")
      val r1 = freshTerm[R](freshR, null)
      val rExpr = c.Expr[R](q"$r1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val p1 = freshTerm[P](freshP, null)
      val pExpr = c.Expr[P](q"$p1")
      val q1 = freshTerm[Q](freshQ, null)
      val qExpr = c.Expr[Q](q"$q1")
      val r1 = freshTerm[R](freshR, null)
      val rExpr = c.Expr[R](q"$r1")
      val s1 = freshTerm[S](freshS, null)
      val sExpr = c.Expr[S](q"$s1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val p1 = freshTerm[P](freshP, null)
      val pExpr = c.Expr[P](q"$p1")
      val q1 = freshTerm[Q](freshQ, null)
      val qExpr = c.Expr[Q](q"$q1")
      val r1 = freshTerm[R](freshR, null)
      val rExpr = c.Expr[R](q"$r1")
      val s1 = freshTerm[S](freshS, null)
      val sExpr = c.Expr[S](q"$s1")
      val t1 = freshTerm[T](freshT, null)
      val tExpr = c.Expr[T](q"$t1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}, $t1: ${Type[T]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val p1 = freshTerm[P](freshP, null)
      val pExpr = c.Expr[P](q"$p1")
      val q1 = freshTerm[Q](freshQ, null)
      val qExpr = c.Expr[Q](q"$q1")
      val r1 = freshTerm[R](freshR, null)
      val rExpr = c.Expr[R](q"$r1")
      val s1 = freshTerm[S](freshS, null)
      val sExpr = c.Expr[S](q"$s1")
      val t1 = freshTerm[T](freshT, null)
      val tExpr = c.Expr[T](q"$t1")
      val u1 = freshTerm[U](freshU, null)
      val uExpr = c.Expr[U](q"$u1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped, Type[U].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}, $t1: ${Type[T]}, $u1: ${Type[U]}): ${Type[Returned]} = $body"
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
      val a1 = freshTerm[A](freshA, null)
      val aExpr = c.Expr[A](q"$a1")
      val b1 = freshTerm[B](freshB, null)
      val bExpr = c.Expr[B](q"$b1")
      val c1 = freshTerm[C](freshC, null)
      val cExpr = c.Expr[C](q"$c1")
      val d1 = freshTerm[D](freshD, null)
      val dExpr = c.Expr[D](q"$d1")
      val e1 = freshTerm[E](freshE, null)
      val eExpr = c.Expr[E](q"$e1")
      val f1 = freshTerm[F](freshF, null)
      val fExpr = c.Expr[F](q"$f1")
      val g1 = freshTerm[G](freshG, null)
      val gExpr = c.Expr[G](q"$g1")
      val h1 = freshTerm[H](freshH, null)
      val hExpr = c.Expr[H](q"$h1")
      val i1 = freshTerm[I](freshI, null)
      val iExpr = c.Expr[I](q"$i1")
      val j1 = freshTerm[J](freshJ, null)
      val jExpr = c.Expr[J](q"$j1")
      val k1 = freshTerm[K](freshK, null)
      val kExpr = c.Expr[K](q"$k1")
      val l1 = freshTerm[L](freshL, null)
      val lExpr = c.Expr[L](q"$l1")
      val m1 = freshTerm[M](freshM, null)
      val mExpr = c.Expr[M](q"$m1")
      val n1 = freshTerm[N](freshN, null)
      val nExpr = c.Expr[N](q"$n1")
      val o1 = freshTerm[O](freshO, null)
      val oExpr = c.Expr[O](q"$o1")
      val p1 = freshTerm[P](freshP, null)
      val pExpr = c.Expr[P](q"$p1")
      val q1 = freshTerm[Q](freshQ, null)
      val qExpr = c.Expr[Q](q"$q1")
      val r1 = freshTerm[R](freshR, null)
      val rExpr = c.Expr[R](q"$r1")
      val s1 = freshTerm[S](freshS, null)
      val sExpr = c.Expr[S](q"$s1")
      val t1 = freshTerm[T](freshT, null)
      val tExpr = c.Expr[T](q"$t1")
      val u1 = freshTerm[U](freshU, null)
      val uExpr = c.Expr[U](q"$u1")
      val v1 = freshTerm[V](freshV, null)
      val vExpr = c.Expr[V](q"$v1")
      val name = freshTerm[Returned](freshName, null)
      val self = (a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U], v: Expr[V]) => asExpr[Returned](q"$name($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v)")
      new ValDefBuilder[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], Returned, ((Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], (Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]))](
        new MkValDef[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned], Returned](
          signature = self,
          mkKey = (key: String) => new ValDefsCache.Key(key, Seq(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped, Type[F].asUntyped, Type[G].asUntyped, Type[H].asUntyped, Type[I].asUntyped, Type[J].asUntyped, Type[K].asUntyped, Type[L].asUntyped, Type[M].asUntyped, Type[N].asUntyped, Type[O].asUntyped, Type[P].asUntyped, Type[Q].asUntyped, Type[R].asUntyped, Type[S].asUntyped, Type[T].asUntyped, Type[U].asUntyped, Type[V].asUntyped), Type[Returned].asUntyped),
          buildValDef = (body: Expr[Returned]) => q"def $name($a1: ${Type[A]}, $b1: ${Type[B]}, $c1: ${Type[C]}, $d1: ${Type[D]}, $e1: ${Type[E]}, $f1: ${Type[F]}, $g1: ${Type[G]}, $h1: ${Type[H]}, $i1: ${Type[I]}, $j1: ${Type[J]}, $k1: ${Type[K]}, $l1: ${Type[L]}, $m1: ${Type[M]}, $n1: ${Type[N]}, $o1: ${Type[O]}, $p1: ${Type[P]}, $q1: ${Type[Q]}, $r1: ${Type[R]}, $s1: ${Type[S]}, $t1: ${Type[T]}, $u1: ${Type[U]}, $v1: ${Type[V]}): ${Type[Returned]} = $body"
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

    override def directStyle[Signature, Returned]: fp.DirectStyle[ValDefBuilder[Signature, Returned, *]] =
      new fp.DirectStyle[ValDefBuilder[Signature, Returned, *]] {
        private val saved = scala.collection.mutable.Map.empty[Any, Mk[Signature, Returned]]

        override protected def scopedUnsafe[A](
            owner: fp.DirectStyle.ScopeOwner[ValDefBuilder[Signature, Returned, *]]
        )(thunk: => A): ValDefBuilder[Signature, Returned, A] = {
          val result = fp.effect.DirectStyleExecutor(thunk)
          val mk = saved
            .remove(owner)
            // $COVERAGE-OFF$
            .getOrElse(
              hearthRequirementFailed("ValDefBuilder.directStyle: runSafe was not called inside scoped")
            )
          // $COVERAGE-ON$
          new ValDefBuilder[Signature, Returned, A](mk, result)
        }

        override protected def runUnsafe[A](
            owner: fp.DirectStyle.ScopeOwner[ValDefBuilder[Signature, Returned, *]]
        )(value: => ValDefBuilder[Signature, Returned, A]): A = {
          val vdb = fp.effect.DirectStyleExecutor(value)
          saved(owner) = vdb.mk
          vdb.value
        }
      }
  }

  final class ValDefsCache private[typed] (val definitions: ListMap[ValDefsCache.Key, ValDefsCache.Value]) {

    private[typed] def forwardDeclare(key: ValDefsCache.Key, signature: Any): ValDefsCache =
      new ValDefsCache(definitions.updated(key, new ValDefsCache.Value(signature, None)))

    private[typed] def set(key: ValDefsCache.Key, signature: Any, definition: ValOrDefDef): ValDefsCache = {
      // $COVERAGE-OFF$
      if (definitions.get(key).exists(_.signature != signature)) {
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

    final private[typed] case class Key(name: String, args: Seq[UntypedType], returned: UntypedType) {

      override def hashCode(): Int = name.hashCode()

      override def equals(other: Any): Boolean = other match {
        case that: Key =>
          name == that.name && args.length == that.args.length && {
            val length = args.length
            var i = 0
            while (i < length && args(i) =:= that.args(i))
              i += 1
            i == length
          } && returned =:= that.returned
        case _ => false
      }

      override def toString: String =
        s"def $name(${args.view.map(_.prettyPrint).mkString(", ")}): ${returned.prettyPrint}"
    }

    final private[typed] case class Value(signature: Any, definition: Option[ValOrDefDef])

    override def empty: ValDefsCache = new ValDefsCache(ListMap.empty)

    override def merge(cache1: ValDefsCache, cache2: ValDefsCache): ValDefsCache = {
      val keys = scala.collection.immutable.ListSet.from(cache1.definitions.keys ++ cache2.definitions.keys)
      val result = keys.view.map { key =>
        (cache1.definitions.get(key), cache2.definitions.get(key)) match {
          case (Some(value1), Some(value2)) =>
            // $COVERAGE-OFF$
            if (value1.signature != value2.signature) {
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
                if (value1.definition != value2.definition) {
                  hearthRequirementFailed(
                    s"Def with key $key already exists with different definition, you probably created it twice in 2 branches, without noticing"
                  )
                }
                // $COVERAGE-ON$
                (key, value1)
            }
          case (Some(value), None) => (key, value)
          case (None, Some(value)) => (key, value)
          // $COVERAGE-OFF$
          case (None, None) => ??? // impossible
          // $COVERAGE-ON$
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
      if (pending.nonEmpty) {
        // $COVERAGE-OFF$
        hearthRequirementFailed(
          s"""Definitions were forward declared, but not built:
             |${pending.map(p => "  " + p.toString).mkString("\n")}
             |Make sure, that you built all the forwarded definitions.
             |Also, make sure, that you build forwrded definitions as a part of the ValDefsCache, not outside of it.definitions
             |Otherwise you would leak some definition outside if the scope it is available in which this check prevents.
             |""".stripMargin
        )
        // $COVERAGE-ON$
      } else {
        new ValDefs[Unit](definitions.toVector, ())
      }
    }
  }

  final class LambdaBuilder[From[_], To] private (private val mk: LambdaBuilder.Mk[From], private val value: To)

  object LambdaBuilder extends LambdaBuilderModule {

    private trait Mk[From[_]] {

      def apply[To: Type](body: Expr[To]): Expr[From[To]]
    }

    // format: off
    override def of1[A: Type](
        freshA: FreshName
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
        freshA: FreshName,
        freshB: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName
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
        freshA: FreshName,
        freshB: FreshName,
        freshC: FreshName,
        freshD: FreshName,
        freshE: FreshName,
        freshF: FreshName,
        freshG: FreshName,
        freshH: FreshName
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

    override def directStyle[From[_]]: fp.DirectStyle[LambdaBuilder[From, *]] =
      new fp.DirectStyle[LambdaBuilder[From, *]] {
        private val saved = scala.collection.mutable.Map.empty[Any, Mk[From]]

        override protected def scopedUnsafe[A](
            owner: fp.DirectStyle.ScopeOwner[LambdaBuilder[From, *]]
        )(thunk: => A): LambdaBuilder[From, A] = {
          val result = fp.effect.DirectStyleExecutor(thunk)
          val mk = saved
            .remove(owner)
            // $COVERAGE-OFF$
            .getOrElse(
              hearthRequirementFailed("LambdaBuilder.directStyle: runSafe was not called inside scoped")
            )
          // $COVERAGE-ON$
          new LambdaBuilder[From, A](mk, result)
        }

        override protected def runUnsafe[A](
            owner: fp.DirectStyle.ScopeOwner[LambdaBuilder[From, *]]
        )(value: => LambdaBuilder[From, A]): A = {
          val lb = fp.effect.DirectStyleExecutor(value)
          saved(owner) = lb.mk
          lb.value
        }
      }
  }
}
