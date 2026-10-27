package hearth
package typed

import hearth.data.Data
import hearth.examples.exprsedge.RuntimeShow
import hearth.fp.syntax.*

/** Fixtures checking the documented edge cases of [[Exprs]]: built-in codecs, `semiEval`/`semiQuote`, `VarArgs`,
  * `FreshName`, `MatchCase`, `ValDefs`, `ValDefBuilder`, `ValDefsCache` and `LambdaBuilder`.
  */
trait ExprsEdgeCasesFixturesImpl { this: MacroCommons =>

  // Cross-quotes Type.of cannot be assigned directly to a local implicit val (on Scala 2 the expansion would reference
  // the implicit being defined), so the types are kept in private vals and made implicit locally.
  private val IntTypeEE: Type[Int] = Type.of[Int]
  private val LongTypeEE: Type[Long] = Type.of[Long]
  private val ByteTypeEE: Type[Byte] = Type.of[Byte]
  private val ShortTypeEE: Type[Short] = Type.of[Short]
  private val FloatTypeEE: Type[Float] = Type.of[Float]
  private val CharTypeEE: Type[Char] = Type.of[Char]
  private val UnitTypeEE: Type[Unit] = Type.of[Unit]
  private val StringTypeEE: Type[String] = Type.of[String]
  private val BooleanTypeEE: Type[Boolean] = Type.of[Boolean]
  private val AnyTypeEE: Type[Any] = Type.of[Any]
  private val DataTypeEE: Type[Data] = Type.of[Data]
  private val BigIntTypeEE: Type[BigInt] = Type.of[BigInt]
  private val BigDecimalTypeEE: Type[BigDecimal] = Type.of[BigDecimal]
  private val OptionIntTypeEE: Type[Option[Int]] = Type.of[Option[Int]]
  private val ListIntTypeEE: Type[List[Int]] = Type.of[List[Int]]
  private val ListStringTypeEE: Type[List[String]] = Type.of[List[String]]
  private val EitherStringIntTypeEE: Type[Either[String, Int]] = Type.of[Either[String, Int]]
  private val SetIntTypeEE: Type[Set[Int]] = Type.of[Set[Int]]
  private val MapStringIntTypeEE: Type[Map[String, Int]] = Type.of[Map[String, Int]]
  private val BoxIntTypeEE: Type[hearth.examples.exprsedge.Box[Int]] = Type.of[hearth.examples.exprsedge.Box[Int]]
  private val WithCollectionsTypeEE: Type[hearth.examples.exprsedge.WithCollections] =
    Type.of[hearth.examples.exprsedge.WithCollections]
  private val WithSealedFieldTypeEE: Type[hearth.examples.exprsedge.WithSealedField] =
    Type.of[hearth.examples.exprsedge.WithSealedField]
  private val ArithTypeEE: Type[hearth.examples.exprsedge.Arith] = Type.of[hearth.examples.exprsedge.Arith]
  private val ServerConfigTypeEE: Type[hearth.examples.expr_codecs.ServerConfig] =
    Type.of[hearth.examples.expr_codecs.ServerConfig]

  private def dataMapOf(entries: List[(String, Expr[Data])]): Expr[Data] = {
    implicit val dataType: Type[Data] = DataTypeEE
    implicit val stringType: Type[String] = StringTypeEE
    val keys: Expr[List[String]] = Expr(entries.map(_._1))
    val values: Expr[Seq[Data]] = VarArgs.from(entries.map(_._2))
    Expr.quote(Data.map(Expr.splice(keys).zip(Expr.splice(values))*))
  }

  private def renderSemiEval[A](result: Either[fp.data.NonEmptyVector[String], A]): Data = result match {
    case Right(value) =>
      if (value == null) Data("null") else Data(s"${RuntimeShow.show(value)} (${value.getClass.getName})")
    case Left(errors) => Data.list(errors.toList.map(error => Data(removeAnsiColors(error)))*)
  }

  // Expr.typeOf / expr.tpe

  def testExprTpe[A: Type](expr: Expr[A]): Expr[Data] = {
    implicit val stringType: Type[String] = StringTypeEE
    Expr(
      Data.map(
        "tpe <:< String" -> Data(expr.tpe <:< Type[String]),
        "tpe =:= A" -> Data(expr.tpe =:= Type[A])
      )
    )
  }

  // Expr_?? and UntypedExpr

  def testExistentialExpr[A: Type](expr: Expr[A]): Expr[Data] = {
    val existential = expr.as_??
    import existential.{Underlying as Param, value as paramExpr}
    val viaUntyped = UntypedExpr.as_??(expr.asUntyped)
    val viaKnownType = UntypedExpr.as_??(expr.asUntyped, UntypedType.fromTyped[A])
    import viaKnownType.Underlying as Known
    Expr(
      Data.map(
        "existential.plainPrint" -> Data(existential.plainPrint),
        "existential.Underlying" -> Data(Type[Param].plainPrint),
        "typed roundtrip" -> Data(paramExpr.asUntyped.asTyped[Param].plainPrint),
        "UntypedExpr.as_??" -> Data(viaUntyped.plainPrint),
        "UntypedExpr.as_?? with known type" -> Data(Type[Known].plainPrint)
      )
    )
  }

  // SummoningResult

  def testSummoningOutcome[A: Type]: Expr[Data] = {
    val result = Expr.summonImplicit[A]
    Expr(
      Data.map(
        "outcome" -> Data(
          result.fold(
            found = _ => "found",
            ambiguous = _ => "ambiguous",
            diverging = _ => "diverging",
            notFound = _ => "notFound"
          )
        ),
        "isDefined" -> Data(result.isDefined),
        "isEmpty" -> Data(result.isEmpty),
        "nonEmpty" -> Data(result.nonEmpty),
        "toOption" -> Data(result.toOption.isDefined),
        "get" -> Data(
          try {
            val _ = result.get
            "returned"
          } catch {
            case _: NoSuchElementException => "NoSuchElementException"
          }
        )
      )
    )
  }

  // Expr.value / Expr.unapply

  def testExprValueInt(expr: Expr[Int]): Expr[Data] =
    Expr(Data(expr.value.fold("<none>")(_.toString)))

  def testExprValueString(expr: Expr[String]): Expr[Data] =
    Expr(Data(expr match {
      case Expr(value) => value
      case _           => "<none>"
    }))

  // semiEval

  def testSemiEvalOutcome[A: Type](expr: Expr[A]): Expr[Data] =
    Expr(expr.semiEval match {
      case success @ Right(_) => renderSemiEval(success)
      case Left(_)            => Data("<failed>")
    })

  def testSemiEvalApply[F: Type](function: Expr[F], args: VarArgs[Any]): Expr[Data] = {
    val evaluatedArgs = args.toList.map(_.semiEval)
    val failures = evaluatedArgs.collect { case Left(errors) => errors.toList }.flatten
    val result: Either[fp.data.NonEmptyVector[String], Any] =
      if (failures.nonEmpty) Left(fp.data.NonEmptyVector(failures.head, failures.tail*))
      else
        function.semiEval.flatMap { value =>
          val xs = evaluatedArgs.collect { case Right(arg) => arg }
          try Right(xs.length match {
            case 0 => value.asInstanceOf[() => Any]()
            case 1 => value.asInstanceOf[Any => Any](xs(0))
            case 2 => value.asInstanceOf[(Any, Any) => Any](xs(0), xs(1))
            case 3 => value.asInstanceOf[(Any, Any, Any) => Any](xs(0), xs(1), xs(2))
            case _ =>
              // Above arity 22 Scala 3 compiles lambdas to `scala.runtime.FunctionXXL`, whose only `apply` takes all
              // arguments as one array.
              val applyAll = value.getClass.getMethods
                .find(m => m.getName == "apply" && m.getParameterTypes.toList == List(classOf[Array[Object]]))
                .getOrElse(throw new NoSuchMethodException(s"apply(Object[]) on ${value.getClass.getName}"))
              val argArray: Array[Object] = xs.map(_.asInstanceOf[Object]).toArray
              applyAll.invoke(value, argArray)
          })
          catch {
            // A lambda body is only evaluated when the function is applied, so its failures surface here.
            case e: Throwable =>
              val cause = Iterator.iterate(e: Throwable)(_.getCause).takeWhile(_ != null).toList.last
              Left(fp.data.NonEmptyVector(s"<threw on apply: ${cause.getClass.getSimpleName}: ${cause.getMessage}>"))
          }
        }
    Expr(renderSemiEval(result))
  }

  def testSemiEvalWithOverride[A: Type](mode: Expr[String], expr: Expr[A]): Expr[Data] = {
    implicit val stringType: Type[String] = StringTypeEE
    val noOverride: Existential[EvalOverride] = {
      implicit val anyType: Type[Any] = AnyTypeEE
      Existential[EvalOverride, Any](EvalOverride.none[Any])
    }
    val stringOverride: EvalOverride[String] = mode.value match {
      case Some("replace") => EvalOverride[String](_ => Right("OVERRIDDEN"))
      case Some("fail")    => EvalOverride[String](_ => Left("strings are forbidden"))
      case _               => EvalOverride.none[String]
    }
    val overrides: UntypedType => Existential[EvalOverride] = { tpe =>
      if (tpe =:= UntypedType.fromTyped[String]) Existential[EvalOverride, String](stringOverride)
      else noOverride
    }
    Expr(renderSemiEval(expr.semiEval(overrides)))
  }

  // semiQuote

  private def semiQuotedAtRuntime[A: Type](result: Either[String, Expr[A]]): Expr[Data] = result match {
    case Right(quoted) => Expr.quote(Data(hearth.examples.exprsedge.RuntimeShow.show(Expr.splice(quoted))))
    case Left(error)   => Expr(Data(s"<failed: ${removeAnsiColors(error)}>"))
  }

  def testSemiQuoteBuiltInsAndStructures: Expr[Data] = {
    implicit val longType: Type[Long] = LongTypeEE
    implicit val byteType: Type[Byte] = ByteTypeEE
    implicit val shortType: Type[Short] = ShortTypeEE
    implicit val floatType: Type[Float] = FloatTypeEE
    implicit val charType: Type[Char] = CharTypeEE
    implicit val unitType: Type[Unit] = UnitTypeEE
    implicit val stringType: Type[String] = StringTypeEE
    implicit val dataType: Type[Data] = DataTypeEE
    implicit val bigIntType: Type[BigInt] = BigIntTypeEE
    implicit val bigDecimalType: Type[BigDecimal] = BigDecimalTypeEE
    implicit val listStringType: Type[List[String]] = ListStringTypeEE
    implicit val optionIntType: Type[Option[Int]] = OptionIntTypeEE
    implicit val setIntType: Type[Set[Int]] = SetIntTypeEE
    implicit val mapStringIntType: Type[Map[String, Int]] = MapStringIntTypeEE
    implicit val eitherType: Type[Either[String, Int]] = EitherStringIntTypeEE
    implicit val boxType: Type[hearth.examples.exprsedge.Box[Int]] = BoxIntTypeEE
    implicit val withCollectionsType: Type[hearth.examples.exprsedge.WithCollections] = WithCollectionsTypeEE
    implicit val withSealedFieldType: Type[hearth.examples.exprsedge.WithSealedField] = WithSealedFieldTypeEE
    implicit val arithType: Type[hearth.examples.exprsedge.Arith] = ArithTypeEE
    dataMapOf(
      List(
        "Long" -> semiQuotedAtRuntime(Expr.semiQuote(Long.MinValue)),
        "Byte" -> semiQuotedAtRuntime(Expr.semiQuote(Byte.MinValue)),
        "Short" -> semiQuotedAtRuntime(Expr.semiQuote(Short.MinValue)),
        "Float" -> semiQuotedAtRuntime(Expr.semiQuote(1.5f)),
        "Char" -> semiQuotedAtRuntime(Expr.semiQuote('\n')),
        "Unit" -> semiQuotedAtRuntime(Expr.semiQuote(())),
        "String with escapes" -> semiQuotedAtRuntime(Expr.semiQuote("q\"n\nt\tuéb\\")),
        "BigInt" -> semiQuotedAtRuntime(Expr.semiQuote(BigInt("123456789012345678901234567890"))),
        "BigDecimal" -> semiQuotedAtRuntime(Expr.semiQuote(BigDecimal("-3.14159265358979323846"))),
        "Data" -> semiQuotedAtRuntime(
          Expr.semiQuote(Data.map("list" -> Data.list(Data(1), Data("a")), "flag" -> Data(true)))
        ),
        "empty List" -> semiQuotedAtRuntime(Expr.semiQuote(List.empty[String])),
        "None" -> semiQuotedAtRuntime(Expr.semiQuote(Option.empty[Int])),
        "Set" -> semiQuotedAtRuntime(Expr.semiQuote(Set(1, 2))),
        "Map" -> semiQuotedAtRuntime(Expr.semiQuote(Map("a" -> 1))),
        "Left" -> semiQuotedAtRuntime(Expr.semiQuote[Either[String, Int]](Left("error"))),
        "generic case class" -> semiQuotedAtRuntime(Expr.semiQuote(hearth.examples.exprsedge.Box(1))),
        "case class with collection fields" -> semiQuotedAtRuntime(
          Expr.semiQuote(hearth.examples.exprsedge.WithCollections(List("a", "b"), Some(1)))
        ),
        "case class with sealed field" -> semiQuotedAtRuntime(
          Expr.semiQuote(
            hearth.examples.exprsedge.WithSealedField("label", hearth.examples.expr_codecs.Shape.Origin)
          )
        ),
        "recursive sealed hierarchy" -> semiQuotedAtRuntime(
          Expr.semiQuote[hearth.examples.exprsedge.Arith](
            hearth.examples.exprsedge.Arith.Add(
              hearth.examples.exprsedge.Arith.Lit(1),
              hearth.examples.exprsedge.Arith.Add(
                hearth.examples.exprsedge.Arith.Lit(2),
                hearth.examples.exprsedge.Arith.Lit(3)
              )
            )
          )
        )
      )
    )
  }

  def testSemiQuoteWithFailingOverride: Expr[Data] = {
    implicit val stringType: Type[String] = StringTypeEE
    implicit val serverConfigType: Type[hearth.examples.expr_codecs.ServerConfig] = ServerConfigTypeEE
    val noOverride: Existential[QuoteOverride] = {
      implicit val anyType: Type[Any] = AnyTypeEE
      Existential[QuoteOverride, Any](QuoteOverride.none[Any])
    }
    val overrides: UntypedType => Existential[QuoteOverride] = { tpe =>
      if (tpe =:= UntypedType.fromTyped[String])
        Existential[QuoteOverride, String](QuoteOverride[String](_ => Left("strings are forbidden")))
      else noOverride
    }
    dataMapOf(
      List(
        "direct" -> semiQuotedAtRuntime(Expr.semiQuote("hello", overrides)),
        "nested" -> semiQuotedAtRuntime(
          Expr.semiQuote(hearth.examples.expr_codecs.ServerConfig("localhost", 8080), overrides)
        ),
        "not overridden" -> {
          implicit val intType: Type[Int] = IntTypeEE
          semiQuotedAtRuntime(Expr.semiQuote(42, overrides))
        }
      )
    )
  }

  // built-in ExprCodecs

  private def shownAtRuntime(expr: Expr[Any]): Expr[String] = {
    Expr.quote(hearth.examples.exprsedge.RuntimeShow.show(Expr.splice(expr)))
  }

  private def codecRoundtrip[A: ExprCodec](value: A): Expr[Data] = {
    val encoded = Expr(value)
    val decoded = Expr(Expr.unapply(encoded).fold("<not decoded>")(RuntimeShow.show))
    // Only the runtime value of the encoded tree is rendered, so it can be spliced as `Any` without a `Type[A]`.
    decodedAndRuntime(decoded, shownAtRuntime(encoded.asInstanceOf[Expr[Any]]))
  }

  private def decodedAndRuntime(decoded: Expr[String], runtime: Expr[String]): Expr[Data] =
    Expr.quote(
      Data.map(
        "decoded" -> Data(Expr.splice(decoded)),
        "runtime" -> Data(Expr.splice(runtime))
      )
    )

  def testBuiltInCodecEdgeCases: Expr[Data] = {
    implicit val intType: Type[Int] = IntTypeEE
    implicit val stringType: Type[String] = StringTypeEE
    implicit val optionIntType: Type[Option[Int]] = OptionIntTypeEE
    implicit val listIntType: Type[List[Int]] = ListIntTypeEE
    implicit val eitherType: Type[Either[String, Int]] = EitherStringIntTypeEE
    dataMapOf(
      List(
        "Int.MinValue" -> codecRoundtrip(Int.MinValue),
        "Long.MinValue" -> codecRoundtrip(Long.MinValue),
        "Byte.MinValue" -> codecRoundtrip(Byte.MinValue),
        "Short.MinValue" -> codecRoundtrip(Short.MinValue),
        "negative Float" -> codecRoundtrip(-1.5f),
        "negative Double" -> codecRoundtrip(-1.5),
        "Double.NaN" -> codecRoundtrip(Double.NaN),
        "Char newline" -> codecRoundtrip('\n'),
        "Char quote" -> codecRoundtrip('\''),
        "String with escapes" -> codecRoundtrip("q\"n\nt\tuéb\\"),
        "empty String" -> codecRoundtrip(""),
        "Class[String]" -> codecRoundtrip(classOf[String]),
        "ClassTag[String]" -> codecRoundtrip(scala.reflect.classTag[String]),
        "huge BigInt" -> codecRoundtrip(BigInt("-123456789012345678901234567890")),
        "precise BigDecimal" -> codecRoundtrip(BigDecimal("3.14159265358979323846")),
        "empty Array" -> codecRoundtrip(Array.empty[Int]),
        "Array of 3" -> codecRoundtrip(Array(1, 2, 3)),
        "Seq of 3" -> codecRoundtrip(Seq(1, 2, 3)),
        "empty Seq" -> codecRoundtrip(Seq.empty[Int]),
        "empty List" -> codecRoundtrip(List.empty[Int]),
        "empty Vector" -> codecRoundtrip(Vector.empty[Int]),
        "empty Set" -> codecRoundtrip(Set.empty[Int]),
        "empty Map" -> codecRoundtrip(Map.empty[Int, Int]),
        "None as Option" -> codecRoundtrip(Option.empty[Int]),
        "Right as Either" -> codecRoundtrip[Either[Int, Int]](Right(2)),
        "List of Options" -> codecRoundtrip(List[Option[Int]](Some(1), None)),
        "Map of Lists" -> codecRoundtrip(Map[String, List[Int]]("a" -> List(1, 2))),
        "Either of List" -> codecRoundtrip[Either[String, List[Int]]](Right(List(1))),
        "Option of Either" -> codecRoundtrip[Option[Either[String, Int]]](Some(Left("x")))
      )
    )
  }

  def testDataCodecRoundtrips: Expr[Data] =
    dataMapOf(
      List(
        "null" -> codecRoundtrip(Data()),
        "Int" -> codecRoundtrip(Data(1)),
        "Long" -> codecRoundtrip(Data(2L)),
        "Float" -> codecRoundtrip(Data(1.5f)),
        "Double" -> codecRoundtrip(Data(2.5)),
        "Boolean" -> codecRoundtrip(Data(true)),
        "String" -> codecRoundtrip(Data("text")),
        "list" -> codecRoundtrip(Data.list(Data(1), Data("a"))),
        "map" -> codecRoundtrip(Data.map("k" -> Data(1), "nested" -> Data.map("x" -> Data(false)))),
        "map with 6 keys" -> codecRoundtrip(
          Data.map("f" -> Data(1), "e" -> Data(2), "d" -> Data(3), "c" -> Data(4), "b" -> Data(5), "a" -> Data(6))
        )
      )
    )

  // ExprCodec.derived

  def testExprCodecDerivedAtRuntime[A: Type](expr: Expr[A]): Expr[Data] = {
    val codec: ExprCodec[A] = ExprCodec.derived[A]
    codec.fromExpr(expr) match {
      case Some(value) =>
        val decoded = Expr(RuntimeShow.show(value))
        val reLifted = codec.toExpr(value)
        Expr.quote(
          Data.map(
            "decoded" -> Data(Expr.splice(decoded)),
            "reLifted" -> Data(hearth.examples.exprsedge.RuntimeShow.show(Expr.splice(reLifted)))
          )
        )
      case None => Expr(Data.map("decoded" -> Data("<none>")))
    }
  }

  def testSemiQuoteRoundTripAtRuntime[A: Type](expr: Expr[A]): Expr[Data] =
    Expr.semiEval(expr) match {
      case Right(value) =>
        val decoded = Expr(RuntimeShow.show(value))
        val reLifted = Expr.semiQuote(value) match {
          case Right(quoted) => Expr.quote(hearth.examples.exprsedge.RuntimeShow.show(Expr.splice(quoted)))
          case Left(error)   => Expr(s"<failed: ${removeAnsiColors(error)}>")
        }
        Expr.quote(Data.map("decoded" -> Data(Expr.splice(decoded)), "reLifted" -> Data(Expr.splice(reLifted))))
      case Left(errors) => Expr(Data.map("decoded" -> Data(s"<failed: ${errors.toList.mkString(", ")}>")))
    }

  // Expr.singletonOf

  def testSingletonOfAtRuntime[A: Type]: Expr[Data] =
    Expr.singletonOf[A] match {
      case Some(singleton) => Expr.quote(Data(Expr.splice(singleton).toString))
      case None            => Expr(Data("<none>"))
    }

  // VarArgs

  def testVarArgsConversions[A: Type](args: VarArgs[A]): Expr[Data] =
    Expr(
      Data.map(
        "toIterable" -> Data(args.toIterable.size),
        "toSeq" -> Data(args.toSeq.size),
        "toList" -> Data(args.toList.size),
        "toVector" -> Data(args.toVector.size),
        "to(List)" -> Data(args.to(List).size),
        "toIterableOption" -> Data(args.toIterableOption.fold("<None>")(elements => s"Some(${elements.size})"))
      )
    )

  // FreshName

  def testFreshNames[A: Type](expr: Expr[A]): Expr[Data] = {
    def prefixOf(freshName: FreshName): Expr[Data] =
      ValDefs.createVal(expr, freshName).use { (ref: Expr[A]) =>
        val prefix = Expr(ref.plainPrint.takeWhile(_ != '$'))
        Expr.quote(Data(Expr.splice(prefix) + ":" + Expr.splice(ref).toString))
      }
    dataMapOf(
      List(
        "FromType" -> prefixOf(FreshName.FromType),
        "FromExpr" -> prefixOf(FreshName.FromExpr),
        "FromPrefix" -> prefixOf(FreshName.FromPrefix("custom")),
        "String" -> prefixOf("implicitly")
      )
    )
  }

  // MatchCase

  def testMatchCaseEqValueAgainst[A: Type](expr: Expr[A], expected: Expr[A]): Expr[String] = {
    implicit val stringType: Type[String] = StringTypeEE
    expr.matchOn[String](
      MatchCase.eqValue[A](expected, "matched").map(_ => Expr("matched!")),
      MatchCase.typeMatch[A]("other").map { other =>
        Expr.quote("other: " + Expr.splice(other).toString)
      }
    )
  }

  def testMatchCaseTypeTestMatch[A: Type, B: Type](expr: Expr[A]): Expr[String] = {
    implicit val stringType: Type[String] = StringTypeEE
    try
      expr.matchOn[String](
        MatchCase.typeTestMatch[A, B]("narrowed").map { narrowed =>
          Expr.quote("narrowed: " + Expr.splice(narrowed).toString)
        },
        MatchCase.typeMatch[A]("other").map(_ => Expr("other"))
      )
    catch {
      case _: AssertionError => Expr("<AssertionError>")
    }
  }

  def testDirectStyleWithoutRunSafe: Expr[Data] = {
    def outcome(thunk: => Any): Expr[Data] =
      try {
        val _ = thunk
        Expr(Data("<no error>"))
      } catch {
        case _: AssertionError => Expr(Data("<AssertionError>"))
      }
    val valDefs: Expr[Int] = fp.DirectStyle[ValDefs].scoped(_ => Expr(1)).close
    dataMapOf(
      List(
        "MatchCase" -> outcome {
          import MatchCase.unsafe.*
          fp.DirectStyle[MatchCase].scoped(_ => Expr(1))
        },
        "ValDefBuilder" -> outcome {
          import ValDefBuilder.unsafe.*
          fp.DirectStyle[ValDefBuilder[Expr[Int], Int, *]].scoped(_ => Expr(1))
        },
        "LambdaBuilder" -> outcome {
          import LambdaBuilder.unsafe.*
          fp.DirectStyle[LambdaBuilder[Int => *, *]].scoped(_ => Expr(1))
        },
        "ValDefs" -> Expr.quote(Data(Expr.splice(valDefs)))
      )
    )
  }

  // ValDefs

  def testValDefsEvaluationCount(kind: Expr[String], effect: Expr[Int], useIt: Expr[Boolean]): Expr[Int] = {
    implicit val intType: Type[Int] = IntTypeEE
    def body(x: Expr[Int]): Expr[Int] = Expr.quote(if (Expr.splice(useIt)) Expr.splice(x) + Expr.splice(x) else 0)
    kind.value match {
      case Some("val")  => ValDefs.createVal(effect, "x").use(body)
      case Some("lazy") => ValDefs.createLazy(effect, "x").use(body)
      case Some("def")  => ValDefs.createDef(effect, "x").use(body)
      case other        => Environment.reportErrorAndAbort(s"Unknown definition kind: $other")
    }
  }

  def testValDefsSamePrefixDoNotClash: Expr[Int] = {
    implicit val intType: Type[Int] = IntTypeEE
    ValDefs
      .createVal(Expr(1), "a")
      .tuple(ValDefs.createVal(Expr(2), "a"))
      .tuple(ValDefs.createLazy(Expr(3), "a"))
      .use { case ((first, second), third) =>
        Expr.quote(Expr.splice(first) * 100 + Expr.splice(second) * 10 + Expr.splice(third))
      }
  }

  // ValDefBuilder

  def testValDefBuilderRecursiveDef(n: Expr[Int]): Expr[Int] = {
    implicit val intType: Type[Int] = IntTypeEE
    ValDefBuilder
      .ofDef1[Int, Int]("factorial", "n")
      .buildWith { case (self, k) =>
        Expr.quote {
          if (Expr.splice(k) <= 1) 1
          else Expr.splice(k) * Expr.splice(self(Expr.quote(Expr.splice(k) - 1)))
        }
      }
      .use(factorial => factorial(n))
  }

  def testValDefBuilderMixedTypes: Expr[String] = {
    implicit val intType: Type[Int] = IntTypeEE
    implicit val stringType: Type[String] = StringTypeEE
    implicit val booleanType: Type[Boolean] = BooleanTypeEE
    ValDefBuilder
      .ofDef3[String, Int, Boolean, String]("render", "s", "n", "upper")
      .buildWith { case (_, (s, n, upper)) =>
        Expr.quote {
          val repeated = Expr.splice(s) * Expr.splice(n)
          if (Expr.splice(upper)) repeated.toUpperCase else repeated
        }
      }
      .use(render => render(Expr("ab"), Expr(2), Expr(true)))
  }

  // ValDefsCache

  def testValDefsCacheMutualRecursion(n: Expr[Int]): Expr[Data] = {
    implicit val intType: Type[Int] = IntTypeEE
    implicit val booleanType: Type[Boolean] = BooleanTypeEE
    implicit val stringType: Type[String] = StringTypeEE

    val evenBuilder = ValDefBuilder.ofDef1[Int, Boolean]("isEven", "n")
    val oddBuilder = ValDefBuilder.ofDef1[Int, Boolean]("isOdd", "n")

    var cache = ValDefsCache.empty
    val visibleBeforeDeclare = cache.get1Ary[Int, Boolean]("even").isDefined
    cache = evenBuilder.forwardDeclare(cache, "even")
    cache = oddBuilder.forwardDeclare(cache, "odd")
    val visibleAfterDeclare = cache.get1Ary[Int, Boolean]("even").isDefined
    val builtAfterDeclare = evenBuilder.isBuilt(cache, "even")

    val callEven = cache.get1Ary[Int, Boolean]("even").get
    val callOdd = cache.get1Ary[Int, Boolean]("odd").get

    cache = evenBuilder.buildCachedWith(cache, "even") { case (_, k) =>
      Expr.quote(if (Expr.splice(k) == 0) true else Expr.splice(callOdd(Expr.quote(Expr.splice(k) - 1))))
    }
    cache = oddBuilder.buildCachedWith(cache, "odd") { case (_, k) =>
      Expr.quote(if (Expr.splice(k) == 0) false else Expr.splice(callEven(Expr.quote(Expr.splice(k) - 1))))
    }

    val flags = Data.map(
      "visible before forwardDeclare" -> Data(visibleBeforeDeclare),
      "visible after forwardDeclare" -> Data(visibleAfterDeclare),
      "isBuilt after forwardDeclare" -> Data(builtAfterDeclare),
      "isBuilt after buildCachedWith" -> Data(evenBuilder.isBuilt(cache, "even")),
      "missing key" -> Data(cache.get1Ary[Int, Boolean]("missing").isDefined),
      "wrong parameter type" -> Data(cache.get1Ary[String, Boolean]("even").isDefined),
      "wrong return type" -> Data(cache.get1Ary[Int, Int]("even").isDefined),
      "wrong arity" -> Data(cache.get2Ary[Int, Int, Boolean]("even").isDefined)
    )
    val flagsExpr = Expr(flags)

    cache.toValDefs.use { _ =>
      Expr.quote {
        Data.map(
          "isEven(n)" -> Data(Expr.splice(callEven(n))),
          "isOdd(n)" -> Data(Expr.splice(callOdd(n))),
          "flags" -> Expr.splice(flagsExpr)
        )
      }
    }
  }

  def testValDefsCacheMLocal: Expr[Data] = {
    implicit val intType: Type[Int] = IntTypeEE

    val defBuilder = ValDefBuilder.ofDef1[Int, Int]("times10", "a")
    def times10(a: Expr[Int]): Expr[Int] = Expr.quote(Expr.splice(a) * 10)

    val cacheLocal = ValDefsCache.mlocal
    val program = for {
      _ <- cacheLocal.forwardDeclare("defKey", defBuilder)
      declared <- cacheLocal.get1Ary[Int, Int]("defKey")
      _ <- cacheLocal.buildCachedWith("defKey", defBuilder) { case (_, a) => times10(a) }
      call <- cacheLocal.get1Ary[Int, Int]("defKey")
      cache <- cacheLocal.get
    } yield (declared.isDefined, cache.toValDefs.use(_ => call.fold(Expr(0))(_(Expr(2)))))

    val (declaredVisible, result) = program.unsafe.runSync._2.fold(errors => throw errors.head, identity)

    val duplicateLocal = ValDefsCache.mlocal
    val duplicateBuild = for {
      _ <- duplicateLocal.buildCachedWith("defKey", defBuilder) { case (_, a) => times10(a) }
      _ <- duplicateLocal.buildCachedWith("defKey", defBuilder) { case (_, a) => times10(a) }
    } yield ()
    val duplicateOutcome = duplicateBuild.unsafe.runSync._2 match {
      case Left(errors) => errors.head.getClass.getSimpleName
      case Right(_)     => "<no error>"
    }

    dataMapOf(
      List(
        "visible after forwardDeclare" -> Expr(Data(declaredVisible)),
        "result" -> Expr.quote(Data(Expr.splice(result))),
        "building the same key twice" -> Expr(Data(duplicateOutcome))
      )
    )
  }

  // LambdaBuilder

  def testLambdaBuilderMixedTypes: Expr[String] = {
    implicit val intType: Type[Int] = IntTypeEE
    implicit val stringType: Type[String] = StringTypeEE
    val lambda: Expr[(String, Int) => String] = LambdaBuilder
      .of2[String, Int]("s", "n")
      .buildWith { case (s, n) => Expr.quote(Expr.splice(s) * Expr.splice(n)) }
    Expr.quote(Expr.splice(lambda)("ab", 3))
  }
}
