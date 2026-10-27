package hearth
package typed

import hearth.data.Data
import hearth.examples.enums.WeekDay
import hearth.examples.expr_codecs.{ServerConfig, Shape}
import hearth.examples.exprsedge.*

/** Macro implementation is in [[ExprsEdgeCasesFixturesImpl]] */
final class ExprsEdgeCasesSpec extends MacroSuite {

  /** Compares all cases at once, so that a failure reports every mismatching case instead of the first one. */
  private def checkAll(cases: (String, Data, Data)*)(implicit loc: munit.Location): Unit =
    Data.map(cases.map(c => c._1 -> c._2)*) <==> Data.map(cases.map(c => c._1 -> c._3)*)

  private def bothSides(value: Any): Data = Data.map(
    "decoded" -> Data(RuntimeShow.show(value)),
    "runtime" -> Data(RuntimeShow.show(value))
  )

  private def bothSidesDerived(value: Any): Data = Data.map(
    "decoded" -> Data(RuntimeShow.show(value)),
    "reLifted" -> Data(RuntimeShow.show(value))
  )

  group("trait typed.Exprs edge cases") {

    group("type Expr") {
      import ExprsEdgeCasesFixtures.*

      test("method Expr.typeOf returns the compiler-inferred type, which may be narrower than the declared one") {
        testExprTpe[Any]("abc") <==> Data.map(
          "tpe <:< String" -> Data(true),
          "tpe =:= A" -> Data(false)
        )
      }

      test("Expr_?? exposes the Underlying type and the typed expression; UntypedExpr converts both ways") {
        testExistentialExpr(42) <==> Data.map(
          "existential.plainPrint" -> Data("42"),
          "existential.Underlying" -> Data("scala.Int"),
          "typed roundtrip" -> Data("42"),
          "UntypedExpr.as_??" -> Data("42"),
          "UntypedExpr.as_?? with known type" -> Data("scala.Int")
        )
      }

      test("method Expr.summonImplicit distinguishes found and not-found implicits") {
        testSummoningOutcome[DummyImplicit] <==> Data.map(
          "outcome" -> Data("found"),
          "isDefined" -> Data(true),
          "isEmpty" -> Data(false),
          "nonEmpty" -> Data(true),
          "toOption" -> Data(true),
          "get" -> Data("returned")
        )
        def notProduced(outcome: String): Data = Data.map(
          "outcome" -> Data(outcome),
          "isDefined" -> Data(false),
          "isEmpty" -> Data(true),
          "nonEmpty" -> Data(false),
          "toOption" -> Data(false),
          "get" -> Data("NoSuchElementException")
        )
        testSummoningOutcome[List[Int]] <==> notProduced("notFound")
      }

      test("methods Expr.unapply and expr.value return None for expressions that are not compile-time constants") {
        val runtimeInt = List(1).head
        val runtimeString = List("a").head
        val _ = (runtimeInt, runtimeString)
        checkAll(
          ("42", testExprValueInt(42), Data("42")),
          ("runtimeInt", testExprValueInt(runtimeInt), Data("<none>")),
          ("\"text\"", testExprValueString("text"), Data("text")),
          ("runtimeString", testExprValueString(runtimeString), Data("<none>"))
        )
      }

      test("method Expr.singletonOf returns the singleton of objects and enum values, and None for other types") {
        checkAll(
          ("[PlainObject.type]", testSingletonOfAtRuntime[PlainObject.type], Data("PlainObject")),
          ("[examples.expr_codecs.Sentinel.type]", testSingletonOfAtRuntime[examples.expr_codecs.Sentinel.type], Data("Sentinel")),
          ("[Shape.Origin.type]", testSingletonOfAtRuntime[Shape.Origin.type], Data("Origin")),
          ("[None.type]", testSingletonOfAtRuntime[None.type], Data("None")),
          ("[Nil.type]", testSingletonOfAtRuntime[Nil.type], Data("List()")),
          ("[WeekDay.Tue.type]", testSingletonOfAtRuntime[WeekDay.Tue.type], Data("Tue")),
          ("[ServerConfig]", testSingletonOfAtRuntime[ServerConfig], Data("<none>")),
          ("[Shape]", testSingletonOfAtRuntime[Shape], Data("<none>")),
          ("[Int]", testSingletonOfAtRuntime[Int], Data("<none>"))
        )
      }
    }

    group("method Expr.semiEval") {
      import ExprsEdgeCasesFixtures.{testSemiEvalApply, testSemiEvalOutcome, testSemiEvalWithOverride}

      // Operands are vals with explicit types (not constants), so that the compiler cannot fold the operators away
      // before semiEval sees them.
      test("should evaluate integer arithmetic, bitwise and unary operators") {
        checkAll(
          ("PlainObject.answer % 5", testSemiEvalOutcome(PlainObject.answer % 5), Data("2 (java.lang.Integer)")),
          ("PlainObject.answer & 15", testSemiEvalOutcome(PlainObject.answer & 15), Data("10 (java.lang.Integer)")),
          ("PlainObject.answer | 1", testSemiEvalOutcome(PlainObject.answer | 1), Data("43 (java.lang.Integer)")),
          ("PlainObject.answer ^ 2", testSemiEvalOutcome(PlainObject.answer ^ 2), Data("40 (java.lang.Integer)")),
          ("PlainObject.answer << 1", testSemiEvalOutcome(PlainObject.answer << 1), Data("84 (java.lang.Integer)")),
          ("PlainObject.answer >> 1", testSemiEvalOutcome(PlainObject.answer >> 1), Data("21 (java.lang.Integer)")),
          ("PlainObject.answer >>> 1", testSemiEvalOutcome(PlainObject.answer >>> 1), Data("21 (java.lang.Integer)")),
          ("~PlainObject.answer", testSemiEvalOutcome(~PlainObject.answer), Data("-43 (java.lang.Integer)")),
          ("-PlainObject.answer", testSemiEvalOutcome(-PlainObject.answer), Data("-42 (java.lang.Integer)"))
        )
      }

      test("should evaluate comparison and Boolean operators") {
        checkAll(
          ("PlainObject.answer <= 42", testSemiEvalOutcome(PlainObject.answer <= 42), Data("true (java.lang.Boolean)")),
          ("PlainObject.answer >= 43", testSemiEvalOutcome(PlainObject.answer >= 43), Data("false (java.lang.Boolean)")),
          ("PlainObject.answer < 0", testSemiEvalOutcome(PlainObject.answer < 0), Data("false (java.lang.Boolean)")),
          ("PlainObject.answer == 42", testSemiEvalOutcome(PlainObject.answer == 42), Data("true (java.lang.Boolean)")),
          ("PlainObject.answer != 42", testSemiEvalOutcome(PlainObject.answer != 42), Data("false (java.lang.Boolean)")),
          ("PlainObject.flag && false", testSemiEvalOutcome(PlainObject.flag && false), Data("false (java.lang.Boolean)")),
          ("PlainObject.flag || false", testSemiEvalOutcome(PlainObject.flag || false), Data("true (java.lang.Boolean)")),
          ("!PlainObject.flag", testSemiEvalOutcome(!PlainObject.flag), Data("false (java.lang.Boolean)"))
        )
      }

      test("should evaluate operators on Char, Long and Double, widening mixed operands") {
        checkAll(
          ("PlainObject.letter + 1", testSemiEvalOutcome(PlainObject.letter + 1), Data("98 (java.lang.Integer)")),
          ("PlainObject.big + 1L", testSemiEvalOutcome(PlainObject.big + 1L), Data("10000000001 (java.lang.Long)")),
          ("PlainObject.answer + PlainObject.big", testSemiEvalOutcome(PlainObject.answer + PlainObject.big), Data("10000000042 (java.lang.Long)")),
          ("PlainObject.ratio * 2", testSemiEvalOutcome(PlainObject.ratio * 2), Data("3.0 (java.lang.Double)")),
          ("PlainObject.ratio / 2", testSemiEvalOutcome(PlainObject.ratio / 2), Data("0.75 (java.lang.Double)")),
          ("PlainObject.answer.toLong", testSemiEvalOutcome(PlainObject.answer.toLong), Data("42 (java.lang.Long)"))
        )
      }

      test("should evaluate references to objects on the classpath, including non-case objects and Nil") {
        checkAll(
          ("PlainObject", testSemiEvalOutcome(PlainObject), Data("PlainObject (hearth.examples.exprsedge.PlainObject$)")),
          ("PlainObject.answer", testSemiEvalOutcome(PlainObject.answer), Data("42 (java.lang.Integer)")),
          ("Nil", testSemiEvalOutcome(Nil), Data("List() (scala.collection.immutable.Nil$)")),
          ("Shape.Origin", testSemiEvalOutcome(Shape.Origin), Data("Origin (hearth.examples.expr_codecs.Shape$Origin$)")),
          ("WeekDay.Mon", testSemiEvalOutcome(WeekDay.Mon), Data("Mon (scala.Enumeration$Val)"))
        )
      }

      test("should evaluate constructor calls with new, and case class / sealed trait values") {
        checkAll(
          ("new PlainClass(1, \"a\")", testSemiEvalOutcome(new PlainClass(1, "a")), Data("PlainClass(1, a) (hearth.examples.exprsedge.PlainClass)")),
          ("ServerConfig(\"host\", 1)", testSemiEvalOutcome(ServerConfig("host", 1)), Data("ServerConfig(host,1) (hearth.examples.expr_codecs.ServerConfig)")),
          ("Shape.Circle(1.5): Shape", testSemiEvalOutcome(Shape.Circle(1.5): Shape), Data("Circle(1.5) (hearth.examples.expr_codecs.Shape$Circle)")),
          ("Box(1)", testSemiEvalOutcome(Box(1)), Data("Box(1) (hearth.examples.exprsedge.Box)")),
          ("Arith.Add(Arith.Lit(1), Arith.Lit(2))", testSemiEvalOutcome(Arith.Add(Arith.Lit(1), Arith.Lit(2))), Data("Add(Lit(1),Lit(2)) (hearth.examples.exprsedge.Arith$Add)"))
        )
      }

      test("should evaluate BigInt, BigDecimal and Data values") {
        checkAll(
          ("BigInt(42)", testSemiEvalOutcome(BigInt(42)), Data("42 (scala.math.BigInt)")),
          ("BigInt(\"123456789012345678901234567890\")", testSemiEvalOutcome(BigInt("123456789012345678901234567890")), Data("123456789012345678901234567890 (scala.math.BigInt)")),
          ("BigDecimal(\"3.14\")", testSemiEvalOutcome(BigDecimal("3.14")), Data("3.14 (scala.math.BigDecimal)")),
          ("Data(1)", testSemiEvalOutcome(Data(1)), Data("1 (java.lang.Integer)")),
          ("Data(\"text\")", testSemiEvalOutcome(Data("text")), Data("text (java.lang.String)")),
          ("Data.list(Data(1), Data(true))", testSemiEvalOutcome(Data.list(Data(1), Data(true))), Data("List(1, true) (scala.collection.immutable.$colon$colon)"))
        )
      }

      test("should evaluate overloaded methods, methods inherited by Predef and nested combinations") {
        checkAll(
          ("\"hello\".substring(2)", testSemiEvalOutcome("hello".substring(2)), Data("llo (java.lang.String)")),
          ("\"hello\".indexOf(\"l\")", testSemiEvalOutcome("hello".indexOf("l")), Data("2 (java.lang.Integer)")),
          ("\"hello\".indexOf('l'.toInt)", testSemiEvalOutcome("hello".indexOf('l'.toInt)), Data("2 (java.lang.Integer)")),
          ("1.max(2)", testSemiEvalOutcome(1.max(2)), Data("2 (java.lang.Integer)")),
          ("\"abc\".reverse", testSemiEvalOutcome("abc".reverse), Data("cba (java.lang.String)")),
          ("\"ab\" * 2", testSemiEvalOutcome("ab" * 2), Data("abab (java.lang.String)")),
          ("SemiEvalConfig.default.name + \"!\"", testSemiEvalOutcome(SemiEvalConfig.default.name + "!"), Data("default! (java.lang.String)")),
          ("PlainObject.letter.toInt", testSemiEvalOutcome(PlainObject.letter.toInt), Data("97 (java.lang.Integer)")),
          ("Some(List(1, 2, 3).size)", testSemiEvalOutcome(Some(List(1, 2, 3).size)), Data("Some(3) (scala.Some)"))
        )
      }

      test("should evaluate lambdas of different arities and eta-expanded methods into callable functions") {
        checkAll(
          ("() => 42", testSemiEvalApply(() => 42), Data("42 (java.lang.Integer)")),
          ("(x: Int) => x + 1, 41", testSemiEvalApply((x: Int) => x + 1, 41), Data("42 (java.lang.Integer)")),
          ("(a: Int, b: Int) => a * b, 6, 7", testSemiEvalApply((a: Int, b: Int) => a * b, 6, 7), Data("42 (java.lang.Integer)")),
          ("(a: Int, b: Int, c: Int) => a + b * c, 2, 5, 8", testSemiEvalApply((a: Int, b: Int, c: Int) => a + b * c, 2, 5, 8), Data("42 (java.lang.Integer)")),
          ("(s: String, n: Int, suffix: String) => (s * n) + suffix, \"ab\", 2, \"!\"", testSemiEvalApply((s: String, n: Int, suffix: String) => (s * n) + suffix, "ab", 2, "!"), Data("abab! (java.lang.String)")),
          ("(s: String) => s * 2, \"ab\"", testSemiEvalApply((s: String) => s * 2, "ab"), Data("abab (java.lang.String)")),
          ("(s: String, t: String) => s + t, \"ab\", \"!\"", testSemiEvalApply((s: String, t: String) => s + t, "ab", "!"), Data("ab! (java.lang.String)")),
          ("SemiEvalConfig.transform: String => String, \"abc\"", testSemiEvalApply(SemiEvalConfig.transform: String => String, "abc"), Data("ABC (java.lang.String)"))
        )
      }

      test("should fail for local defs and pattern matching, but evaluates local vars and lazy vals") {
        @scala.annotation.nowarn // the var is deliberately never reassigned
        def run(): Unit = checkAll(
          ("var", testSemiEvalOutcome { var x = 1; x }, Data("1 (java.lang.Integer)")),
          ("lazy val", testSemiEvalOutcome { lazy val x = 1; x }, Data("1 (java.lang.Integer)")),
          ("def", testSemiEvalOutcome { def x = 1; x }, Data("<failed>")),
          ("match", testSemiEvalOutcome((PlainObject.answer: Any) match {
            case i: Int => i
            case _      => 0
          }), Data("<failed>"))
        )
        run()
      }

      test("method Expr.semiEval with overrides uses an EvalOverride for the matching type, also for nested values") {
        checkAll(
          ("\"replace\", \"abc\"", testSemiEvalWithOverride("replace", "abc"), Data("OVERRIDDEN (java.lang.String)")),
          ("\"replace\", SemiEvalConfig(1, \"abc\")", testSemiEvalWithOverride("replace", SemiEvalConfig(1, "abc")), Data("SemiEvalConfig(1,OVERRIDDEN) (hearth.typed.SemiEvalConfig)")),
          ("\"replace\", 42", testSemiEvalWithOverride("replace", 42), Data("42 (java.lang.Integer)"))
        )
      }

      test("method Expr.semiEval with overrides falls back to the default evaluation for EvalOverride.none") {
        checkAll(
          ("\"none\", \"abc\"", testSemiEvalWithOverride("none", "abc"), Data("abc (java.lang.String)")),
          ("\"none\", SemiEvalConfig(1, \"abc\")", testSemiEvalWithOverride("none", SemiEvalConfig(1, "abc")), Data("SemiEvalConfig(1,abc) (hearth.typed.SemiEvalConfig)"))
        )
      }

      test("method Expr.semiEval with overrides reports the error of a failing EvalOverride") {
        checkAll(
          ("\"fail\", \"abc\"", testSemiEvalWithOverride("fail", "abc"), Data.list(Data("strings are forbidden"))),
          ("\"fail\", SemiEvalConfig(1, \"abc\")", testSemiEvalWithOverride("fail", SemiEvalConfig(1, "abc")), Data.list(Data("strings are forbidden")))
        )
      }
    }

    group("method Expr.semiQuote") {
      import ExprsEdgeCasesFixtures.{testSemiQuoteBuiltInsAndStructures, testSemiQuoteWithFailingOverride}

      test("should quote all built-in types, collections and nested/generic/recursive structures") {
        testSemiQuoteBuiltInsAndStructures <==> Data.map(
          "Long" -> Data(RuntimeShow.show(Long.MinValue)),
          "Byte" -> Data(RuntimeShow.show(Byte.MinValue)),
          "Short" -> Data(RuntimeShow.show(Short.MinValue)),
          "Float" -> Data(RuntimeShow.show(1.5f)),
          "Char" -> Data(RuntimeShow.show('\n')),
          "Unit" -> Data(RuntimeShow.show(())),
          "String with escapes" -> Data("q\"n\nt\tuéb\\"),
          "BigInt" -> Data("123456789012345678901234567890"),
          "BigDecimal" -> Data("-3.14159265358979323846"),
          "Data" -> Data(RuntimeShow.show(Data.map("list" -> Data.list(Data(1), Data("a")), "flag" -> Data(true)))),
          "empty List" -> Data("List()"),
          "None" -> Data("None"),
          "Set" -> Data("Set(1, 2)"),
          "Map" -> Data("Map(a -> 1)"),
          "Left" -> Data("Left(error)"),
          "generic case class" -> Data("Box(1)"),
          "case class with collection fields" -> Data("WithCollections(List(a, b),Some(1))"),
          "case class with sealed field" -> Data("WithSealedField(label,Origin)"),
          "recursive sealed hierarchy" -> Data("Add(Lit(1),Add(Lit(2),Lit(3)))")
        )
      }

      test("should return Left when a QuoteOverride fails, also for a nested value") {
        testSemiQuoteWithFailingOverride <==> Data.map(
          "direct" -> Data("<failed: strings are forbidden>"),
          "nested" -> Data("<failed: strings are forbidden>"),
          "not overridden" -> Data("42")
        )
      }
    }

    group("type ExprCodec") {
      import ExprsEdgeCasesFixtures.{testBuiltInCodecEdgeCases, testDataCodecRoundtrips, testExprCodecDerivedAtRuntime}

      test("built-in codecs round-trip extreme values, escapes, empty and nested collections") {
        testBuiltInCodecEdgeCases <==> Data.map(
          "Int.MinValue" -> bothSides(Int.MinValue),
          "Long.MinValue" -> bothSides(Long.MinValue),
          "Byte.MinValue" -> bothSides(Byte.MinValue),
          "Short.MinValue" -> bothSides(Short.MinValue),
          "negative Float" -> bothSides(-1.5f),
          "negative Double" -> bothSides(-1.5),
          "Double.NaN" -> bothSides(Double.NaN),
          "Char newline" -> bothSides('\n'),
          "Char quote" -> bothSides('\''),
          "String with escapes" -> bothSides("q\"n\nt\tuéb\\"),
          "empty String" -> bothSides(""),
          "Class[String]" -> bothSides(classOf[String]),
          "ClassTag[String]" -> bothSides(scala.reflect.classTag[String]),
          "huge BigInt" -> bothSides(BigInt("-123456789012345678901234567890")),
          "precise BigDecimal" -> bothSides(BigDecimal("3.14159265358979323846")),
          "empty Array" -> bothSides(Array.empty[Int]),
          "Array of 3" -> bothSides(Array(1, 2, 3)),
          "Seq of 3" -> bothSides(Seq(1, 2, 3)),
          "empty Seq" -> bothSides(Seq.empty[Int]),
          "empty List" -> bothSides(List.empty[Int]),
          "empty Vector" -> bothSides(Vector.empty[Int]),
          "empty Set" -> bothSides(Set.empty[Int]),
          "empty Map" -> bothSides(Map.empty[Int, Int]),
          "None as Option" -> bothSides(None),
          "Right as Either" -> bothSides(Right(2)),
          "List of Options" -> bothSides(List(Some(1), None)),
          "Map of Lists" -> bothSides(Map("a" -> List(1, 2))),
          "Either of List" -> bothSides(Right(List(1))),
          "Option of Either" -> bothSides(Some(Left("x")))
        )
      }

      test("Data codec round-trips every kind of Data") {
        testDataCodecRoundtrips <==> Data.map(
          "null" -> bothSides(Data()),
          "Int" -> bothSides(Data(1)),
          "Long" -> bothSides(Data(2L)),
          "Float" -> bothSides(Data(1.5f)),
          "Double" -> bothSides(Data(2.5)),
          "Boolean" -> bothSides(Data(true)),
          "String" -> bothSides(Data("text")),
          "list" -> bothSides(Data.list(Data(1), Data("a"))),
          "map" -> bothSides(Data.map("k" -> Data(1), "nested" -> Data.map("x" -> Data(false)))),
          "map with 6 keys" -> bothSides(
            Data.map("f" -> Data(1), "e" -> Data(2), "d" -> Data(3), "c" -> Data(4), "b" -> Data(5), "a" -> Data(6))
          )
        )
      }

      test("ExprCodec.derived round-trips generic, nested, recursive and Enumeration types") {
        testExprCodecDerivedAtRuntime(Box(1)) <==> bothSidesDerived(Box(1))
        testExprCodecDerivedAtRuntime(Box("text")) <==> bothSidesDerived(Box("text"))
        testExprCodecDerivedAtRuntime(WithCollections(List("a", "b"), Some(1))) <==>
          bothSidesDerived(WithCollections(List("a", "b"), Some(1)))
        testExprCodecDerivedAtRuntime(WithSealedField("label", Shape.Circle(1.5))) <==>
          bothSidesDerived(WithSealedField("label", Shape.Circle(1.5)))
        testExprCodecDerivedAtRuntime[Arith](Arith.Add(Arith.Lit(1), Arith.Lit(2))) <==>
          bothSidesDerived(Arith.Add(Arith.Lit(1), Arith.Lit(2)))
      }

      // ExprCodec.derived's toExpr is Expr.semiQuote, which (unlike the derived codec) reports a failure as a value
      // instead of aborting the compilation.
      test("Expr.semiQuote re-lifts the same value of a Scala Enumeration") {
        import ExprsEdgeCasesFixtures.testSemiQuoteRoundTripAtRuntime

        testSemiQuoteRoundTripAtRuntime[WeekDay.Value](WeekDay.Tue) <==> bothSidesDerived(WeekDay.Tue)
      }

      test("ExprCodec.derived fromExpr returns None for a value only known at runtime") {
        val runtimeConfig = List(ServerConfig("host", 1)).head
        val _ = runtimeConfig
        testExprCodecDerivedAtRuntime(runtimeConfig) <==> Data.map("decoded" -> Data("<none>"))
      }
    }

    group("type VarArgs") {

      test("methods VarArgs.{toIterable, toSeq, toList, toVector, to} give one Expr per written-out argument") {
        import ExprsEdgeCasesFixtures.testVarArgsConversions

        testVarArgsConversions(1, 2, 3) <==> Data.map(
          "toIterable" -> Data(3),
          "toSeq" -> Data(3),
          "toList" -> Data(3),
          "toVector" -> Data(3),
          "to(List)" -> Data(3),
          "toIterableOption" -> Data("Some(3)")
        )
        testVarArgsConversions[Int]() <==> Data.map(
          "toIterable" -> Data(0),
          "toSeq" -> Data(0),
          "toList" -> Data(0),
          "toVector" -> Data(0),
          "to(List)" -> Data(0),
          "toIterableOption" -> Data("Some(0)")
        )
      }

      test("methods VarArgs.{apply, from} create an empty Seq from no expressions") {
        import ExprsFixtures.testVarArgs

        testVarArgs[Int]() <==> Data.map(
          "VarArgs.apply" -> Data.list(),
          "VarArgs.from" -> Data.list()
        )
      }
    }

    group("type FreshName") {

      test("FreshName strategies name the definition after the type, the expression or the given prefix") {
        import ExprsEdgeCasesFixtures.testFreshNames

        val sth = List("abc").head
        testFreshNames(sth.toString) <==> Data.map(
          "FromType" -> Data("string:abc"),
          "FromExpr" -> Data("sthtostring:abc"),
          "FromPrefix" -> Data("custom:abc"),
          "String" -> Data("implicitly:abc")
        )
      }
    }

    group("type MatchCase") {
      import ExprsEdgeCasesFixtures.testMatchCaseEqValueAgainst

      test("method MatchCase.eqValue matches equal literals and falls through for different ones") {
        val fortyTwo = List(42).head
        val text = List("a").head
        testMatchCaseEqValueAgainst(fortyTwo, 42) <==> "matched!"
        testMatchCaseEqValueAgainst(fortyTwo, 99) <==> "other: 42"
        testMatchCaseEqValueAgainst(text, "a") <==> "matched!"
        testMatchCaseEqValueAgainst(text, "b") <==> "other: a"
      }

      test("method MatchCase.eqValue matches case objects and Enumeration values by identity") {
        val origin: Shape = List(Shape.Origin).head
        val circle: Shape = List(Shape.Circle(1.5)).head
        val mon: WeekDay.Value = List(WeekDay.Mon).head
        testMatchCaseEqValueAgainst[Shape](origin, Shape.Origin) <==> "matched!"
        testMatchCaseEqValueAgainst[Shape](circle, Shape.Origin) <==> "other: Circle(1.5)"
        testMatchCaseEqValueAgainst[WeekDay.Value](mon, WeekDay.Mon) <==> "matched!"
        testMatchCaseEqValueAgainst[WeekDay.Value](mon, WeekDay.Tue) <==> "other: Mon"
      }

      test("method MatchCase.eqValue matches stable references and general expressions by equality") {
        val config = List(SemiEvalConfig(0, "default")).head
        val other = List(SemiEvalConfig(1, "other")).head
        val list = List(List(1, 2)).head
        testMatchCaseEqValueAgainst(config, SemiEvalConfig.default) <==> "matched!"
        testMatchCaseEqValueAgainst(other, SemiEvalConfig.default) <==> "other: SemiEvalConfig(1,other)"
        testMatchCaseEqValueAgainst(list, List(1, 2)) <==> "matched!"
        testMatchCaseEqValueAgainst(list, List(3)) <==> "other: List(1, 2)"
      }

      test("DirectStyle of MatchCase, ValDefBuilder and LambdaBuilder throws when runSafe is never called") {
        import ExprsEdgeCasesFixtures.testDirectStyleWithoutRunSafe

        testDirectStyleWithoutRunSafe <==> Data.map(
          "MatchCase" -> Data("<AssertionError>"),
          "ValDefBuilder" -> Data("<AssertionError>"),
          "LambdaBuilder" -> Data("<AssertionError>"),
          "ValDefs" -> Data(1)
        )
      }
    }

    group("type ValDefs") {
      import ExprsEdgeCasesFixtures.{testValDefsEvaluationCount, testValDefsSamePrefixDoNotClash}

      def evaluate(kind: String, useIt: Boolean): Data = {
        val counter = new EvalCounter
        val result = kind match {
          case "val"  => if (useIt) testValDefsEvaluationCount("val", counter.tick(), true) else testValDefsEvaluationCount("val", counter.tick(), false)
          case "lazy" => if (useIt) testValDefsEvaluationCount("lazy", counter.tick(), true) else testValDefsEvaluationCount("lazy", counter.tick(), false)
          case _      => if (useIt) testValDefsEvaluationCount("def", counter.tick(), true) else testValDefsEvaluationCount("def", counter.tick(), false)
        }
        Data.map("result" -> Data(result), "evaluations" -> Data(counter.count))
      }

      test("val is evaluated exactly once, even when unused") {
        evaluate("val", useIt = true) <==> Data.map("result" -> Data(1 + 1), "evaluations" -> Data(1))
        evaluate("val", useIt = false) <==> Data.map("result" -> Data(0), "evaluations" -> Data(1))
      }

      test("lazy val is evaluated at most once, and only when used") {
        evaluate("lazy", useIt = true) <==> Data.map("result" -> Data(1 + 1), "evaluations" -> Data(1))
        evaluate("lazy", useIt = false) <==> Data.map("result" -> Data(0), "evaluations" -> Data(0))
      }

      test("def is evaluated on every use") {
        evaluate("def", useIt = true) <==> Data.map("result" -> Data(1 + 2), "evaluations" -> Data(2))
        evaluate("def", useIt = false) <==> Data.map("result" -> Data(0), "evaluations" -> Data(0))
      }

      test("definitions created with the same name prefix do not clash") {
        testValDefsSamePrefixDoNotClash ==> 123
      }
    }

    group("type ValDefBuilder") {
      import ExprsEdgeCasesFixtures.{testValDefBuilderMixedTypes, testValDefBuilderRecursiveDef}

      test("a def built with ValDefBuilder can call itself recursively") {
        testValDefBuilderRecursiveDef(5) ==> 120
      }

      test("a def built with ValDefBuilder can take parameters of different types than it returns") {
        testValDefBuilderMixedTypes ==> "ABAB"
      }
    }

    group("type ValDefsCache") {
      import ExprsEdgeCasesFixtures.{testValDefsCacheMLocal, testValDefsCacheMutualRecursion}

      test("forward-declared defs can call each other, and lookups require a matching key and signature") {
        testValDefsCacheMutualRecursion(10) <==> Data.map(
          "isEven(n)" -> Data(true),
          "isOdd(n)" -> Data(false),
          "flags" -> Data.map(
            "visible before forwardDeclare" -> Data(false),
            "visible after forwardDeclare" -> Data(true),
            "isBuilt after forwardDeclare" -> Data(false),
            "isBuilt after buildCachedWith" -> Data(true),
            "missing key" -> Data(false),
            "wrong parameter type" -> Data(false),
            "wrong return type" -> Data(false),
            "wrong arity" -> Data(false)
          )
        )
      }

      test("ValDefsCache.mlocal supports forwardDeclare, buildCachedWith and get, and refuses building a key twice") {
        testValDefsCacheMLocal <==> Data.map(
          "visible after forwardDeclare" -> Data(true),
          "result" -> Data(20),
          "building the same key twice" -> Data("HearthRequirementError")
        )
      }
    }

    group("type LambdaBuilder") {

      test("a lambda built with LambdaBuilder can take parameters of different types than it returns") {
        import ExprsEdgeCasesFixtures.testLambdaBuilderMixedTypes

        testLambdaBuilderMixedTypes ==> "ababab"
      }
    }
  }
}
