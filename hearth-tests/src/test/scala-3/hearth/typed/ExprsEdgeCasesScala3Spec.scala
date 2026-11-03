package hearth
package typed

import hearth.data.Data
import hearth.examples.{Color, ExampleEnum, LowerEnum1}

/** Macro implementation is in [[ExprsEdgeCasesFixturesImpl]] */
final class ExprsEdgeCasesScala3Spec extends MacroSuite {

  group("trait typed.Exprs edge cases (Scala 3)") {
    import ExprsEdgeCasesFixtures.*

    test("VarArgs of a splatted Seq give no expressions at all, which toIterableOption reports as None") {
      val more = Seq(1, 2)
      testVarArgsConversions(more*) <==> Data.map(
        "toIterable" -> Data(0),
        "toSeq" -> Data(0),
        "toList" -> Data(0),
        "toVector" -> Data(0),
        "to(List)" -> Data(0),
        "toIterableOption" -> Data("<None>")
      )
    }

    test("Expr.summonImplicit reports ambiguous implicits as Ambiguous") {
      testSummoningOutcome[hearth.examples.exprsedge.AmbiguousEvidence] <==> Data.map(
        "outcome" -> Data("ambiguous"),
        "isDefined" -> Data(false),
        "isEmpty" -> Data(true),
        "nonEmpty" -> Data(false),
        "toOption" -> Data(false),
        "get" -> Data("NoSuchElementException")
      )
    }

    test("IArray codec round-trips the value") {
      testIArrayRoundtrip <==> Data.map(
        "decoded" -> Data("IArray(1, 2, 3)"),
        "runtime" -> Data("IArray(1, 2, 3)")
      )
    }

    test("Expr.semiEval evaluates Scala 3 enum cases") {
      testSemiEvalOutcome(ExampleEnum.ExampleEnumClass(1)) <==>
        Data("ExampleEnumClass(1) (hearth.examples.ExampleEnum$ExampleEnumClass)")
      testSemiEvalOutcome(Color.Green) <==> Data("Green (hearth.examples.Color$$anon$1)")
    }

    test("Expr.semiEval evaluates lambdas above arity 22 (FunctionXXL)") {
      // format: off
      testSemiEvalApply(
        (a1: Int, a2: Int, a3: Int, a4: Int, a5: Int, a6: Int, a7: Int, a8: Int, a9: Int, a10: Int, a11: Int, a12: Int,
         a13: Int, a14: Int, a15: Int, a16: Int, a17: Int, a18: Int, a19: Int, a20: Int, a21: Int, a22: Int, a23: Int) =>
          a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 +
            a20 + a21 + a22 + a23,
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
      ) <==> Data("276 (java.lang.Integer)")
      // format: on
    }

    test("ExprCodec.derived round-trips Scala 3 enums") {
      testExprCodecDerivedAtRuntime[ExampleEnum](ExampleEnum.ExampleEnumClass(1)) <==> Data.map(
        "decoded" -> Data("ExampleEnumClass(1)"),
        "reLifted" -> Data("ExampleEnumClass(1)")
      )
      testExprCodecDerivedAtRuntime[ExampleEnum](ExampleEnum.ExampleEnumValue) <==> Data.map(
        "decoded" -> Data("ExampleEnumValue"),
        "reLifted" -> Data("ExampleEnumValue")
      )
    }

    // ExprCodec.derived's toExpr is Expr.semiQuote, which (unlike the derived codec) reports a failure as a value
    // instead of aborting the compilation.
    test("Expr.semiQuote re-lifts the same case of an enum with several parameterless cases") {
      testSemiQuoteRoundTripAtRuntime[Color](Color.Green) <==> Data.map(
        "decoded" -> Data("Green"),
        "reLifted" -> Data("Green")
      )
      testSemiQuoteRoundTripAtRuntime[LowerEnum1](LowerEnum1.school) <==> Data.map(
        "decoded" -> Data("school"),
        "reLifted" -> Data("school")
      )
    }

    test("Expr.singletonOf returns parameterless Scala 3 enum cases") {
      testSingletonOfAtRuntime[ExampleEnum.ExampleEnumValue.type] <==> Data("ExampleEnumValue")
      testSingletonOfAtRuntime[ExampleEnum.ExampleEnumClass] <==> Data("<none>")
    }

    test("MatchCase.eqValue matches parameterless Scala 3 enum cases, also with lowercase names") {
      val green: Color = List(Color.Green).head
      val team: LowerEnum1 = List(LowerEnum1.team).head
      testMatchCaseEqValueAgainst[Color](green, Color.Green) <==> "matched!"
      testMatchCaseEqValueAgainst[Color](green, Color.Red) <==> "other: Green"
      testMatchCaseEqValueAgainst[LowerEnum1](team, LowerEnum1.team) <==> "matched!"
      testMatchCaseEqValueAgainst[LowerEnum1](team, LowerEnum1.solo) <==> "other: team"
    }

    test("MatchCase.typeTestMatch narrows with the TypeTest available at the expansion site") {
      val text: Any = List("abc").head
      val number: Any = List(1).head
      testMatchCaseTypeTestMatch[Any, String](text) <==> "narrowed: abc"
      testMatchCaseTypeTestMatch[Any, String](number) <==> "other"
    }
  }
}
