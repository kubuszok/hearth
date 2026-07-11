package hearth
package typed

import hearth.data.Data

/** Scala 3-only tests for [[passQuotes]] and [[withQuotes]] utilities.
  *
  * These tests verify that [[LambdaBuilder]] and [[ValDefBuilder]] work correctly when using native Scala 3 quoting
  * syntax (`'{...}` / `${...}`) with explicit [[passQuotes]]/[[withQuotes]] calls, as opposed to the cross-quotes
  * ([[Expr.quote]]/[[Expr.splice]]) approach tested in [[ExprsSpec]].
  *
  * Macro implementation is in [[ExprsScala3Fixtures]].
  */
final class ExprsScala3Spec extends MacroSuite {

  group("regression: enum-case dispatch (Chimney #625)") {

    // Reproduces the Hearth bug behind Chimney #625 (surfacing dotty scala/scala3#20350): `matchOn` on
    // LOWERCASE parameterless enum cases miscompiles - every case collapses to the first (`team`/`school`
    // -> `solo`). FAILS today; will pass once Hearth's enum-val match pattern switches from
    // `Bind(name, Ref(sym))` to the guard form `case x if x == Ref(sym)` (already used by the EqValue fallback).
    test("matchOn dispatches lowercase parameterless enum cases to the correct target") {
      import ExprsFixtures.testEnumDispatch
      import examples.{LowerEnum1, LowerEnum2}
      assertEquals(testEnumDispatch[LowerEnum1, LowerEnum2](LowerEnum1.solo).toString, "solo")
      assertEquals(testEnumDispatch[LowerEnum1, LowerEnum2](LowerEnum1.team).toString, "team")
      assertEquals(testEnumDispatch[LowerEnum1, LowerEnum2](LowerEnum1.school).toString, "school")
    }
  }

  group("trait typed.Exprs (Scala 3, passQuotes/withQuotes)") {

    group("type LambdaBuilder scope issue") {

      test("method LambdaBuilder.of1 should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testLambdaBuilderOf1ScopeIssue

        testLambdaBuilderOf1ScopeIssue ==> Data(2 + 1)
      }

      test("method LambdaBuilder.of2 should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testLambdaBuilderOf2ScopeIssue

        testLambdaBuilderOf2ScopeIssue ==> Data(2 * 3 + 1)
      }

      test("method LambdaBuilder.of3 should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testLambdaBuilderOf3ScopeIssue

        testLambdaBuilderOf3ScopeIssue ==> Data(2 * 3 * 5 + 1)
      }
    }

    group("type ValDefBuilder scope issue") {

      test("method ValDefBuilder.ofVal should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testValDefBuilderOfValScopeIssue

        testValDefBuilderOfValScopeIssue ==> Data(42)
      }

      test("method ValDefBuilder.ofVar should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testValDefBuilderOfVarScopeIssue

        @scala.annotation.nowarn // suppress "local var v$macro$N in value <local ExprsScala3Spec> is never updated" error
        val result = testValDefBuilderOfVarScopeIssue
        result ==> Data(42)
      }

      test("method ValDefBuilder.ofLazy should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testValDefBuilderOfLazyScopeIssue

        testValDefBuilderOfLazyScopeIssue ==> Data(42)
      }

      test("method ValDefBuilder.ofDef0 should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testValDefBuilderOfDef0ScopeIssue

        testValDefBuilderOfDef0ScopeIssue ==> Data(42)
      }

      test("method ValDefBuilder.ofDef1 should handle body Expr from different scope") {
        import ExprsScala3Fixtures.testValDefBuilderOfDef1ScopeIssue

        testValDefBuilderOfDef1ScopeIssue ==> Data(2 + 1)
      }
    }

    group("type ExprCodec") {

      test(
        "methods ExprCodec.{toExpr, fromExpr} should allow converting between expressions and values for IArray (Scala 3 only)"
      ) {
        import ExprsFixtures.testIArrayOneWayCodecs

        testIArrayOneWayCodecs <==> Data.map(
          "IArray[Int]" -> Data.map(
            "encoded" -> Data("scala.IArray$package.IArray.unsafeFromArray[scala.Int](scala.Array.apply(1, ))")
          )
        )
      }
    }

    group("type MatchCase for Scala 3 enums") {

      test("method MatchCase.typeMatch should work with Scala 3 enum") {
        import ExprsFixtures.testMatchCaseTypeMatch

        def expand(example: examples.ExampleEnum): Data = testMatchCaseTypeMatch(example)

        expand(examples.ExampleEnum.ExampleEnumClass(1)).toString.contains("ExampleEnumClass") ==> true
        expand(examples.ExampleEnum.ExampleEnumValue).toString.contains("ExampleEnumValue") ==> true
      }

      test("method MatchCase.eqValueSingleton should work with Scala 3 enum parameterless case") {
        import ExprsFixtures.testMatchCaseEqValueSingleton

        @scala.annotation.nowarn
        def run = testMatchCaseEqValueSingleton[examples.ExampleEnum.ExampleEnumValue.type] <==> Data.map(
          "singletonOf" -> Data("found"),
          "matched" -> Data("matched")
        )
        run
      }

      test("method Expr.singletonOf should work for Scala 3 enum children via directChildren") {
        import ExprsFixtures.testChildrenSingletonOf

        testChildrenSingletonOf[examples.ExampleEnum] <==> Data.map(
          "ExampleEnumClass" -> Data("none"),
          "ExampleEnumValue" -> Data("found")
        )
      }
    }

    group("method Expr.semiEval — asInstanceOf/isInstanceOf/ValueOf") {

      test("isInstanceOf should return true when type matches") {
        ExprsScala3Fixtures.testSemiEvalIsInstanceOfTrue <==> Data.map(
          "status" -> Data("success"),
          "value" -> Data("true"),
          "class" -> Data("java.lang.Boolean")
        )
      }

      test("isInstanceOf should return false when type does not match") {
        ExprsScala3Fixtures.testSemiEvalIsInstanceOfFalse <==> Data.map(
          "status" -> Data("success"),
          "value" -> Data("false"),
          "class" -> Data("java.lang.Boolean")
        )
      }

      test("isInstanceOf should return true for supertype") {
        ExprsScala3Fixtures.testSemiEvalIsInstanceOfSupertype <==> Data.map(
          "status" -> Data("success"),
          "value" -> Data("true"),
          "class" -> Data("java.lang.Boolean")
        )
      }

      test("asInstanceOf should pass through value") {
        ExprsScala3Fixtures.testSemiEvalAsInstanceOf <==> Data.map(
          "status" -> Data("success"),
          "value" -> Data("42"),
          "class" -> Data("java.lang.Integer")
        )
      }

      test("should evaluate ValueOf constructor successfully") {
        val result = ExprsScala3Fixtures.testSemiEvalValueOf
        assert(result.get("status").contains(Data("success")), s"Expected status=success, got: $result")
        assert(result.get("class").contains(Data("scala.ValueOf")), s"Expected class=scala.ValueOf, got: $result")
      }
    }
  }
}
