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
  }
}
