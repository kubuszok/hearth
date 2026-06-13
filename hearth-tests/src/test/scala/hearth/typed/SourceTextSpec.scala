package hearth
package typed

import hearth.data.Data

final class SourceTextSpec extends MacroSuite {

  group("typed.Expr.sourceCode / Position.sourceCode") {

    group("expression source text") {
      import SourceTextFixtures.testExprSourceText

      test("simple arithmetic expression") {
        testExprSourceText(1 + 2) <==> Data("1 + 2")
      }

      test("literal") {
        testExprSourceText(42) <==> Data("42")
      }

      test("string literal") {
        testExprSourceText("hello") <==> Data("\"hello\"")
      }

      test("method call expression") {
        testExprSourceText("abc".length) <==> Data("\"abc\".length")
      }
    }

    group("synthetic (positionless) expression") {
      import SourceTextFixtures.testSyntheticSourceText

      test("returns <none> for a macro-synthesized expression") {
        testSyntheticSourceText <==> Data("<none>")
      }
    }

    group("destructured subexpression source text (assert-macro use case)") {
      import SourceTextFixtures.testDestructuredSourceText

      test("method call reports whole source and method name") {
        testDestructuredSourceText("abc".substring(1)) <==> Data.map(
          "whole" -> Data("\"abc\".substring(1)"),
          "methodName" -> Data("substring"),
          "argSources" -> Data.list(Data("1"))
        )
      }
    }
  }
}
