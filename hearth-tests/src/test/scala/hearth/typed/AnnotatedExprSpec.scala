package hearth
package typed

/** Cross-platform tests for `Expr.annotated` (hearth#334) — Scala and Java annotations, with and without arguments. */
final class AnnotatedExprSpec extends MacroSuite {

  group("Expr.annotated") {

    test("@nowarn (parameterless) round-trips at runtime") {
      AnnotatedExprFixtures.testNowarn(42) ==> 42
      AnnotatedExprFixtures.testNowarn("hello") ==> "hello"
    }

    test("@nowarn(\"msg\") round-trips at runtime") {
      AnnotatedExprFixtures.testNowarnMsg(42) ==> 42
    }

    test("@SuppressWarnings(Array(...)) round-trips at runtime (a Java annotation)") {
      AnnotatedExprFixtures.testSuppressWarnings(42) ==> 42
      AnnotatedExprFixtures.testSuppressWarnings(List(1, 2, 3)) ==> List(1, 2, 3)
    }

    test("the annotation is attached to the generated code") {
      val rendered = AnnotatedExprFixtures.testNowarnMsgRendered(42)
      assert(rendered.contains("nowarn"), s"expected the rendered code to mention `nowarn`, but was:\n$rendered")
    }
  }
}
