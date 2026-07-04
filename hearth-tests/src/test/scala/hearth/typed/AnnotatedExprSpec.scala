package hearth
package typed

/** Cross-platform tests for `Expr.annotated` (hearth#334). */
final class AnnotatedExprSpec extends MacroSuite {

  group("Expr.annotated") {

    test("an annotated value round-trips at runtime (the annotated val compiles and passes the value through)") {
      AnnotatedExprFixtures.testAnnotatedRoundtrip(42) ==> 42
      AnnotatedExprFixtures.testAnnotatedRoundtrip("hello") ==> "hello"
      AnnotatedExprFixtures.testAnnotatedRoundtrip(List(1, 2, 3)) ==> List(1, 2, 3)
    }

    test("the annotation is attached to the generated code") {
      val rendered = AnnotatedExprFixtures.testAnnotatedRendered(42)
      assert(rendered.contains("nowarn"), s"expected the rendered code to mention `nowarn`, but was:\n$rendered")
    }
  }
}
