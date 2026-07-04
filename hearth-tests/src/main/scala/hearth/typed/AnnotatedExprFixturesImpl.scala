package hearth
package typed

/** Fixtures for [[AnnotatedExprSpec]] — exercising `Expr.annotated` (hearth#334). */
trait AnnotatedExprFixturesImpl { this: MacroCommons =>

  // Wraps `value` in `@nowarn` and returns it: proves the annotated val compiles and the value passes through.
  def testAnnotatedRoundtrip[A: Type](value: Expr[A]): Expr[A] =
    Expr.annotated(value, Expr.quote(new scala.annotation.nowarn()))

  // Renders the annotated block so the test can assert the annotation is present in the generated code.
  def testAnnotatedRendered[A: Type](value: Expr[A]): Expr[String] =
    Expr(Expr.annotated(value, Expr.quote(new scala.annotation.nowarn("msg"))).prettyPrint)
}
