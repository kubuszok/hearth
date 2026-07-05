package hearth
package typed

/** Fixtures for [[AnnotatedExprSpec]] — exercising `Expr.annotated` (hearth#334) for a Scala annotation (`@nowarn`,
  * with and without a message) and a JAVA annotation (`@SuppressWarnings(Array(...))`).
  */
trait AnnotatedExprFixturesImpl { this: MacroCommons =>

  implicit private def nowarnType: Type[scala.annotation.nowarn] = Type.of[scala.annotation.nowarn]
  implicit private def suppressWarningsType: Type[java.lang.SuppressWarnings] = Type.of[java.lang.SuppressWarnings]

  // @nowarn (parameterless)
  def testNowarn[A: Type](value: Expr[A]): Expr[A] =
    value.annotated[scala.annotation.nowarn]()

  // @nowarn("msg")
  def testNowarnMsg[A: Type](value: Expr[A]): Expr[A] =
    value.annotated[scala.annotation.nowarn](Expr("cat=deprecation").asUntyped)

  // @SuppressWarnings(Array("unused")) — a Java annotation, cannot be `new`-ed in expression position.
  def testSuppressWarnings[A: Type](value: Expr[A]): Expr[A] =
    value.annotated[java.lang.SuppressWarnings](Expr.quote(scala.Array("unused")).asUntyped)

  // Rendered to confirm the annotation is present in the generated code.
  def testNowarnMsgRendered[A: Type](value: Expr[A]): Expr[String] =
    Expr(value.annotated[scala.annotation.nowarn](Expr("cat=deprecation").asUntyped).prettyPrint)
}
