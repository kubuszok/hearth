package hearth
package treeprinter

trait ShowCodePrettyFixturesImpl { this: MacroCommons =>

  def testExprPrettyPrint[A: Type](expr: Expr[A]): Expr[String] =
    asExprOrError("Failed to pretty print expression")(removeAnsiColors(expr.prettyPrint))

  def testExprPlainPrint[A: Type](expr: Expr[A]): Expr[String] =
    asExprOrError("Failed to plain print expression")(expr.plainPrint)

  def testExprPrettyAST[A: Type](expr: Expr[A]): Expr[String] =
    asExprOrError("Failed to pretty AST expression")(removeAnsiColors(expr.prettyAST))

  def testExprPlainAST[A: Type](expr: Expr[A]): Expr[String] =
    asExprOrError("Failed to plain AST expression")(expr.plainAST)

  def testTypePrettyPrint[A: Type]: Expr[String] =
    asExprOrError("Failed to pretty print type")(removeAnsiColors(Type[A].prettyPrint))

  def testTypePlainPrint[A: Type]: Expr[String] =
    asExprOrError("Failed to plain print type")(Type[A].plainPrint)

  private def asExprOrError(onError: String)(thunk: => String): Expr[String] = try
    Expr(thunk)
  catch {
    case e: Throwable =>
      val msg = e.getMessage()
      val stackTrace = e.getStackTrace
        .map(st => s"  ${st.getClassName}.${st.getMethodName} at ${st.getFileName}:${st.getLineNumber}")
        .mkString("\n")
      Environment.reportErrorAndAbort(s"$onError: $msg:\n$stackTrace")
  }
}
