package hearth
package debug

private[debug] trait DebugMacros { this: MacroCommons =>

  def withFinalCodeInIDE[A](expr: Expr[A]): Expr[A] = {
    val preview = expr.prettyPrint
    Environment.reportInfo(s"Final version of the code:\n$preview")
    expr
  }

  def withFinalASTInIDE[A](expr: Expr[A]): Expr[A] = {
    val preview = expr.prettyAST
    Environment.reportInfo(s"Final version of the AST:\n$preview")
    expr
  }

  def withGivenCodeInIDE[A: Type]: Expr[A] = Expr.summonImplicit[A] match {
    case Some(expr) => withFinalCodeInIDE(expr)
    case None       => Environment.reportErrorAndAbort(s"No implicit value of type ${Type.prettyPrint[A]} found")
  }

  def withGivenASTInIDE[A: Type]: Expr[A] = Expr.summonImplicit[A] match {
    case Some(expr) => withFinalASTInIDE(expr)
    case None       => Environment.reportErrorAndAbort(s"No implicit value of type ${Type.prettyPrint[A]} found")
  }
}
