package hearth
package source

private[source] trait SourceMacros { this: MacroCommons =>

  def methodName: Expr[MethodName] = {
    val method = currentMethod
    val name = Expr(method.name)
    Expr.quote(MethodName.wrap(Expr.splice(name)))
  }

  def line: Expr[Line] = {
    val lineNo = Expr(Environment.currentPosition.line)
    Expr.quote(Line.wrap(Expr.splice(lineNo)))
  }

  def file: Expr[File] = {
    val pathString = Expr(currentFile.toString().replace("\\", "/"))
    Expr.quote(File.wrap(Expr.splice(pathString)))
  }

  def fileName: Expr[FileName] = {
    val fileName = Expr(currentFile.getFileName.toString())
    Expr.quote(FileName.wrap(Expr.splice(fileName)))
  }

  private def currentMethod: UntypedMethod = UntypedMethod.enclosing.getOrElse(
    Environment.reportErrorAndAbort(s"No method found for position ${Environment.currentPosition}")
  )

  private def currentFile: java.nio.file.Path = Environment.currentPosition.file.getOrElse(
    Environment.reportErrorAndAbort(s"No file found for position ${Environment.currentPosition}")
  )
}
