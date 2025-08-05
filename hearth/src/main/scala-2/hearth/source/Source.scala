package hearth
package source

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private[source] class Source(val c: blackbox.Context) extends MacroCommonsScala2 with SourceMacros {

  def methodNameImpl: c.Expr[MethodName] = methodName
  def lineImpl: c.Expr[Line] = line
  def fileImpl: c.Expr[File] = file
  def fileNameImpl: c.Expr[FileName] = fileName
}

private[source] trait MethodNameCompanion {

  implicit def derived: MethodName = macro Source.methodNameImpl
}

private[source] trait LineCompanion {

  implicit def derived: Line = macro Source.lineImpl
}

private[source] trait FileCompanion {

  implicit def derived: File = macro Source.fileImpl
}

private[source] trait FileNameCompanion {

  implicit def derived: FileName = macro Source.fileNameImpl
}
