package hearth
package source

import scala.quoted.*

final private[source] class Source(q: Quotes) extends MacroCommonsScala3(using q), SourceMacros

private[source] object Source {

  def methodNameImpl(using q: Quotes): Expr[MethodName] = new Source(q).methodName
  def lineImpl(using q: Quotes): Expr[Line] = new Source(q).line
  def fileImpl(using q: Quotes): Expr[File] = new Source(q).file
  def fileNameImpl(using q: Quotes): Expr[FileName] = new Source(q).fileName
}

private[source] trait MethodNameCompanion {

  inline given derived: MethodName = ${ Source.methodNameImpl }
}

private[source] trait LineCompanion {

  inline given derived: Line = ${ Source.lineImpl }
}

private[source] trait FileCompanion {

  inline given derived: File = ${ Source.fileImpl }
}

private[source] trait FileNameCompanion {

  inline given derived: FileName = ${ Source.fileNameImpl }
}
