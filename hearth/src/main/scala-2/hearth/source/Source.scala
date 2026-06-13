package hearth
package source

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox

final private[source] class Source(val c: blackbox.Context) extends MacroCommonsScala2 with SourceMacros {

  def methodNameImpl: c.Expr[MethodName] = methodName
  def lineImpl: c.Expr[Line] = line
  def fileImpl: c.Expr[File] = file
  def fileNameImpl: c.Expr[FileName] = fileName
  def textImpl[T: c.WeakTypeTag](value: c.Expr[T]): c.Expr[Text[T]] = text[T](value)
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

private[source] trait TextCompanion {

  /** Materializes a [[Text]] by capturing the source text of `value` at the call site.
    *
    * @since 0.4.0
    */
  implicit def generate[T](value: T): Text[T] = macro Source.textImpl[T]
}
