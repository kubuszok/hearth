package hearth
package source

object MethodName extends MethodNameCompanion {
  type MethodName <: String
  def wrap(name: String): MethodName = name.asInstanceOf[MethodName]
}

object Line extends LineCompanion {
  type Line <: Int
  def wrap(line: Int): Line = line.asInstanceOf[Line]
}

object File extends FileCompanion {
  type File <: String
  def wrap(file: String): File = file.asInstanceOf[File]
}

object FileName extends FileNameCompanion {
  type FileName <: String
  def wrap(fileName: String): FileName = fileName.asInstanceOf[FileName]
}

/** A value paired with the source text of the expression that produced it.
  *
  * Useful for assert-style macros (expecty/munit/scalatest) that want to show the source of an asserted expression in a
  * failure message, e.g. `Text(a + b)` captures both the runtime value `a + b` and the string `"a + b"`.
  *
  * Unlike the zero-arg source materializers ([[Line]], [[File]], …), the materializer is an implicit conversion that
  * captures the argument expression's source text at the macro call site.
  *
  * @since 0.4.0
  */
final case class Text[T](value: T, source: String) {

  override def toString: String = s"$source = $value"
}
object Text extends TextCompanion

/** The current location in the source code.
  *
  * @since 0.1.0
  */
final case class Location(file: File, line: Line) {

  def fileName: FileName = FileName.wrap(file.split("/").last)

  override def toString: String = s"$file:$line"
}
object Location {

  implicit def derived(implicit file: File, line: Line): Location = Location(file, line)
}
