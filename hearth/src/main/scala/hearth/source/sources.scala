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
  type File <: java.nio.file.Path
  def wrap(file: java.nio.file.Path): File = file.asInstanceOf[File]
}

object FileName extends FileNameCompanion {
  type FileName <: String
  def wrap(fileName: String): FileName = fileName.asInstanceOf[FileName]
}

/** The current location in the source code.
  *
  * @since 0.1.0
  */
final case class Location(file: File, line: Line) {

  def fileName: FileName = FileName.wrap(file.getFileName.toString)

  override def toString: String = s"$file:$line"
}
object Location {

  implicit def derived(implicit file: File, line: Line): Location = Location(file, line)
}
