package hearth
package source

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
