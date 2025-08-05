package hearth

package object source {

  /** The name of the enclosing method.
    *
    * @since 0.1.0
    */
  type MethodName = MethodName.MethodName
  object MethodName extends MethodNameCompanion {
    type MethodName <: String
    def wrap(name: String): MethodName = name.asInstanceOf[MethodName]
  }

  /** The line number of the enclosing method.
    *
    * @since 0.1.0
    */
  type Line = Line.Line
  object Line extends LineCompanion {
    type Line <: Int
    def wrap(line: Int): Line = line.asInstanceOf[Line]
  }

  /** The current file.
    *
    * @since 0.1.0
    */
  type File = File.File
  object File extends FileCompanion {
    type File <: java.nio.file.Path
    def wrap(file: java.nio.file.Path): File = file.asInstanceOf[File]
  }

  /** The name of the current file.
    *
    * @since 0.1.0
    */
  type FileName = FileName.FileName
  object FileName extends FileNameCompanion {
    type FileName <: String
    def wrap(fileName: String): FileName = fileName.asInstanceOf[FileName]
  }
}
