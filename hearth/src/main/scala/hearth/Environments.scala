package hearth

trait Environments extends EnvironmentCrossQuotesSupport {

  /** Platform-specific position representation (`c.universe.Position` in 2, `quotes.reflect.Position` in 3). */
  type Position

  val Position: PositionModule
  trait PositionModule { this: Position.type =>

    def current: Position

    def file(pos: Position): Option[java.nio.file.Path]
    def offset(pos: Position): Int
    def line(pos: Position): Int
    def column(pos: Position): Int

    final def fileName(pos: Position): Option[String] = pos.file.map(_.getFileName().toString)
    final def prettyPrint(pos: Position): String =
      fileName(pos).map(f => s"$f:${pos.line}:${pos.column}").getOrElse(s"<unknown>:${pos.line}:${pos.column}")
  }
  implicit class PositionMethods(private val position: Position) {

    def file: Option[java.nio.file.Path] = Position.file(position)
    def offset: Int = Position.offset(position)
    def line: Int = Position.line(position)
    def column: Int = Position.column(position)

    def fileName: Option[String] = Position.fileName(position)
    def prettyPrint: String = Position.prettyPrint(position)
  }

  implicit lazy val PositionOrdering: Ordering[Position] =
    Ordering[String].on[Position](_.file.toString).orElseBy(_.offset)

  val Environment: EnvironmentModule
  trait EnvironmentModule { this: Environment.type =>

    lazy val currentPosition: Position = Position.current

    def currentScalaVersion: ScalaVersion
    lazy val currentLanguageVersion: LanguageVersion = currentScalaVersion.toLanguageVersion
    lazy val isScala2_13: Boolean = currentLanguageVersion == LanguageVersion.Scala2_13
    lazy val isScala3: Boolean = currentLanguageVersion == LanguageVersion.Scala3

    def XMacroSettings: List[String]

    def reportInfo(msg: String): Unit
    def reportWarn(msg: String): Unit
    def reportErrorAndAbort(msg: String): Nothing

    /** Pass position like "File.scala:12" or "File.scala:12:34", and it will check if current expansion matches it.
      *
      * Useful for debugging macros, when we don't want to print details for every single test case but just one.
      *
      * @param compilationLogPosition
      *   position as seen in the compilation log
      */
    def isExpandedAt(compilationLogPosition: String): Boolean = compilationLogPosition match {
      case fileLineColumnRegex(fileName, line, column) =>
        val fileMatches = currentPosition.file.exists(_.toString.endsWith(fileName))
        val actualLine = currentPosition.line
        val lineMatches = scala.util.Try(line.toInt).toOption.contains(actualLine)
        val columnMatches = scala.util.Try(column.toInt).toOption.contains(currentPosition.column)
        fileMatches && lineMatches && columnMatches
      case fileLineRegex(fileName, line) =>
        val fileMatches = currentPosition.file.exists(_.toString.endsWith(fileName))
        val actualLine = currentPosition.line
        val lineMatches = scala.util.Try(line.toInt).toOption.contains(actualLine)
        fileMatches && lineMatches
      case _ => reportErrorAndAbort(s"Invalid position: $compilationLogPosition")
    }
    private val fileLineRegex = """^(.+):(\d+)$""".r
    private val fileLineColumnRegex = """^(.+):(\d+):(\d+)$""".r
  }

  val CrossQuotes: CrossQuotesModule
  trait CrossQuotesModule extends CrossQuotesSupport { this: CrossQuotes.type =>

    /** `scala.reflect.macros.blackbox.Context` on Scala 2, `scala.quoted.Quotes` on Scala 3. */
    def ctx[CastAs]: CastAs
  }
}
