package hearth

trait Environments {

  type Position

  val Position: PositionModule
  trait PositionModule { this: Position.type =>

    def current: Position

    def file(pos: Position): Option[java.nio.file.Path]
    def offset(pos: Position): Int
    def line(pos: Position): Int
    def column(pos: Position): Int
  }
  implicit class PositionMethods(private val position: Position) {
    def file: Option[java.nio.file.Path] = Position.file(position)
    def offset: Int = Position.offset(position)
    def line: Int = Position.line(position)
    def column: Int = Position.column(position)
  }

  implicit lazy val PositionOrdering: Ordering[Position] =
    Ordering[String].on[Position](_.file.toString).orElseBy(_.offset)

  val Environment: EnvironmentModule
  trait EnvironmentModule { this: Environment.type =>

    val currentPosition: Position = Position.current
    val currentScalaVersion: ScalaVersion = ScalaVersion.current

    val XMacroSettings: List[String]

    def reportInfo(msg: String): Unit
    def reportWarn(msg: String): Unit
    def reportErrorAndAbort(msg: String): Nothing
  }
}
