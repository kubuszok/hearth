package hearth

trait EnvironmentsScala2 extends Environments { this: MacroCommonsScala2 =>

  override type Position = c.universe.Position

  object Position extends PositionModule {
    override def current: Position = c.enclosingPosition

    override def file(pos: Position): Option[java.nio.file.Path] = scala.util.Try(new java.io.File(pos.source.path).toPath).toOption
    override def offset(pos: Position): Int = pos.start
    override def line(pos: Position): Int = pos.line
    override def column(pos: Position): Int = pos.column
  }

  object Environment extends EnvironmentModule {

    override val XMacroSettings: List[String] = c.settings

    override def reportInfo(msg: String): Unit = c.echo(c.enclosingPosition, msg)
    override def reportWarn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    override def reportErrorAndAbort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
  }
}
