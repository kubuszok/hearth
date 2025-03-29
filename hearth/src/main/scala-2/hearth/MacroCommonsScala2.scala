package hearth

trait MacroCommonsScala2
    extends MacroCommons
    with TypesScala2
    with ExprsScala2
    with FunctionsScala2
    with ClassesScala2 {

  val c: scala.reflect.macros.blackbox.Context

  object Environment extends EnvironmentModule {

    override val XMacroSettings: List[String] = c.settings

    override def reportInfo(msg: String): Unit = c.echo(c.enclosingPosition, msg)
    override def reportWarn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    override def reportErrorAndAbort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
  }
}
