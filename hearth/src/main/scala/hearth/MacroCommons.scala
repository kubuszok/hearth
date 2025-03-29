package hearth

trait MacroCommons extends Types with Exprs with Existentials with Functions with Classes {

  val Environment: EnvironmentModule
  trait EnvironmentModule { this: Environment.type =>

    val currentScalaVersion: ScalaVersion = ScalaVersion.current

    val XMacroSettings: List[String]

    // TODO: Position

    def reportInfo(msg: String): Unit
    def reportWarn(msg: String): Unit
    def reportErrorAndAbort(msg: String): Nothing
  }
}
