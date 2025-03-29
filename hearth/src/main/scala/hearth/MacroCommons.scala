package hearth

trait MacroCommons extends Types with Exprs with Functions with Classes {

  val Environment: EnvironmentModule
  trait EnvironmentModule { this: Environment.type =>

    sealed trait ScalaVersion extends Product with Serializable
    object ScalaVersion {
      case object Scala2_12 extends ScalaVersion
      case object Scala2_13 extends ScalaVersion
      case object Scala3 extends ScalaVersion
    }

    val currentScalaVersion: ScalaVersion

    val XMacroSettings: List[String]

    // TODO: Position

    def reportInfo(msg: String): Unit
    def reportWarn(msg: String): Unit
    def reportErrorAndAbort(msg: String): Nothing
  }
}
