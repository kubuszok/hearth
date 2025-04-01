package hearth

trait MacroUntypedCommons extends untyped.Types with untyped.Exprs with untyped.Methods {
  this: MacroCommons =>
}

trait MacroTypedCommons
    extends typed.Types
    with typed.Exprs
    with typed.Methods
    with typed.Existentials
    with typed.Classes {
  this: MacroCommons =>
}

trait MacroCommons extends MacroUntypedCommons with MacroTypedCommons {

  // TODO: move the definitions below to some module

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
