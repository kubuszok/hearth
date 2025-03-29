package hearth

abstract class MacroCommonsScala3(using val quotes: scala.quoted.Quotes)
    extends MacroCommons
    with TypesScala3
    with ExprsScala3
    with FunctionsScala3
    with ClassesScala3 {

  import quotes.*, quotes.reflect.*

  object Environment extends EnvironmentModule {

    override val XMacroSettings: List[String] = {
      // workaround to contain @experimental from polluting the whole codebase
      val info = quotes.reflect.CompilationInfo
      info.getClass.getMethod("XmacroSettings").invoke(info).asInstanceOf[List[String]]
    }

    override def reportInfo(msg: String): Unit = report.info(msg, Position.ofMacroExpansion)
    override def reportWarn(msg: String): Unit = report.info(msg, Position.ofMacroExpansion)
    override def reportErrorAndAbort(msg: String): Nothing = report.errorAndAbort(msg, Position.ofMacroExpansion)
  }
}
