package hearth

trait MacroUntypedCommonsScala3
    extends MacroUntypedCommons
    with untyped.TypesScala3
    with untyped.ExprsScala3
    with untyped.MethodsScala3 { this: MacroCommonsScala3 => }

object MacroUntypedCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroUntypedCommonsScala3 =
    new MacroCommonsScala3
}

trait MacroTypedCommonsScala3
    extends MacroTypedCommons
    with typed.TypesScala3
    with typed.ExprsScala3
    with typed.MethodsScala3 { this: MacroCommonsScala3 => }

object MacroTypedCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroTypedCommonsScala3 =
    new MacroCommonsScala3
}

class MacroCommonsScala3(using val quotes: scala.quoted.Quotes)
    extends MacroCommons
    with MacroUntypedCommonsScala3
    with MacroTypedCommonsScala3 {

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
object MacroCommonsScala3 {

  def apply()(using quotes: scala.quoted.Quotes): MacroCommonsScala3 =
    new MacroCommonsScala3
}
