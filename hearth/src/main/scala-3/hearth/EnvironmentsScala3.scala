package hearth

trait EnvironmentsScala3 extends Environments { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  override type Position = quotes.reflect.Position

  object Position extends PositionModule {

    override def current: Position = quotes.reflect.Position.ofMacroExpansion

    override def file(pos: Position): Option[java.nio.file.Path] = pos.sourceFile.getJPath
    override def offset(pos: Position): Int = pos.start
    override def line(pos: Position): Int = pos.startLine + 1 // for some reason, the line number is 0-based in Scala 3
    override def column(pos: Position): Int = pos.startColumn + 1 // same for the column number
  }

  object Environment extends EnvironmentModule {

    override lazy val currentScalaVersion: ScalaVersion = ScalaVersion.byScalaLibrary(quotes)

    override lazy val XMacroSettings: List[String] = {
      // workaround to contain @experimental from polluting the whole codebase
      val info = quotes.reflect.CompilationInfo
      info.getClass.getMethod("XmacroSettings").invoke(info).asInstanceOf[List[String]]
    }

    override def reportInfo(msg: String): Unit = report.info(msg, currentPosition)
    override def reportWarn(msg: String): Unit = report.info(msg, currentPosition)
    override def reportError(msg: String): Unit = report.error(msg, currentPosition)
    override def reportErrorAndAbort(msg: String): Nothing = report.errorAndAbort(msg, currentPosition)
  }

  object CrossQuotes extends CrossQuotesModule {

    currentCtx = quotes

    override def ctx[CastAs]: CastAs = currentCtx.asInstanceOf[CastAs]
  }
}
