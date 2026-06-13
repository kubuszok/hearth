package hearth

trait EnvironmentsScala3 extends Environments { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Position = quotes.reflect.Position

  object Position extends PositionModule {

    override def current: Position = quotes.reflect.Position.ofMacroExpansion

    override def file(pos: Position): Option[java.nio.file.Path] = pos.sourceFile.getJPath
    override def offset(pos: Position): Int = pos.start
    override def line(pos: Position): Int = pos.startLine + 1 // for some reason, the line number is 0-based in Scala 3
    override def column(pos: Position): Int = pos.startColumn + 1 // same for the column number

    // We slice the SourceFile content by [start, end) rather than calling `pos.sourceCode` directly, because
    // `pos.sourceCode` (a quotes.reflect extension) would clash with our own `PositionMethods.sourceCode` implicit
    // class (in scope here) and resolve to it, causing infinite recursion.
    override def sourceCode(pos: Position): Option[String] = scala.util
      .Try {
        pos.sourceFile.content.flatMap { content =>
          val start = pos.start
          val end = pos.end
          if start >= 0 && start <= end && end <= content.length then Some(content.substring(start, end))
          else None
        }
      }
      .toOption
      .flatten
  }

  object Environment extends EnvironmentModule {

    override lazy val currentScalaVersion: ScalaVersion = ScalaVersion.byScalaLibrary(quotes)

    override lazy val XMacroSettings: List[String] = {
      // workaround to contain @experimental from polluting the whole codebase
      val info = quotes.reflect.CompilationInfo
      info.getClass.getMethod("XmacroSettings").invoke(info).asInstanceOf[List[String]]
    }

    override def reportInfo(msg: String): Unit = report.info(msg, currentPosition)
    override def reportInfo(msg: String, position: Position): Unit = report.info(msg, position)
    override def reportWarn(msg: String): Unit = report.warning(msg, currentPosition)
    override def reportWarn(msg: String, position: Position): Unit = report.warning(msg, position)
    override def reportError(msg: String): Unit = report.error(msg, currentPosition)
    override def reportError(msg: String, position: Position): Unit = report.error(msg, position)
    override def reportErrorAndAbort(msg: String): Nothing = report.errorAndAbort(msg, currentPosition)
    override def reportErrorAndAbort(msg: String, position: Position): Nothing = report.errorAndAbort(msg, position)
  }

  object CrossQuotes extends CrossQuotesModule {

    currentCtx = quotes

    override def ctx[CastAs]: CastAs = currentCtx.asInstanceOf[CastAs]
  }
}
