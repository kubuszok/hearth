package hearth

trait EnvironmentsScala2 extends Environments { this: MacroCommonsScala2 =>

  final override type Position = c.universe.Position

  object Position extends PositionModule {
    override def current: Position = c.enclosingPosition

    override def file(pos: Position): Option[java.nio.file.Path] =
      scala.util.Try(new java.io.File(pos.source.path).toPath).toOption
    override def offset(pos: Position): Int = pos.start - macroPositionCorrection(pos)
    override def line(pos: Position): Int = pos.line
    override def column(pos: Position): Int = pos.column - macroPositionCorrection(pos)

    /** So, apparently when we are expanding a macro, the position of the macro expansion is computed differently for
      * macro-methods that have no parameter lists and for those that have:
      *
      * {{{
      * MyType.noParamMacro
      * //     ^ macro expansion is counted from this place
      * MyType.nullaryMacro()
      * //                 ^ macro expansion is counted from this place
      * MyType.paramMacro(arg)
      * //               ^ macro expansion is counted from this place
      * }}}
      *
      * It means that the column/offset has to be adjusted by the macro-method's name length.
      */
    private def macroPositionCorrection(pos: Position) =
      if (pos.source == current.source && pos.start == current.start) macroPossitionCorrectonValue else 0
    private lazy val macroPossitionCorrectonValue: Int = {
      val currentMacro = c.macroApplication.symbol.asMethod
      if (currentMacro.paramLists.isEmpty) 0
      else currentMacro.name.decodedName.toString.length
    }
  }

  object Environment extends EnvironmentModule {

    override lazy val currentScalaVersion: ScalaVersion = ScalaVersion.byScalaLibrary(c)

    override lazy val XMacroSettings: List[String] = c.settings

    override def reportInfo(msg: String): Unit = c.echo(c.enclosingPosition, msg)
    override def reportWarn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
    override def reportError(msg: String): Unit = c.error(c.enclosingPosition, msg)
    override def reportErrorAndAbort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
  }

  /** Do not use this module, it exists only to be used by Scala 2 Cross-Quotes macros and Scala 3 Cross-Quotes compiler
    * plugin.
    *
    * Here live dragons.
    */
  object CrossQuotes extends CrossQuotesModule {

    override def ctx[CastAs]: CastAs = c.asInstanceOf[CastAs]
  }
}
