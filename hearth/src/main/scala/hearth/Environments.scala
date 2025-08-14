package hearth

trait Environments extends EnvironmentCrossQuotesSupport {

  /** Platform-specific position representation (`c.universe.Position` in 2, `quotes.reflect.Position` in 3).
    *
    * @since 0.1.0
    */
  type Position

  val Position: PositionModule
  trait PositionModule { this: Position.type =>

    def current: Position

    def file(pos: Position): Option[java.nio.file.Path]
    def offset(pos: Position): Int
    def line(pos: Position): Int
    def column(pos: Position): Int

    final def fileName(pos: Position): Option[String] = pos.file.map(_.getFileName().toString)
    final def prettyPrint(pos: Position): String =
      fileName(pos).map(f => s"$f:${pos.line}:${pos.column}").getOrElse(s"<unknown>:${pos.line}:${pos.column}")
  }
  implicit class PositionMethods(private val position: Position) {

    def file: Option[java.nio.file.Path] = Position.file(position)
    def offset: Int = Position.offset(position)
    def line: Int = Position.line(position)
    def column: Int = Position.column(position)

    def fileName: Option[String] = Position.fileName(position)
    def prettyPrint: String = Position.prettyPrint(position)
  }

  implicit lazy val PositionOrdering: Ordering[Position] =
    Ordering[String].on[Position](_.file.toString).orElseBy(_.offset)

  /** Provides some reporting and information about the current expansion: where is happens, what is the current Scala
    * version, macro settings, etc.
    *
    * @since 0.1.0
    */
  val Environment: EnvironmentModule
  trait EnvironmentModule { this: Environment.type =>

    lazy val currentPosition: Position = Position.current

    def currentScalaVersion: ScalaVersion
    lazy val currentLanguageVersion: LanguageVersion = currentScalaVersion.toLanguageVersion
    lazy val isScala2_13: Boolean = currentLanguageVersion == LanguageVersion.Scala2_13
    lazy val isScala3: Boolean = currentLanguageVersion == LanguageVersion.Scala3

    def XMacroSettings: List[String]

    def typedSettings: Either[String, data.Data] = data.Data.parseList(XMacroSettings)

    def reportInfo(msg: String): Unit
    def reportWarn(msg: String): Unit
    def reportErrorAndAbort(msg: String): Nothing

    /** Pass position like "File.scala:12" or "File.scala:12:34", and it will check if current expansion matches it.
      *
      * Useful for debugging macros, when we don't want to print details for every single test case but just one.
      *
      * @param compilationLogPosition
      *   position as seen in the compilation log
      */
    def isExpandedAt(compilationLogPosition: String): Boolean = compilationLogPosition match {
      case fileLineColumnRegex(fileName, line, column) =>
        val fileMatches = currentPosition.file.exists(_.toString.endsWith(fileName))
        val actualLine = currentPosition.line
        val lineMatches = scala.util.Try(line.toInt).toOption.contains(actualLine)
        val columnMatches = scala.util.Try(column.toInt).toOption.contains(currentPosition.column)
        fileMatches && lineMatches && columnMatches
      case fileLineRegex(fileName, line) =>
        val fileMatches = currentPosition.file.exists(_.toString.endsWith(fileName))
        val actualLine = currentPosition.line
        val lineMatches = scala.util.Try(line.toInt).toOption.contains(actualLine)
        fileMatches && lineMatches
      case _ => reportErrorAndAbort(s"Invalid position: $compilationLogPosition")
    }
    private val fileLineRegex = """^(.+):(\d+)$""".r
    private val fileLineColumnRegex = """^(.+):(\d+):(\d+)$""".r
  }

  /** Module used under the hood by Cross Quotes macros on Scala 2 and Cross Quotes compiler plugin on Scala 3.
    *
    * Cross Quotes push the limits of what could be shared between Scala 2 and Scala 3 macros.
    *
    * It allows rewriting:
    *
    *   1. [[Type.of]], [[Type.Ctor1.of]], etc Scala 2/Scala 3-specific implementations, e.g.
    *
    * {{{
    * Type.of[SomeType]
    * }}}
    *
    * becomes on Scala 2:
    *
    * {{{
    * val ctx: blackbox.Context = CrossQuotes.ctx
    * ctx.weakTypeOf[SomeType]
    * }}}
    *
    * and on Scala 3:
    *
    * {{{
    * given quotes: Quotes = CrossQuotes.ctx
    * scala.quoted.Type.of[SomeType]
    * }}}
    *
    *   2. similarly [[Expr.quote]] and [[Expr.splice]]:
    *
    * {{{
    * Expr.quote {
    *   new SomeType {
    *     def method(a: A): B = Expr.splice { methodImpl(Expr.quote { a }) }
    *   }
    * }
    * }}}
    *
    * becomes on Scala 2:
    *
    * {{{
    * val ctx: blackbox.Context = CrossQuotes.ctx
    * import ctx.universe._
    * ctx.Expr(
    *   q"""
    *   new SomeType {
    *     def method(a: A): B = methodImpl(${ methodImpl(ctx.Expr(q"""a""")) })
    *   }
    *   """
    * )
    * }}}
    *
    * and on Scala 3:
    *
    * {{{
    * given quotes: Quotes = CrossQuotes.ctx
    * '{
    *   new SomeType {
    *     def method(a: A): B = ${ methodImpl('{ a }) }
    *   }
    * }
    * }}}
    *
    * In practice, a bit more work needs to be done to make it work, but that is the basic idea.
    *
    * @since 0.1.0
    */
  val CrossQuotes: CrossQuotesModule
  trait CrossQuotesModule extends CrossQuotesSupport { this: CrossQuotes.type =>

    /** `scala.reflect.macros.blackbox.Context` on Scala 2, `scala.quoted.Quotes` on Scala 3. */
    def ctx[CastAs]: CastAs
  }
}
