package hearth

import hearth.fp.data.{NonEmptyMap, NonEmptyVector}
import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.{classTag, ClassTag}

trait Environments extends EnvironmentCrossQuotesSupport { env =>

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
    final def prettyPrintLong(pos: Position): String =
      file(pos).map(f => s"$f:${pos.line}:${pos.column}").getOrElse(s"<unknown>:${pos.line}:${pos.column}")
  }
  implicit final class PositionMethods(private val position: Position) {

    def file: Option[java.nio.file.Path] = Position.file(position)
    def offset: Int = Position.offset(position)
    def line: Int = Position.line(position)
    def column: Int = Position.column(position)

    def fileName: Option[String] = Position.fileName(position)
    def prettyPrint: String = Position.prettyPrint(position)
    def prettyPrintLong: String = Position.prettyPrintLong(position)
  }

  implicit final lazy val PositionOrdering: Ordering[Position] =
    Ordering[String].on[Position](_.file.toString).orElseBy(_.offset)

  /** Provides some reporting and information about the current expansion: where is happens, what is the current Scala
    * version, macro settings, etc.
    *
    * @since 0.1.0
    */
  val Environment: EnvironmentModule
  trait EnvironmentModule { this: Environment.type =>

    final lazy val currentPosition: Position = Position.current

    def currentScalaVersion: ScalaVersion

    final lazy val currentLanguageVersion: LanguageVersion = currentScalaVersion.toLanguageVersion
    final lazy val isScala2_13: Boolean = currentLanguageVersion.isScala2_13
    final lazy val isScala3: Boolean = currentLanguageVersion.isScala3

    final lazy val currentJDKVersion: JDKVersion = JDKVersion.runtimeJDKVersion

    final lazy val currentPlatform: Platform = Platform.byHearth
    final lazy val isJvm: Boolean = currentPlatform.isJvm
    final lazy val isJs: Boolean = currentPlatform.isJs
    final lazy val isNative: Boolean = currentPlatform.isNative

    def XMacroSettings: List[String]

    final def typedSettings: Either[String, data.Data] = data.Data.parseList(XMacroSettings)

    def reportInfo(msg: String): Unit
    def reportWarn(msg: String): Unit
    def reportError(msg: String): Unit
    def reportErrorAndAbort(msg: String): Nothing

    /** Pass position like "File.scala:12" or "File.scala:12:34", and it will check if current expansion matches it.
      *
      * Useful for debugging macros, when we don't want to print details for every single test case but just one.
      *
      * @since 0.1.0
      *
      * @param compilationLogPosition
      *   position as seen in the compilation log
      */
    final def isExpandedAt(compilationLogPosition: String): Boolean = compilationLogPosition match {
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

    /** Handle MIO termination on Ctrl+C.
      *
      * If you want to implement [[MioExprOps.runToExprOrFail]] yourself, but you want to reuse the atandard mechanism
      * of handling manual termination of the compilation (e.g. if there might be an infinite loop in the MIO, that you
      * want to allow your users to terminate with Ctrl+C) you can use this method.
      *
      * It would pretty print the last known state of MIO for debugging and consider
      * `-Xmacro-settings:hearth.mioTerminationShouldUseReportError=true|false` option (false by default), which could
      * be used to tell the macro whether it should use:
      *   - [[Environment.reportErrorAndAbort]] to terminate the macro expansion and show the error message using the
      *     reporter, which would terminate only the current macro,
      *   - or just print the error message to `stderr` and throw the exception to terminate the whole compilation
      *     process (on Scala 3, on Scala 3 each looped macro has to be terminated individually).
      *
      * Scala-CLI seems to not propagate the messages on Ctrl+C, no matter if we use Scala 2 or Scala 3, if we use
      * reporting or stderr, or if we use `--server=false` or not.
      *
      * @since 0.1.0
      *
      * @param thunk
      *   the code to execute
      * @return
      *   the result of the code execution or handled MIO termination exception
      */
    final def handleMioTerminationException[A](thunk: => A): A = try
      thunk
    catch {
      case e: fp.effect.MIO.MioTerminationException =>
        val shouldUseReportError = for {
          data <- typedSettings.toOption
          hearth <- data.get("hearth")
          mioTerminationShouldUseReportError <- hearth.get("mioTerminationShouldUseReportError")
          value <- mioTerminationShouldUseReportError.asBoolean
        } yield value

        if (shouldUseReportError.getOrElse(true)) {
          // This will terminate the macro expansion and show the error message using the reporter,
          // but only the current macro will be terminated. (E.g. infinite loop in multiple macros will have to be terminated for each of them).
          reportErrorAndAbort(e.prettyPrintedMessageWithStackTrace)
        } else {
          // This will print only on stderr. It won't be visible if some compilation server is used,
          // but it will terminate the whole compilation process... on Scala 2, on Scala 3 each looped macro has to be terminated individually.
          e.prettyPrintMessageWithStackTrace()
          throw e
        }
    }

    /** Loads all the macro extensions for the given extension type.
      *
      * @since 0.1.0
      *
      * @tparam Extension
      *   the type of the extension to load
      *
      * @return
      *   `AllLoaded(loadedExtensions)` if all the extensions were loaded successfully,
      *   `SomeFailed(loadedExtensions, errors)` otherwise, `LoaderFailed(error)` if the ServiceLoader failed to load
      *   them in thr first place.
      */
    final def loadMacroExtensions[Extension <: MacroExtension[?]: ClassTag]: ExtensionLoadingResult[Extension] = {
      @scala.annotation.nowarn
      val Extension = classTag[Extension].runtimeClass.asInstanceOf[Class[Extension]]

      // Allow aggregating errors from each extension loading
      def safeLoadExtension(ext: Extension): Either[(Extension, Throwable), Extension] = try
        if (ext.isDefinedAt(env)) {
          ext(env)
          Right(ext)
        } else {
          Left(
            ext -> new IllegalStateException(
              s"Instance of ${ext.getClass.getName} cannot be applied to ${env.getClass.getName}"
            )
          )
        }
      catch {
        case e: Throwable => Left(ext -> e)
      }

      platformSpecificServiceLoader.load[Extension](Extension, env.getClass.getClassLoader) match {
        case Right(extensions) =>
          val (failure, success) = extensions.partitionMap(safeLoadExtension)
          val loadedExtensions = ListSet.from(success)
          NonEmptyMap.fromListMap(ListMap.from(failure)) match {
            case Some(failed) => ExtensionLoadingResult.SomeFailed(loadedExtensions, failed)
            case None         => ExtensionLoadingResult.AllLoaded(loadedExtensions)
          }
        case Left(error) => ExtensionLoadingResult.LoaderFailed(error)
      }
    }
  }

  sealed trait ExtensionLoadingResult[Extension] extends Product with Serializable {
    import ExtensionLoadingResult.*

    def loadedExtensions: Loaded[Extension]

    final def fold[B](
        allLoaded: Loaded[Extension] => B
    )(
        someFailed: (Loaded[Extension], Failed[Extension]) => B
    )(
        loaderFailed: Throwable => B
    ): B = this match {
      case AllLoaded(_)          => allLoaded(loadedExtensions)
      case SomeFailed(_, errors) => someFailed(loadedExtensions, errors)
      case LoaderFailed(error)   => loaderFailed(error)
    }
    final def toEither: Either[NonEmptyVector[Throwable], Loaded[Extension]] =
      fold[Either[NonEmptyVector[Throwable], Loaded[Extension]]](loaded => Right(loaded)) { (_, errors) =>
        Left(errors.toNonEmptyVector.map(_._2))
      }(error => Left(NonEmptyVector.one(error)))

    final def toOption: Option[Loaded[Extension]] = toEither.toOption

    final def hasFailures: Boolean = toOption.isEmpty
  }
  object ExtensionLoadingResult {
    type Loaded[Extension] = ListSet[Extension]
    type Failed[Extension] = NonEmptyMap[Extension, Throwable]

    final case class AllLoaded[Extension](
        loadedExtensions: Loaded[Extension]
    ) extends ExtensionLoadingResult[Extension]

    final case class SomeFailed[Extension](
        loadedExtensions: Loaded[Extension],
        errors: Failed[Extension]
    ) extends ExtensionLoadingResult[Extension]

    final case class LoaderFailed[Extension](
        error: Throwable
    ) extends ExtensionLoadingResult[Extension] {

      override def loadedExtensions: Loaded[Extension] = ListSet.empty
    }
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
