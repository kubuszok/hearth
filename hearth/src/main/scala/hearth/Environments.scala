package hearth

import hearth.fp.data.{NonEmptyMap, NonEmptyVector}
import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.{classTag, ClassTag}

trait Environments extends EnvironmentCrossQuotesSupport { env: MacroCommons =>

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

    /** Returns the original source text spanned by this [[Position]], if available.
      *
      * Useful for assert-style macros (expecty/munit/scalatest) that want to show the source of a (sub)expression in a
      * failure message. Returns [[None]] for synthetic positions that do not point at real source code.
      *
      * @since 0.4.0
      */
    def sourceCode(pos: Position): Option[String]

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

    /** Returns the original source text spanned by this [[Position]], if available.
      *
      * @since 0.4.0
      */
    def sourceCode: Option[String] = Position.sourceCode(position)

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

    /** Extensions that have already been applied (via extend()) in this macro expansion. Uses identity hash to track
      * specific instances — extension objects are cached singletons from ServiceLoader.
      */
    private val appliedExtensions = new java.util.IdentityHashMap[Any, Unit]()

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

    /** The raw `-Xmacro-settings:` strings passed to the compiler.
      *
      * This is the source of every `hearth.mio*` flag that rides on [[MioExprOps.runToExprOrFail]], notably:
      *   - `hearth.mioBenchmarkScopes` — enable per-scope benchmarking / flame graphs (see
      *     [[configureMioBenchmarking]])
      *   - `hearth.mioBenchmarkFlameGraphDir` — directory to write speedscope flame graphs to (see
      *     [[mioBenchmarkFlameGraphDir]])
      *   - `hearth.mioTerminationShouldUseReportError` — how Ctrl+C termination is reported (see
      *     [[handleMioTerminationException]])
      *
      * For parsed, structured access to these use [[typedSettings]] instead of reading the raw strings.
      *
      * @see
      *   [[typedSettings]] for the parsed form
      *
      * @since 0.1.0
      *
      * @return
      *   the raw macro-settings strings
      */
    def XMacroSettings: List[String]

    /** Parses [[XMacroSettings]] into structured [[data.Data]] (or a [[scala.util.Left]] error message).
      *
      * @since 0.1.0
      */
    final def typedSettings: Either[String, data.Data] = data.Data.parseList(XMacroSettings)

    def reportInfo(msg: String): Unit

    /** Reports an info message at the given [[Position]] instead of the macro expansion point, e.g. to point at the
      * specific field/parameter that a derivation step refers to.
      *
      * @since 0.4.0
      */
    def reportInfo(msg: String, position: Position): Unit

    def reportWarn(msg: String): Unit

    /** Reports a warning at the given [[Position]] instead of the macro expansion point, e.g. to point at the specific
      * field/parameter that a derivation step refers to.
      *
      * @since 0.4.0
      */
    def reportWarn(msg: String, position: Position): Unit

    def reportError(msg: String): Unit

    /** Reports an error at the given [[Position]] instead of the macro expansion point, e.g. to point at the specific
      * field/parameter that made a derivation step fail.
      *
      * @since 0.4.0
      */
    def reportError(msg: String, position: Position): Unit

    /** Reports an error at the macro expansion point and aborts the expansion; this is the terminal reporter that
      * [[MioExprOps.runToExprOrFail]] calls to fail a macro.
      *
      * @see
      *   [[MioExprOps.runToExprOrFail]] which calls this to fail an expansion
      *
      * @since 0.1.0
      *
      * @param msg
      *   the error message to report
      */
    def reportErrorAndAbort(msg: String): Nothing

    /** Reports an error at the given [[Position]] instead of the macro expansion point and aborts the expansion, e.g.
      * to point at the specific field/parameter that made a derivation step fail.
      *
      * @since 0.4.0
      */
    def reportErrorAndAbort(msg: String, position: Position): Nothing

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
      case _ if compilationLogPosition.endsWith(".scala") =>
        val fileMatches = currentPosition.file.exists(_.toString.endsWith(compilationLogPosition))
        fileMatches
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
      *   - `Environment.reportErrorAndAbort` to terminate the macro expansion and show the error message using the
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

    /** Run a thunk with an MIO timeout deadline.
      *
      * Sets [[fp.effect.MIO.timeoutDeadlineNanos]], runs the thunk, and restores the previous deadline in a finally
      * block. If [[fp.effect.MIO.MioTimeoutException]] is thrown during execution, it is caught and returned as
      * [[Left]].
      *
      * ==Contract==
      *
      * The MIO timeout is a SINGLE, global, top-level setting — like the error aggregation and the internal state
      * threaded by an MIO run. `withMioTimeout` (and [[MioExprOps.runToExprOrFail]], which calls it) must be entered
      * '''exactly once''', at the top level of a macro expansion. It is '''not''' re-entrant by design, and re-entering
      * it is a contract violation (see the assertion below), not an implementation gap.
      *
      * To compose derivations that themselves produce `MIO` values (e.g. summoning a macro-derived implicit
      * mid-derivation), stay INSIDE the one MIO program: extract values with `flatMap` (or [[fp.DirectStyle]] /
      * [[fp.effect.MIO.scoped]]), use them, and re-wrap the result in `MIO`. Do not open a second timeout / run.
      * [[fp.effect.MIO.unsafe]]'s `runSync` and friends are internal escape hatches that set up no global state and do
      * not thread nested state — use them at your own risk, and never nested.
      *
      * @see
      *   [[MioExprOps.runToExprOrFail]] for the single, top-level entry point that calls this
      * @see
      *   [[fp.effect.MIO.unsafe]] for the internal, own-risk escape hatch that installs no timeout
      *
      * @since 0.3.0
      *
      * @param timeout
      *   maximum wall-clock time the thunk may run before the deadline fires and the run is terminated
      * @param thunk
      *   the top-level MIO run to execute under the timeout deadline
      * @return
      *   the thunk's result as [[Right]], or the [[fp.effect.MIO.MioTimeoutException]] as [[Left]] if the deadline
      *   fired
      */
    final def withMioTimeout[A](
        timeout: scala.concurrent.duration.FiniteDuration
    )(thunk: => A): Either[fp.effect.MIO.MioTimeoutException, A] = {
      if (fp.effect.MIO.timeoutDeadlineNanos != Long.MaxValue)
        throw HearthAssertionError(
          "MIO timeout is already set — you are re-entering `runToExprOrFail`/`withMioTimeout` while one is already " +
            "running. This is a MISUSE of the API, not a bug: the MIO timeout (like error aggregation and the run's " +
            "internal state) is a single, top-level setting and is deliberately NOT re-entrant. Call " +
            "`runToExprOrFail` exactly once, at the top level of the expansion. To compose a derivation that yields an " +
            "MIO (e.g. a summoned macro-derived implicit), extract it with `flatMap`/`DirectStyle` and re-wrap the " +
            "result in MIO instead of starting a nested run. `MIO.unsafe.runSync` sets up no global state and does not " +
            "thread nested state — it is internal and must not be nested either.",
          hearthVersion = HearthVersion.byHearthLibrary,
          scalaVersion = currentScalaVersion,
          platform = currentPlatform,
          jdkVersion = currentJDKVersion
        )
      fp.effect.MIO.timeoutDeadlineNanos = System.nanoTime() + timeout.toNanos
      try Right(thunk)
      catch {
        case e: fp.effect.MIO.MioTimeoutException => Left(e)
      } finally
        fp.effect.MIO.timeoutDeadlineNanos = Long.MaxValue
    }

    /** Runs `thunk` with MIO logging turned into no-ops: while active, `Log.info`/`warn`/`error` and `Log.namedScope`
      * record nothing, avoiding the per-entry and per-scope allocations.
      *
      * UNSAFE and opt-in: any log rendering during `thunk` will be empty, so only use it when the logs are guaranteed
      * to be discarded. [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] enables it automatically when every
      * rendering knob is `DontRender` (and neither error-failing nor benchmarking is on). The flag is saved and
      * restored, so nesting is safe.
      *
      * @since 0.4.1
      */
    final def withMioLoggingDisabled[A](thunk: => A): A = {
      val previous = fp.effect.MIO.disableLogging
      fp.effect.MIO.disableLogging = true
      try thunk
      finally fp.effect.MIO.disableLogging = previous
    }

    /** Sets up MIO to use benchmark scopes based on the hearth.mioBenchmarkScopes setting.
      *
      * It would add to MIO logging output the duration for each scope execution if enabled by
      * `-Xmacro-settings:hearth.mioBenchmarkScopes=true|false` option (false by default).
      *
      * It additionally enables generating flame graphs for benchmarked scopes.
      *
      * @since 0.3.0
      */
    final def configureMioBenchmarking(): Unit = {
      val shouldBenchmark = for {
        data <- typedSettings.toOption
        hearth <- data.get("hearth")
        setting <- hearth.get("mioBenchmarkScopes")
        value <- setting.asBoolean
      } yield value

      fp.effect.MIO.benchmarkScopes = shouldBenchmark.getOrElse(false)
      if (fp.effect.MIO.benchmarkScopes) {
        fp.effect.MIO.macroStartTimestamp = fp.effect.Log.Timestamp.now
      }
    }

    /** Returns the flame graph output directory configured via
      * `-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=<path>`.
      *
      * @since 0.3.0
      */
    final def mioBenchmarkFlameGraphDir: Option[String] = for {
      data <- typedSettings.toOption
      hearth <- data.get("hearth")
      setting <- hearth.get("mioBenchmarkFlameGraphDir")
      value <- setting.asString
    } yield value

    /** Loads all the macro extensions for the given extension type.
      *
      * @since 0.1.0
      *
      * @tparam Extension
      *   the type of the extension to load
      * @return
      *   `AllLoaded(loadedExtensions)` if all the extensions were loaded successfully,
      *   `SomeFailed(loadedExtensions, errors)` otherwise, `LoaderFailed(error)` if the ServiceLoader failed to load
      *   them in thr first place.
      */
    final def loadMacroExtensions[Extension <: MacroExtension[?]: ClassTag]: ExtensionLoadingResult[Extension] = {
      @scala.annotation.nowarn
      val Extension = classTag[Extension].runtimeClass.asInstanceOf[java.lang.Class[Extension]]
      loadMacroExtensionsFrom(
        platformSpecificServiceLoader.load[Extension](Extension, env.getClass.getClassLoader)
      )
    }

    /** Loads all the macro extensions for the given extension type, excluding the ones with the given names.
      *
      * @since 0.3.0
      *
      * @tparam Extension
      *   the type of the extension to load
      * @param excluded
      *   the names of the extensions to exclude (case-sensitive full qualified class names)
      * @return
      *   `AllLoaded(loadedExtensions)` if all the extensions were loaded successfully,
      *   `SomeFailed(loadedExtensions, errors)` otherwise, `LoaderFailed(error)` if the ServiceLoader failed to load
      *   them in thr first place.
      */
    final def loadMacroExtensionsExcluding[Extension <: MacroExtension[?]: ClassTag](
        excluded: String*
    ): ExtensionLoadingResult[Extension] = {
      @scala.annotation.nowarn
      val Extension = classTag[Extension].runtimeClass.asInstanceOf[java.lang.Class[Extension]]
      loadMacroExtensionsFrom(
        platformSpecificServiceLoader.loadExcluding[Extension](Extension, env.getClass.getClassLoader)(excluded*)
      )
    }

    /** Loads all the macro extensions for the given extension type, filtering by the given condition.
      *
      * @since 0.3.0
      *
      * @tparam Extension
      *   the type of the extension to load
      * @param condition
      *   the condition to filter extensions by
      * @return
      *   `AllLoaded(loadedExtensions)` if all the extensions were loaded successfully,
      *   `SomeFailed(loadedExtensions, errors)` otherwise, `LoaderFailed(error)` if the ServiceLoader failed to load
      *   them in thr first place.
      */
    final def loadMacroExtensionsWhen[Extension <: MacroExtension[?]: ClassTag](
        condition: java.lang.Class[?] => Boolean
    ): ExtensionLoadingResult[Extension] = {
      @scala.annotation.nowarn
      val Extension = classTag[Extension].runtimeClass.asInstanceOf[java.lang.Class[Extension]]
      loadMacroExtensionsFrom(
        platformSpecificServiceLoader.loadWhen[Extension](Extension, env.getClass.getClassLoader)(condition)
      )
    }

    final private def loadMacroExtensionsFrom[Extension <: MacroExtension[?]](
        extensions: Either[Throwable, platformSpecificServiceLoader.Loaded[Extension]]
    ): ExtensionLoadingResult[Extension] = {
      // Allow aggregating errors from each extension loading.
      // Extensions that were already applied in this macro expansion are skipped to avoid duplicate side effects
      // (e.g. double provider registration which causes exponential compilation time).
      def safeLoadExtension(ext: Extension): Either[(Extension, Throwable), Extension] =
        if (appliedExtensions.containsKey(ext)) {
          // Using reportInfo instead of reportWarn because warnings are fatal under -Xfatal-warnings.
          reportInfo(
            s"Extension ${ext.getClass.getName} was already loaded in this macro expansion, skipping re-initialization"
          )
          Right(ext)
        } else
          try
            if (ext.isDefinedAt(env)) {
              ext(env)
              appliedExtensions.put(ext, ()): Unit
              Right(ext)
            } else {
              // $COVERAGE-OFF$ I don't have an idea how to test this.
              Left(
                ext -> new IllegalStateException(
                  s"Instance of ${ext.getClass.getName} cannot be applied to ${env.getClass.getName}"
                )
              )
              // $COVERAGE-ON$
            }
          catch {
            case e: Throwable => Left(ext -> e)
          }

      extensions match {
        // No provider could even be instantiated (e.g. every candidate threw, or the only service jar has a
        // forward-incompatible TASTy file): preserve the historical `LoaderFailed` shape so the cause stays visible.
        case Right(loaded) if loaded.services.isEmpty && loaded.failures.nonEmpty =>
          ExtensionLoadingResult.LoaderFailed(loaded.failures.head._2)
        case Right(loaded) =>
          // A subset of providers could not be instantiated (e.g. one unloadable extension jar sitting next to
          // Hearth's own built-in providers). The loader skipped just those rather than aborting the whole run, so the
          // built-ins and the other extensions still load instead of poisoning EVERY derivation in the module. We do
          // NOT `reportInfo`/`reportWarn` the skipped providers here: a macro may emit only one such message, and
          // spending it inside shared loading machinery would steal it from the user's own macro. See issue #325.
          val sorted = loaded.services.sortBy(_.priority)(Ordering[Int].reverse)
          val (failure, success) = sorted.partitionMap(safeLoadExtension)
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

  // Lexical-scope (enclosure) inspection for the current macro expansion.
  //
  // `enclosingScope` returns the chain of enclosures surrounding the expansion point, from the INNERMOST (closest to
  // the expansion) OUTWARDS to the root package. This is the enabler for call-site-aware macros (macwire-style
  // dependency injection, proxy/mock generation, etc.).

  /** A single lexical enclosure (scope) surrounding the current macro expansion point.
    *
    * Obtained from [[enclosingScope]], which returns a [[hearth.fp.data.NonEmptyVector]] of enclosures ordered from the
    * INNERMOST (closest to the expansion point) OUTWARDS to the root package.
    *
    * The variants carry the enclosure's [[name]], optionally its [[fullName]] and source [[position]], and — for
    * [[Enclosure.Class]] and [[Enclosure.Object]] — a way to enumerate ([[Enclosure.Class.members]]) and CALL the
    * members of the enclosing instance.
    *
    * ==Calling an in-scope member (macwire-style)==
    *
    * {{{
    * // Inside a macro, find a no-argument member returning `Dependency` in the immediately-enclosing class/object and
    * // call it on `this`:
    * val receiverAndMembers: Option[(Expr_??, List[Method])] = enclosingScope.iterator.collectFirst {
    *   case enc: Enclosure.Class if enc.thisRef.isDefined => enc.thisRef.get -> enc.members
    *   case enc: Enclosure.Object                         => enc.thisRef     -> enc.members
    * }
    * val call: Option[Expr_??] = receiverAndMembers.flatMap { case (receiver, members) =>
    *   members.collectFirst {
    *     case m: Method.OnInstance if m.isNullary && m.knownReturning.exists(_.Underlying =:= Type[Dependency]) =>
    *       import receiver.value as self
    *       m.apply(self.asInstanceOf[Expr[m.Instance]]) match {
    *         case r: Method.Result[?] => r.build().toOption.map(_.as_??)
    *         case _                   => None
    *       }
    *   }.flatten
    * }
    * }}}
    *
    * @since 0.4.0
    */
  sealed trait Enclosure extends Product with Serializable {

    /** Decoded simple name of the enclosure (e.g. method/val/class/object/package name). */
    def name: String

    /** Fully-qualified name, when cheaply available from the underlying symbol. */
    def fullName: Option[String]

    /** Source position of the enclosure's definition, when available. */
    def position: Option[Position]

    /** A short, stable, cross-platform tag for the enclosure kind ("method", "value", "class", "object", "package"). */
    final def kind: String = this match {
      case _: Enclosure.Method  => "method"
      case _: Enclosure.Value   => "value"
      case _: Enclosure.Class   => "class"
      case _: Enclosure.Object  => "object"
      case _: Enclosure.Package => "package"
    }
  }

  /** Stable alias to the cake's `Method` type, so the nested `Enclosure.Method` case class does not shadow it. */
  type CakeMethod = Method

  object Enclosure {

    /** An enclosing `def` (or the initializer scope of a `val`/`var`/`lazy val` seen as a method owner).
      *
      * Local `val`s declared INSIDE this method body, BEFORE the macro call (the literal macwire case), are exposed
      * best-effort via [[localValues]]. This is populated only for the IMMEDIATELY-enclosing method; outer enclosing
      * methods always have an empty [[localValues]].
      *
      * '''Platform asymmetry''': on Scala 2 (`c.enclosingMethod` exposes the enclosing method body) the in-scope local
      * `val`s are discoverable. On Scala 3, `Symbol.tree` returns the method's `DefDef` but its `rhs` is `None` while
      * the method is being compiled (the body is not yet available during its own macro expansion), so [[localValues]]
      * is empty there. See [[Enclosure.LocalValue]].
      *
      * @since 0.4.0
      */
    final case class Method(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: Option[??],
        localValues: List[Enclosure.LocalValue]
    ) extends Enclosure

    /** An enclosing `val`/`var`/`lazy val`.
      *
      * @since 0.4.0
      */
    final case class Value(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: ??
    ) extends Enclosure

    /** An enclosing `class`/`trait`.
      *
      * [[members]] lists the callable methods/values of the enclosing type. [[thisRef]] is a `this` reference for the
      * enclosing instance, present only for the IMMEDIATELY-enclosing class (a sound `this` for an OUTER enclosing
      * class cannot be materialized cross-platform, so it is [[None]] there).
      *
      * @since 0.4.0
      */
    final case class Class(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: ??,
        members: List[CakeMethod],
        thisRef: Option[Expr_??]
    ) extends Enclosure

    /** An enclosing `object`/module.
      *
      * Because an object has a stable path, [[members]] are always callable via [[thisRef]] (the module reference).
      *
      * @since 0.4.0
      */
    final case class Object(
        name: String,
        fullName: Option[String],
        position: Option[Position],
        tpe: ??,
        members: List[CakeMethod],
        thisRef: Expr_??
    ) extends Enclosure

    /** An enclosing `package`. Terminal element of the chain (the root package). Has no type. */
    final case class Package(
        name: String,
        fullName: Option[String],
        position: Option[Position]
    ) extends Enclosure

    /** A local `val` declared inside an enclosing [[Enclosure.Method]] body, BEFORE the macro call, with a reference
      * expression ([[ref]]) so it can be used as a constructor argument (the macwire use case).
      *
      * '''Platform asymmetry''': discovered only on Scala 2 today. On Scala 3 the enclosing method's body is not
      * available during its own macro expansion (`DefDef.rhs` is `None`), so [[Enclosure.Method.localValues]] is empty.
      * See [[Enclosure.Method]].
      *
      * @since 0.4.0
      */
    final case class LocalValue(name: String, tpe: ??, ref: Expr_??)
  }

  /** The chain of lexical enclosures (scopes) surrounding the current macro-expansion point, from the INNERMOST
    * (closest to the expansion) OUTWARDS to the root package.
    *
    * There is always at least one element (the root package), so the result is a [[hearth.fp.data.NonEmptyVector]].
    *
    * Synthetic/compiler-internal owners (macro-expansion wrappers, `$anonfun`, the synthetic `<root>`/`<empty>` package
    * wrappers, etc.) are filtered out so that the user-visible chain is clean and identical on Scala 2 and Scala 3.
    *
    * See [[Enclosure]] for the variants and a macwire-style member-calling example.
    *
    * @since 0.4.0
    */
  def enclosingScope: NonEmptyVector[Enclosure]
}
