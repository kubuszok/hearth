package hearth

import hearth.data.Data

trait EnvironmentFixturesImpl { this: MacroCommons =>

  def testPosition: Expr[Data] = {
    val currentPosition = Environment.currentPosition
    Expr(
      Data.map(
        "file" -> Data(
          currentPosition.file
            .map(_.toString)
            .map(value => value.drop(value.indexOf("hearth-tests")))
            .getOrElse("<no file>")
        ),
        "offset" -> Data(currentPosition.offset),
        "line" -> Data(currentPosition.line),
        "column" -> Data(currentPosition.column),
        "fileName" -> Data(currentPosition.fileName.getOrElse("<no file name>")),
        "prettyPrint" -> Data(currentPosition.prettyPrint)
      )
    )
  }

  def testEnvironment: Expr[Data] =
    Expr(
      Data.map(
        "currentPosition" -> Data(Environment.currentPosition.prettyPrint),
        "currentLanguageVersion" -> Data(Environment.currentLanguageVersion.toString),
        "isScala2_13" -> Data(Environment.isScala2_13),
        "isScala3" -> Data(Environment.isScala3),
        "currentPlatform" -> Data(Environment.currentPlatform.toString),
        "isJvm" -> Data(Environment.isJvm),
        "isJs" -> Data(Environment.isJs),
        "isNative" -> Data(Environment.isNative),
        // Drop possible hearth.cross-quotes.* settings, we are using them for debugging, and they should not fail the test when used.
        "XMacroSettings" -> Data(
          Environment.XMacroSettings.filter(_.startsWith("hearth-tests.")).map(Data(_))
        ),
        "typedSettings" -> Environment.typedSettings.fold(Data(_), _.updateAsMap(_.removed("hearth")))
      )
    )

  def testErrorAndAbort: Expr[Any] = Environment.reportErrorAndAbort("Error and abort message")

  def testReportInfoAtPosition[A: Type]: Expr[Data] = {
    // Scala 2 does not store positions for symbols from a previous compilation unit (they would be None),
    // so we fall back to the macro expansion position to keep the test cross-platform.
    val methodPosition = Type[A].methods.flatMap(_.position).headOption
    val position = methodPosition.getOrElse(Environment.currentPosition)
    Environment.reportInfo(s"Position-targeted info message at ${position.prettyPrint}", position)
    Expr(Data.map("infoReported" -> Data(true)))
  }

  def testReportErrorAtPosition: Expr[Data] = {
    Environment.reportError("Error at position message", Environment.currentPosition)
    Expr(Data.map("errorReported" -> Data(true)))
  }

  def testReportErrorAndAbortAtPosition: Expr[Any] =
    Environment.reportErrorAndAbort("Error and abort at position message", Environment.currentPosition)

  def testIsExpandedAt(position: Expr[String]): Expr[Boolean] = Expr
    .unapply(position)
    .fold(
      Environment.reportErrorAndAbort(s"Position must be a string literal, got ${position.prettyPrint}")
    ) { position =>
      Expr(Environment.isExpandedAt(position))
    }

  def weLoadedAnExtension(name: String): Unit =
    loadedExtensions :+= name
  private var loadedExtensions: Vector[String] = Vector.empty

  def testLoadingExtensions: Expr[Data] = {
    val result = Data.map(
      "successful" -> processResult(Environment.loadMacroExtensions[SuccessfulMacroExtension]),
      "runningFailed" -> processResult(Environment.loadMacroExtensions[PartiallyFailedMacroExtension]),
      "loadingFailed" -> processResult(Environment.loadMacroExtensions[TotallyFailedMacroExtension]),
      // Must be at the end, evaluated once the extensions are loaded.
      "loadedExtensions" -> Data.list(loadedExtensions.map(Data(_))*)
    )
    Expr(result)
  }

  def testLoadingExtensionsExcluding: Expr[Data] = {
    val result = Data.map(
      "successful" -> processResult(
        Environment.loadMacroExtensionsExcluding[SuccessfulMacroExtension]("hearth.Example2MacroExtension")
      ),
      "runningFailed" -> processResult(
        Environment.loadMacroExtensionsExcluding[PartiallyFailedMacroExtension]("hearth.Example3MacroExtension")
      ),
      "loadingFailed" -> processResult(
        Environment.loadMacroExtensionsExcluding[TotallyFailedMacroExtension]("hearth.Example4MacroExtension")
      ),
      // Must be at the end, evaluated once the extensions are loaded.
      "loadedExtensions" -> Data.list(loadedExtensions.map(Data(_))*)
    )
    Expr(result)
  }

  def testLoadingExtensionsWhen: Expr[Data] = {
    val result = Data.map(
      "successful" -> processResult(
        Environment.loadMacroExtensionsWhen[SuccessfulMacroExtension](_.getName.contains("2"))
      ),
      "runningFailed" -> processResult(
        Environment.loadMacroExtensionsWhen[PartiallyFailedMacroExtension](_.getName.contains("2"))
      ),
      "loadingFailed" -> processResult(
        Environment.loadMacroExtensionsWhen[TotallyFailedMacroExtension](_.getName.contains("2"))
      ),
      // Must be at the end, evaluated once the extensions are loaded.
      "loadedExtensions" -> Data.list(loadedExtensions.map(Data(_))*)
    )
    Expr(result)
  }

  def testLoadingExtensionsTwice: Expr[Data] = {
    loadedExtensions = Vector.empty
    val result1 = Environment.loadMacroExtensions[SuccessfulMacroExtension]
    // Load the same extensions again — should NOT call extend() again
    val result2 = Environment.loadMacroExtensions[SuccessfulMacroExtension]
    val result = Data.map(
      "firstLoad" -> processResult(result1),
      "secondLoad" -> processResult(result2),
      "loadedExtensions" -> Data.list(loadedExtensions.map(Data(_))*)
    )
    Expr(result)
  }

  def testPositionPrettyPrintLong: Expr[Data] = {
    val position = Environment.currentPosition
    val long = position.prettyPrintLong
    val lineAndColumn = s":${position.line}:${position.column}"
    val path = long.stripSuffix(lineAndColumn)
    Expr(
      Data.map(
        "path" -> Data(path.drop(path.indexOf("hearth-tests"))),
        "endsWithLineAndColumn" -> Data(long.endsWith(lineAndColumn)),
        "endsWithPrettyPrint" -> Data(long.endsWith(position.prettyPrint))
      )
    )
  }

  def testCompileTimeVersions: Expr[Data] = {
    val scalaVersion = Environment.currentScalaVersion
    val jdkVersion = Environment.currentJDKVersion
    Expr(
      Data.map(
        "scalaLanguageVersion" -> Data(scalaVersion.toLanguageVersion.toString),
        "scalaMajor" -> Data(scalaVersion.major),
        "scalaMinorIs13WhenScala2" -> Data(!scalaVersion.isScala2_13 || scalaVersion.minor == 13),
        "jdkMajor" -> Data(jdkVersion.major),
        "jdkMinorAtLeast8" -> Data(jdkVersion.minor >= 8),
        "jdkSameAsRuntimeJDKVersion" -> Data(jdkVersion == JDKVersion.runtimeJDKVersion)
      )
    )
  }

  def testTypedSettingsAccessors: Expr[Data] = {
    val primitives = Environment.typedSettings.toOption
      .flatMap(_.get("hearth-tests"))
      .flatMap(_.get("primitives"))
      .getOrElse(Data())
    Expr(
      Data.map(
        "intAsInt" -> Data(primitives.get("int").flatMap(_.asInt).isDefined),
        "longAsLong" -> Data(primitives.get("long").flatMap(_.asLong).isDefined),
        "floatAsFloat" -> Data(primitives.get("float").flatMap(_.asFloat).isDefined),
        "doubleAsDouble" -> Data(primitives.get("double").flatMap(_.asDouble).isDefined),
        "booleanAsBoolean" -> Data(primitives.get("boolean").flatMap(_.asBoolean).isDefined),
        "explicitStringAsString" -> Data(primitives.get("explicit-string").flatMap(_.asString).isDefined),
        "implicitStringAsString" -> Data(primitives.get("implicit-string").flatMap(_.asString).isDefined),
        "intRendered" -> Data(primitives.get("int").map(_.render).getOrElse("<missing>"))
      )
    )
  }

  def testMioBenchmarkFlameGraphDir: Expr[Data] =
    Expr(Data(Environment.mioBenchmarkFlameGraphDir.exists(_.endsWith("flame-graphs"))))

  def testReportErrorTwice: Expr[Data] = {
    Environment.reportError("First error message")
    Environment.reportError("Second error message")
    Expr(Data(true))
  }

  def testReportErrorThenAbort: Expr[Any] = {
    Environment.reportError("Error reported before abort")
    Environment.reportErrorAndAbort("Abort reported after error")
  }

  def testHandleMioTerminationReportsError: Expr[Any] = {
    def terminated(): Expr[Any] = throw new fp.effect.MIO.MioTerminationException("Simulated MIO termination")
    Environment.handleMioTerminationException(terminated())
  }

  def testHandleMioTerminationPassesValue: Expr[Int] =
    Environment.handleMioTerminationException(Expr(42))

  def testWithMioTimeout: Expr[Data] = {
    import scala.concurrent.duration.*
    def loopForever: fp.effect.MIO[Int] = fp.effect.MIO.defer(loopForever)
    val success = Environment.withMioTimeout(5.seconds)(fp.effect.MIO.pure(42).unsafe.runSync._2)
    val timedOut = Environment.withMioTimeout(50.millis)(loopForever.unsafe.runSync._2)
    val deadlineResetAfterTimeout = fp.effect.MIO.timeoutDeadlineNanos == Long.MaxValue
    val reentry = Environment.withMioTimeout(5.seconds) {
      try {
        val _ = Environment.withMioTimeout(5.seconds)(())
        "nested call accepted"
      } catch {
        case _: HearthAssertionError => "nested call rejected"
      }
    }
    Expr(
      Data.map(
        "success" -> Data(success.toOption.flatMap(_.toOption).getOrElse(-1)),
        "timedOut" -> Data(timedOut.isLeft),
        "deadlineResetAfterTimeout" -> Data(deadlineResetAfterTimeout),
        "reentry" -> Data(reentry.toOption.getOrElse("timed out")),
        "deadlineResetAfterReentry" -> Data(fp.effect.MIO.timeoutDeadlineNanos == Long.MaxValue)
      )
    )
  }

  /** Logging is switched off when the `MIO` is built, so the program is built inside the block. */
  def testWithMioLoggingDisabled: Expr[Data] = {
    def logOnce = fp.effect.Log.info("logged") >> fp.effect.MIO.pure(1)
    def entries(state: fp.effect.MState): Int = state.logs.count {
      case _: fp.effect.Log.Entry => true
      case _                      => false
    }
    val disabled = Environment.withMioLoggingDisabled {
      val nested = Environment.withMioLoggingDisabled(logOnce.unsafe.runSync._1)
      val afterNested = logOnce.unsafe.runSync._1
      (entries(nested), entries(afterNested))
    }
    val enabled = logOnce.unsafe.runSync._1
    Expr(
      Data.map(
        "entriesInNestedDisabledBlock" -> Data(disabled._1),
        "entriesAfterNestedBlockEnded" -> Data(disabled._2),
        "entriesAfterDisabledBlockEnded" -> Data(entries(enabled))
      )
    )
  }

  private def processResult[A](result: ExtensionLoadingResult[A]): Data = {
    def loadedData(extensions: Seq[A]): Data = Data.list(extensions.map(e => Data(e.getClass.getName))*)
    def errorsData(errors: fp.data.NonEmptyMap[A, Throwable]): Data =
      Data.list(errors.toList.map(e => Data(e._2.getMessage))*)
    def errorsData2(errors: fp.data.NonEmptyVector[Throwable]): Data =
      Data.list(errors.toVector.map(e => Data(e.getMessage))*)
    val detailed = result match {
      case ExtensionLoadingResult.AllLoaded(extensions) =>
        Data.map("loaded" -> loadedData(extensions.toSeq))
      case ExtensionLoadingResult.SomeFailed(extensions, errors) =>
        Data.map("loaded" -> loadedData(extensions.toSeq), "errors" -> errorsData(errors))
      case ExtensionLoadingResult.LoaderFailed(error) =>
        Data.map("error" -> Data(error.getMessage))
    }
    val asOption = result.toOption match {
      case Some(extensions) =>
        Data.map("loaded" -> loadedData(extensions.toSeq))
      case None =>
        Data.map("error" -> Data("Some extensions failed to load"))
    }
    val asEither = result.toEither match {
      case Right(extensions) =>
        Data.map("loaded" -> loadedData(extensions.toSeq))
      case Left(errors) =>
        Data.map("errors" -> errorsData2(errors))
    }
    Data.map(
      "detailed" -> detailed,
      "asOption" -> asOption,
      "asEither" -> asEither
    )
  }
}

// Nothing fancy, just demonstrates that we can distinguish extensions by type, and that they can access the macro API
// that we prepared for them.
abstract class SuccessfulMacroExtension(name: String) extends MacroExtension[EnvironmentFixturesImpl] {

  def extend(ctx: EnvironmentFixturesImpl): Unit = ctx.weLoadedAnExtension(name)
}

// These need corresponding resource under META-INF/services/hearth.SuccessfulMacroExtension
final class Example1MacroExtension extends SuccessfulMacroExtension("Example 1")
final class Example2MacroExtension extends SuccessfulMacroExtension("Example 2")

// Here, we want to initialize the extension, but fail to apply it to the macro context, so it is not loaded.
abstract class PartiallyFailedMacroExtension(name: String) extends MacroExtension[EnvironmentFixturesImpl] {

  def extend(ctx: EnvironmentFixturesImpl): Unit = throw new IllegalStateException(s"Failed to load $name")
}

// This needs corresponding resource under META-INF/services/hearth.PartiallyFailedMacroExtension
final class Example3MacroExtension extends PartiallyFailedMacroExtension("Example 3")

// Here, we are testing failing to instantiate the extension in the first place.
@scala.annotation.nowarn
abstract class TotallyFailedMacroExtension(name: String) extends MacroExtension[EnvironmentFixturesImpl] {

  throw new RuntimeException(s"Failed to load $name")

  def extend(ctx: EnvironmentFixturesImpl): Unit = ()
}

// This needs corresponding resource under META-INF/services/hearth.TotallyFailedMacroExtension
final class Example4MacroExtension extends TotallyFailedMacroExtension("Example 4")
