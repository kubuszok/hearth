package hearth

import hearth.data.Data

/** Fixtured for testing [[EnvironmentSpec]]. */
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

// Nothing fancy, just demonstarate that we can distinct extensions by type, and that they can access macro API
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
