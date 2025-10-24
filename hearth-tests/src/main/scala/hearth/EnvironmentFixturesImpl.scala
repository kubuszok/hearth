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

  def testLoadingExtensions: Expr[Data] =
    Environment.loadMacroExtensions[ExampleMacroExtension] match {
      case Left(errors) =>
        Environment.reportErrorAndAbort(s"Failed to load macro extensions: ${errors.mkString("\n")}")
      case Right(_) =>
        Expr(Data.list(loadedExtensions.map(Data(_))*))
    }
}

// Nothing fancy, jsut demonstarate that we can distinct extensions by type, and that they can access macro API
// that we prepared for them.
abstract class ExampleMacroExtension(name: String) extends MacroExtension[EnvironmentFixturesImpl] {

  def extend(ctx: EnvironmentFixturesImpl): Unit = ctx.weLoadedAnExtension(name)
}

// These need corresponding resource under META-INF/services/hearth.ExampleMacroExtension
final class Example1MacroExtension extends ExampleMacroExtension("Example 1")
final class Example2MacroExtension extends ExampleMacroExtension("Example 2")
