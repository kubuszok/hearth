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
        "XMacroSettings" -> Data(
          Environment.XMacroSettings.filterNot(_.startsWith("hearth.cross-quotes.")).map(Data(_))
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

  // loadMacroExtensions
}
