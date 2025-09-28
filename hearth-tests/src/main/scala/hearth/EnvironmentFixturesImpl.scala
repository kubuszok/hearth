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
}
