package hearth.cq

import java.io.File as JFile

object CrossQuotesSettings {

  /** Name of the compiler plugin, and also prefix of its settings. */
  val crossQuotesName = "hearth.cross-quotes"

  /** Scala 2 settings are not trimmed, so we need to do it manually. */
  def parseLoggingSettingsForScala2(settings: List[String]): (Option[JFile], Int, Int) => Boolean = parse(
    settings.collect {
      case setting if setting.startsWith(crossQuotesName + ".") => setting.stripPrefix(crossQuotesName + ".").trim
    }
  )

  /** Cross-quotes name should have been checked by the compiler, it's already trimmed. */
  def parseLoggingSettingsForScala3(settings: List[String]): (Option[JFile], Int, Int) => Boolean = parse(settings)

  /** Name of the logging setting. */
  val loggingSettingName = "logging"

  private def parse(settings: List[String]): (Option[JFile], Int, Int) => Boolean = settings
    .collectFirst[((Option[JFile], Int, Int) => Boolean)] {
      case setting if setting.startsWith(loggingSettingName + "=") =>
        val value = setting.stripPrefix(loggingSettingName + "=").trim
        if (value == "true") (_, _, _) => true
        else if (value == "false") (_, _, _) => false
        else {
          val checks = value.split(',').map(_.trim).map(isExpandedAt)
          (currentPositionFile, currentPositionLine, currentPositionColumn) =>
            checks.exists(_(currentPositionFile, currentPositionLine, currentPositionColumn))
        }
    }
    .getOrElse((_, _, _) => false)

  /** Copy of Environments.isExpandedAt, but for Cross-Quotes. */
  private def isExpandedAt(compilationLogPosition: String): (Option[JFile], Int, Int) => Boolean =
    compilationLogPosition match {
      case fileLineColumnRegex(fileName, line, column) =>
        (currentPositionFile, currentPositionLine, currentPositionColumn) => {
          val fileMatches = currentPositionFile.exists(_.toString.endsWith(fileName))
          val lineMatches = scala.util.Try(line.toInt).toOption.contains(currentPositionLine)
          val columnMatches = scala.util.Try(column.toInt).toOption.contains(currentPositionColumn)
          fileMatches && lineMatches && columnMatches
        }
      case fileLineRegex(fileName, line) =>
        (currentPositionFile, currentPositionLine, _) => {
          val fileMatches = currentPositionFile.exists(_.toString.endsWith(fileName))
          val lineMatches = scala.util.Try(line.toInt).toOption.contains(currentPositionLine)
          fileMatches && lineMatches
        }
      case fileName =>
        (currentPositionFile, _, _) => {
          val fileMatches = currentPositionFile.exists(_.toString.endsWith(fileName))
          fileMatches
        }
    }
  private val fileLineRegex = """^(.+):(\d+)$""".r
  private val fileLineColumnRegex = """^(.+):(\d+):(\d+)$""".r
}
