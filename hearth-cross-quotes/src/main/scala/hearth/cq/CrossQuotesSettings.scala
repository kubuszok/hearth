package hearth.cq

import java.io.File as JFile

object CrossQuotesSettings {

  /** Name of the compiler plugin, and also prefix of its settings. */
  val crossQuotesName = "hearth.cross-quotes"

  /** Name of the logging setting. */
  val loggingSettingName = "logging"

  /** Scala 2 settings are not trimmed, so we need to do it manually. */
  def parseLoggingSettingsForScala2(settings: List[String]): Option[JFile] => Boolean = parse(settings.collect {
    case setting if setting.startsWith(crossQuotesName) => setting.stripPrefix(crossQuotesName + ".").trim
  })

  /** Cross-quotes name should have been checked by the compiler, it's already trimmed. */
  def parseLoggingSettingsForScala3(settings: List[String]): Option[JFile] => Boolean = parse(settings)

  private def parse(settings: List[String]): Option[JFile] => Boolean = settings
    .collectFirst[Option[JFile] => Boolean] {
      case setting if setting.startsWith(loggingSettingName) =>
        val value = setting.stripPrefix(loggingSettingName + "=").trim
        if (value == "true") (_: Option[JFile]) => true
        else if (value == "false") (_: Option[JFile]) => false
        else {
          val files = value.split(",").map(_.trim)
          (file: Option[JFile]) => file.exists(f => files.exists(f.getName.endsWith))
        }
    }
    .getOrElse((_: Option[JFile]) => false)
}
