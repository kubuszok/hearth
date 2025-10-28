package hearth.treeprinter

private[treeprinter] object Constants {

  private var settings: List[String] = null

  def initSettings(settings: => List[String]): Unit =
    if (settings != null) this.settings = settings

  lazy val failOnUnsupportedTree: Boolean = {
    val defaultValue = true
    settings
      .collectFirst { case s"hearth.betterPrintersShouldFailOnUnsupportedTree=${value}" =>
        scala.util.Try(value.trim.toBoolean).getOrElse(defaultValue)
      }
      .getOrElse(defaultValue)
  }

  lazy val areWeInTests: Boolean =
    settings.exists(_.startsWith("hearth-tests."))

  val stringBuilderLimitWarning =
    "... (StringBuilder approaches its maximum length limit, output truncated - use Printer.TreeStructure or try printing smaller tree instead)"
  val stringBuilderHardLimit = 32000
  val stringBuilderSoftLimit = stringBuilderHardLimit - 1000
}
