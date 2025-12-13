package hearth.treeprinter

private[treeprinter] object Constants {

  private var settings: List[String] = null

  def initSettings(settings: => List[String]): Unit =
    if (settings != null) this.settings = settings

  // $COVERAGE-OFF$ Should only be triggered by error we don't know about
  lazy val failOnUnsupportedTree: Boolean = {
    val defaultValue = true
    settings
      .collectFirst { case s"hearth.betterPrintersShouldFailOnUnsupportedTree=${value}" =>
        scala.util.Try(value.trim.toBoolean).getOrElse(defaultValue)
      }
      .getOrElse(defaultValue)
  }
  // $COVERAGE-ON$

  lazy val areWeInTests: Boolean =
    settings.exists(_.startsWith("hearth-tests."))

  // Limits due to UTF8 encoding - 65536 is the maximum length of a string in UTF8 ("length is represented by 2 bytes").
  val stringBuilderLimitWarning =
    "... (StringBuilder approaches its maximum length limit, output truncated - use Printer.TreeStructure or try printing smaller tree instead)"
  val stringBuilderHardLimit = 65536
  val stringBuilderSoftLimit = stringBuilderHardLimit - 1000
}
