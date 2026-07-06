package hearth

/** Reported by Hearth utilities when some internal assertion failed.
  *
  * Allows user to report an issue at https://github.com/kubuszok/hearth/issues.
  *
  * It would not be used for reporting errors caused by passing the wrong data to Hearth APIs.
  *
  * Contrast with [[HearthRequirementError]]: this one signals an '''internal invariant broke''' (a Hearth bug - file an
  * issue, do not change your own code), whereas `HearthRequirementError` signals a '''user misused a Hearth API''' (fix
  * your code). Both extend [[AssertionError]], though catching either is discouraged.
  *
  * @see
  *   [[MacroCommons.hearthAssertionFailed]]
  * @see
  *   [[HearthRequirementError]]
  *
  * @since 0.1.0
  *
  * @param description
  *   what invariant was violated
  * @param hearthVersion
  *   the Hearth version in use, if known
  * @param scalaVersion
  *   the Scala version the macro ran under
  * @param platform
  *   the platform (JVM/Scala.js/Scala Native) the macro ran under
  * @param jdkVersion
  *   the JDK version the macro ran under
  */
final case class HearthAssertionError(
    description: String,
    hearthVersion: Option[HearthVersion],
    scalaVersion: ScalaVersion,
    platform: Platform,
    jdkVersion: JDKVersion
) extends AssertionError(
      s"""Hearth assertion failed:
         |${description.split("\n").map("  " + _).mkString("\n")}
         |
         |Hearth version: ${hearthVersion.fold("unknown version")(_.toString)}
         |Scala version:  $scalaVersion
         |Platform:       $platform
         |JDK version:    $jdkVersion
         |
         |Please report an issue at https://github.com/kubuszok/hearth/issues""".stripMargin
    )
