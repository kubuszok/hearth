package hearth

/** Reported by Hearth utilities when some internal assertion failed.
  *
  * Allows user to report an issue at https://github.com/MateuszKubuszok/hearth/issues.
  *
  * It would not be used for reporting errors caused by passing the wrong data to Hearth APIs.
  *
  * @since 0.1.0
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
         |Please report an issue at https://github.com/MateuszKubuszok/hearth/issues""".stripMargin
    )
