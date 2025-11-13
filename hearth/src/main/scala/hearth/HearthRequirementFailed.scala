package hearth

/** Reported by Hearth utilities when API requirement failed.
  *
  * Used to inform users that they are using Hearth in an invalid way, and should fix their code.
  *
  * @since 0.2.0
  */
final case class HearthRequirementError(
    description: String,
    hearthVersion: Option[HearthVersion],
    scalaVersion: ScalaVersion,
    platform: Platform,
    jdkVersion: JDKVersion
) extends AssertionError(
      s"""Hearth requirement failed:
         |${description.split("\n").map("  " + _).mkString("\n")}
         |
         |Hearth version: ${hearthVersion.fold("unknown version")(_.toString)}
         |Scala version:  $scalaVersion
         |Platform:       $platform
         |JDK version:    $jdkVersion
         |
         |Please report an issue to the library maintainer.
         |""".stripMargin
    )
