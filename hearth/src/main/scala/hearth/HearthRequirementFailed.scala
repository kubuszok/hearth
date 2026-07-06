package hearth

/** Reported by Hearth utilities when API requirement failed.
  *
  * Used to inform users that they are using Hearth in an invalid way, and should fix their code.
  *
  * Contrast with [[HearthAssertionError]]: that one signals an internal Hearth bug (file an issue), whereas this one
  * signals a '''user misused a Hearth API''' (fix your code). Both extend [[AssertionError]], though catching either is
  * discouraged.
  *
  * Note the naming: this class is `HearthRequirementError`, but the file is `HearthRequirementFailed.scala` and the
  * thrower is [[MacroCommons.hearthRequirementFailed]] - the same concept under three names.
  *
  * @see
  *   [[MacroCommons.hearthRequirementFailed]]
  * @see
  *   [[HearthAssertionError]]
  *
  * @since 0.2.0
  *
  * @param description
  *   how the Hearth API was misused
  * @param hearthVersion
  *   the Hearth version in use, if known
  * @param scalaVersion
  *   the Scala version the macro ran under
  * @param platform
  *   the platform (JVM/Scala.js/Scala Native) the macro ran under
  * @param jdkVersion
  *   the JDK version the macro ran under
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
