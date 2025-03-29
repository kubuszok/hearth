package hearth

sealed trait ScalaVersion extends Product with Serializable
object ScalaVersion {
  case object Scala2_12 extends ScalaVersion
  case object Scala2_13 extends ScalaVersion
  case object Scala3 extends ScalaVersion

  val current: ScalaVersion =
    if (scala.util.Properties.versionNumberString < "2.13") ScalaVersion.Scala2_12
    else ScalaVersion.Scala2_13
}
