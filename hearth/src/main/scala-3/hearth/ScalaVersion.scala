package hearth

sealed trait ScalaVersion extends Product with Serializable
object ScalaVersion {
  case object Scala2_12 extends ScalaVersion
  case object Scala2_13 extends ScalaVersion
  case object Scala3 extends ScalaVersion

  val current: ScalaVersion = Scala3
}
