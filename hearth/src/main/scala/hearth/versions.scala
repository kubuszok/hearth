package hearth

/** Exact compiler version.
  *
  * @since 0.1.0
  */
final case class ScalaVersion(major: Int, minor: Int, patch: Int) {

  def toLanguageVersion: LanguageVersion = this match {
    case ScalaVersion(2, 13, _) => LanguageVersion.Scala2_13
    case ScalaVersion(3, _, _)  => LanguageVersion.Scala3
    case _                      => throw new RuntimeException(s"Unsupported Scala version: $major.$minor.$patch")
  }

  override def toString: String = s"$major.$minor.$patch"
}
object ScalaVersion {

  /** Pass blackbox.Context or Quotes to resolve the version from the library. */
  def resolveByLibrary(ctx: Any): ScalaVersion = {
    val library = ctx.getClass.getProtectionDomain.getCodeSource.getLocation.toString
    library.substring(library.lastIndexOf('/') + 1) match {
      // when using e.g. scala.quotes.runtime.impl.QuotesImpl
      case s"scala3-compiler_3-$major.$minor.$patch.jar" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
      // when using e.g. blackbox.Context
      case s"scala-compiler-$major.$minor.$patch.jar" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
      case s"scala-reflect-$major.$minor.$patch.jar"  => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
      case _ => throw new RuntimeException(s"Cannot resolve Scala version from library: $library")
    }
  }

  implicit val ordering: Ordering[ScalaVersion] = Ordering.by((v: ScalaVersion) => (v.major, v.minor, v.patch))
}

/** Language version, e.g. Scala 2.13, Scala 3.
  *
  * @since 0.1.0
  */
sealed trait LanguageVersion extends Product with Serializable {

  override def toString: String = this match {
    case LanguageVersion.Scala2_13 => "Scala 2.13"
    case LanguageVersion.Scala3    => "Scala 3"
  }
}
object LanguageVersion {

  case object Scala2_13 extends LanguageVersion
  case object Scala3 extends LanguageVersion

  implicit val ordering: Ordering[LanguageVersion] = Ordering.by {
    case Scala2_13 => 0
    case Scala3    => 1
  }
}
