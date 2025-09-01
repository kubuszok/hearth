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

  override def toString: String = s"Scala $major.$minor.$patch"
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

  /** Resolves the runtime Scala version. It does not have to be the same as the compiler version. */
  lazy val runtimeScalaVersion: ScalaVersion =
    (try
      getClass.getClassLoader
        .loadClass("dotty.tools.dotc.config.Properties")
        .getMethod("versionNumberString")
        .invoke(null)
        .asInstanceOf[String]
    catch {
      case _: ClassNotFoundException => scala.util.Properties.versionNumberString
    }) match {
      case s"$major.$minor.$patch" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
      case versionString           =>
        throw new RuntimeException(s"Cannot resolve Scala version from version string: $versionString")
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

final case class JDKVersion(major: Int, minor: Int) {

  override def toString: String = s"JDK $major.$minor"
}
object JDKVersion {

  lazy val runtimeJDKVersion: JDKVersion = {
    val major = 1
    val minor = fromRuntimeApi
      .orElse(fromProps)
      .orElse(fromClassVersion)
      .getOrElse(throw new RuntimeException("Cannot determine Java version"))
    JDKVersion(major, minor)
  }

  // Try JDK 9+ API via reflection to stay binary-compatible with JDK 8
  private val fromRuntimeApi: Option[Int] =
    try {
      val rt = classOf[java.lang.Runtime]
      val vMethod = rt.getMethod("version")
      val vObj = vMethod.invoke(Runtime.getRuntime)
      val fMethod = vObj.getClass.getMethod("feature")
      Some(fMethod.invoke(vObj).asInstanceOf[Integer].intValue)
    } catch {
      case _: Throwable => None
    }

  private def parseSpecOrVersion(s: String): Option[Int] = {
    // keep only digits and dots at the start (handles "23-ea+7")
    val head = s.takeWhile(ch => ch.isDigit || ch == '.')
    val parts = head.split("\\.", -1).toList
    parts match {
      case "1" :: minor :: _ if minor.forall(_.isDigit) => Some(minor.toInt) // "1.8" -> 8
      case major :: _ if major.forall(_.isDigit)        => Some(major.toInt) // "11"  -> 11
      case _                                            => None
    }
  }

  private def fromProps: Option[Int] =
    sys.props
      .get("java.specification.version")
      .flatMap(parseSpecOrVersion)
      .orElse(sys.props.get("java.version").flatMap(parseSpecOrVersion))

  private def fromClassVersion: Option[Int] =
    sys.props.get("java.class.version").flatMap { s =>
      val head = s.takeWhile(ch => ch.isDigit || ch == '.')
      if (head.nonEmpty) {
        val major = math.floor(head.toDouble).toInt
        Some(major - 44) // classfile major -> feature
      } else None
    }

  implicit val ordering: Ordering[JDKVersion] = Ordering.by((v: JDKVersion) => (v.major, v.minor))
}
