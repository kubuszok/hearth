package hearth

/** Exact JDK version.
  *
  * Uses tools available only in JVM runtime, so it should be used either only on JVM Scala or the code, that is used
  * online for macros (does is not actually linked to Scala.js/Scala Native).
  *
  * @since 0.1.0
  */
final case class JDKVersion(major: Int, minor: Int) {

  override def toString: String = s"JDK $major.$minor"
}

/** Exact JDK version.
  *
  * Uses tools available only in JVM runtime, so it should be used either only on JVM Scala or the code, that is used
  * online for macros (does is not actually linked to Scala.js/Scala Native).
  *
  * @since 0.1.0
  */
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

/** Exact Scala version.
  *
  * Uses heuristics since classpath can contain both Scala 2.13 and Scala 3 standard libraries at once.
  *
  * Uses tools available only in JVM runtime, so it should be used either only on JVM Scala or the code, that is used
  * online for macros (does is not actually linked to Scala.js/Scala Native).
  *
  * @since 0.1.0
  */
final case class ScalaVersion(major: Int, minor: Int, patch: Int) {

  def isScala2_13: Boolean = toLanguageVersion == LanguageVersion.Scala2_13
  def isScala3: Boolean = toLanguageVersion == LanguageVersion.Scala3

  def toLanguageVersion: LanguageVersion = this match {
    case ScalaVersion(2, 13, _) => LanguageVersion.Scala2_13
    case ScalaVersion(3, _, _)  => LanguageVersion.Scala3
    case _                      => throw new RuntimeException(s"Unsupported Scala version: $major.$minor.$patch")
  }

  override def toString: String = s"Scala $major.$minor.$patch"
}

/** Exact Scala version.
  *
  * Uses heuristics since classpath can contain both Scala 2.13 and Scala 3 standard libraries at once.
  *
  * Uses tools available only in JVM runtime, so it should be used either only on JVM Scala or the code, that is used
  * online for macros (does is not actually linked to Scala.js/Scala Native).
  *
  * @since 0.1.0
  */
object ScalaVersion {

  /** Pass blackbox.Context or Quotes to resolve the version from the library. */
  def byScalaLibrary(ctx: Any): ScalaVersion =
    matchScala3Library
      .orElse(matchScala2Library)
      .applyOrElse(
        libraryJarName(ctx),
        (library: String) => throw new RuntimeException(s"Cannot resolve Scala version from library: $library")
      )

  /** Pass blackbox.Context or something from Scala 2 standard library to resolve the version. */
  def resolveByScala2Library(ctx: Any): Option[ScalaVersion] = Option(
    matchScala2Library.applyOrElse(libraryJarName(ctx), (_: String) => null)
  )

  /** Pass Quotes or something from Scala 3 standard library to resolve the version. */
  def resolveByScala3Library(ctx: Any): Option[ScalaVersion] = Option(
    matchScala3Library.applyOrElse(libraryJarName(ctx), (_: String) => null)
  )

  /** Resolves the runtime Scala version. It does not have to be the same as the compiler version. */
  lazy val byJVMRuntime: ScalaVersion =
    try
      resolveByScala3Library(java.lang.Class.forName("scala.util.NotGiven$").getField("MODULE$").get(null)).get
    catch {
      case parent: Throwable =>
        scala.util.Properties.versionNumberString match {
          case s"$major.$minor.$patch" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
          case versionString           =>
            throw new RuntimeException(s"Cannot resolve Scala version from version string: $versionString", parent)
        }
    }

  implicit val ordering: Ordering[ScalaVersion] = Ordering.by((v: ScalaVersion) => (v.major, v.minor, v.patch))

  private def libraryJarName(ctx: Any): String = {
    val library = ctx.getClass.getProtectionDomain.getCodeSource.getLocation.toString
    library.substring(library.lastIndexOf('/') + 1)
  }

  private val matchScala2Library: PartialFunction[String, ScalaVersion] = {
    // when using e.g. blackbox.Context
    case s"scala-compiler-$major.$minor.$patch.jar" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
    case s"scala-reflect-$major.$minor.$patch.jar"  => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
    // when it's something from regular Scala 2 library
    case s"scala-library-$major.$minor.$patch.jar" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
  }

  private val matchScala3Library: PartialFunction[String, ScalaVersion] = {
    // when using e.g. scala.quotes.runtime.impl.QuotesImpl
    case s"scala3-compiler_3-$major.$minor.$patch.jar" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
    // when passing something Scala-3 specific that is NOT a part of 2.13 stdlib
    case s"scala3-library_3-$major.$minor.$patch.jar" => ScalaVersion(major.toInt, minor.toInt, patch.toInt)
  }
}

/** Language version: Scala 2.13, Scala 3.
  *
  * If not provided by [[ScalaVersion]], we can only resolve with that version Hearth was compiled with, since runtime
  * on Scala.js/Scala Native does not contain the necessary tools to resolve the version.
  *
  * @since 0.1.0
  */
sealed trait LanguageVersion extends Product with Serializable {

  final def isScala2_13: Boolean = this == LanguageVersion.Scala2_13
  final def isScala3: Boolean = this == LanguageVersion.Scala3

  final override def toString: String = this match {
    case LanguageVersion.Scala2_13 => "Scala 2.13"
    case LanguageVersion.Scala3    => "Scala 3"
  }
}

/** Language version: Scala 2.13, Scala 3.
  *
  * If not provided by [[ScalaVersion]], we can only resolve with that version Hearth was compiled with, since runtime
  * on Scala.js/Scala Native does not contain the necessary tools to resolve the version.
  *
  * @since 0.1.0
  */
object LanguageVersion extends LanguageVersionCompanionCompat {

  case object Scala2_13 extends LanguageVersion
  case object Scala3 extends LanguageVersion

  implicit val ordering: Ordering[LanguageVersion] = Ordering.by {
    case Scala2_13 => 0
    case Scala3    => 1
  }
}

/** Which platform we are currently compiling for.
  *
  * @since 0.1.0
  */
sealed trait Platform extends Product with Serializable {

  /** Whether we are compiling for the JVM platform. */
  final def isJvm: Boolean = this == Platform.Jvm

  /** Whether we are compiling for the Scala JS platform. */
  final def isJs: Boolean = this == Platform.Js

  /** Whether we are compiling for the Scala Native platform. */
  final def isNative: Boolean = this == Platform.Native
}

/** Which platform we are currently compiling for.
  *
  * @since 0.1.0
  */
object Platform extends PlatformCompanionCompat {

  /** JVM platform. */
  case object Jvm extends Platform

  /** Scala JS platform. */
  case object Js extends Platform

  /** Scala Native platform. */
  case object Native extends Platform
}
