package hearth

/** Which platform we are currently compiling for.
  *
  * @since 0.1.0
  */
sealed trait Platform extends Product with Serializable

/** Which platform we are currently compiling for.
  *
  * @since 0.1.0
  */
object Platform {

  /** JVM platform. */
  case object Jvm extends Platform
  /** Scala JS platform. */
  case object Js extends Platform
  /** Scala Native platform. */
  case object Native extends Platform

  /** The current platform we are compiling for. */
  val current: Platform = Native

  /** Whether we are compiling for the JVM platform. */
  val isJvm: Boolean = current == Jvm
  /** Whether we are compiling for the Scala JS platform. */
  val isJs: Boolean = current == Js
  /** Whether we are compiling for the Scala Native platform. */
  val isNative: Boolean = current == Native
}