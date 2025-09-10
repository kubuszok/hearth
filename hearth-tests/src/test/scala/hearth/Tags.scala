package hearth

import munit.Tag

object Tags {

  /** Behavior differes between Scala 2 and Scala 3. */
  val langVerMismatch: Tag = new Tag("language-version-mismatch")

  /** Behavior differes between JVM/JS/Native platforms. */
  val platformMismatch: Tag = new Tag("platform-mismatch")

  /** Behavior is flaky when recompiled incrementally. */
  val recompileFlaky: Tag = new Tag("recompile-flaky")

  /** Behavior is flaky when JDK version changes. */
  val jdkFlaky: Tag = new Tag("jdk-flaky")
}
