package hearth
package sandwich
package examples213

import scala.beans.BeanProperty

/** Scala 2.13-only example types used for 2.13×3 sandwich tests.
  *
  * These are deliberately small and mirror the Scala 3 equivalents in
  * `hearth.sandwich.examples3`.
  */
object Examples213 {

  // Case classes and value classes

  final case class SimpleCaseClass(a: Int, b: String)

  final case class CaseClassWithDefaults(
      a: Int,
      b: String = "default-b",
      c: Option[Long] = None,
      d: List[String] = Nil
  ) {
    def methodWithDefault(x: Int = 42): Int = a + x
  }

  /** AnyVal wrapper to test how value classes are seen across Scala versions. */
  final class AnyValId(val value: Long) extends AnyVal

  // JavaBean-style class using @BeanProperty

  final class BeanExample() {
    @BeanProperty var booleanField: Boolean = false
    @BeanProperty var intField: Int = 0
    @BeanProperty var stringField: String = ""
  }

  // Sealed hierarchy mirroring a Scala 3 enum

  sealed trait Color213
  object Color213 {
    final case class Rgb(r: Int, g: Int, b: Int) extends Color213
    case object Red extends Color213
  }

  /** Class with multiple methods with default parameters (beyond constructors). */
  final class MethodsWithDefaults(val base: Int, val scale: Int = 2) {

    def addWithDefault(delta: Int = 1): Int = base + delta

    def scaledAdd(delta: Int = 1, extra: Int = 0): Int =
      (base + delta + extra) * scale
  }
}

