package hearth
package sandwich
package examples3

/** Scala 3-only example types used for 2.13×3 sandwich tests.
  *
  * These are deliberately small and mirror the Scala 2.13 equivalents in `hearth.sandwich.examples213`.
  */
object Examples3 {

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

  // JavaBean-style class mirroring the Scala 2.13 BeanExample

  final class BeanExample() {
    private var booleanField: Boolean = false
    private var intField: Int = 0
    private var stringField: String = ""

    def getBooleanField: Boolean = booleanField
    def setBooleanField(value: Boolean): Unit =
      booleanField = value

    def getIntField: Int = intField
    def setIntField(value: Int): Unit =
      intField = value

    def getStringField: String = stringField
    def setStringField(value: String): Unit =
      stringField = value
  }

  // Scala 3 enum corresponding to the Scala 2.13 sealed hierarchy

  enum Color3 {
    case Rgb(r: Int, g: Int, b: Int)
    case Red
  }

  /** Opaque type to verify how it is seen from Scala 2.13 macros. */
  opaque type OpaqueId = Long

  object OpaqueId {
    def apply(value: Long): OpaqueId = value

    extension (id: OpaqueId) {
      def value: Long = id
    }
  }

  /** Class with multiple methods with default parameters (beyond constructors). */
  final class MethodsWithDefaults(val base: Int, val scale: Int = 2) {

    def addWithDefault(delta: Int = 1): Int = base + delta

    def scaledAdd(delta: Int = 1, extra: Int = 0): Int =
      (base + delta + extra) * scale
  }
}
