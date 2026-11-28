package hearth
package examples
package methods

class WithGivens {
  given givenValue: Int = 42
  def methodWithUsingParam(using x: Int): String = x.toString
}

class WithUsingClauses {
  def curriedUsing(a: Int)(using b: Int): Int = a + b
}

/** An extension method in the companion, with default values reading the receiver. */
final class VecWithExtensionCopy(val x: Double, val y: Double) {
  override def toString(): String = s"Vec($x, $y)"
}
object VecWithExtensionCopy {
  extension (v: VecWithExtensionCopy) {
    def copy(x: Double = v.x, y: Double = v.y): VecWithExtensionCopy = new VecWithExtensionCopy(x, y)
  }
}
