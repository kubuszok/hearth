package hearth
package crossquotes

// These live in package `hearth.crossquotes`, while the consuming spec lives in package `hearth`, so a quote that
// references them by their SHORT names only resolves at the expansion site if the reified Scala 2 source
// fully-qualifies them - just like a separately-compiled macro extension quoting its own companions/objects.

/** Companion class + object sharing a name: `CrossUnitWrapper.wrap` must not resolve its qualifier to the class at the
  * expansion site.
  */
final class CrossUnitWrapper private (val unwrap: String)
object CrossUnitWrapper {
  def wrap(value: String): CrossUnitWrapper = new CrossUnitWrapper(value)
}

/** A plain object referenced unqualified: `CrossUnitSupport` must not become `not found: value CrossUnitSupport` at the
  * expansion site.
  */
object CrossUnitSupport {
  def describe(wrapper: CrossUnitWrapper): String = "wrapped(" + wrapper.unwrap + ")"
}
