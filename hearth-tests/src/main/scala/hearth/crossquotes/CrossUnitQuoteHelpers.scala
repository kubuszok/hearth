package hearth
package crossquotes

// Support types for the hearth#320 regression (see Issue320ReproFixturesImpl / Issue320Spec).
//
// They live in package `hearth.crossquotes`, while the consuming spec lives in package `hearth`, so a quote that
// references them by their SHORT names (as the fixture does) only resolves at the expansion site if the reified
// Scala 2 source fully-qualifies them. This mirrors the Chimney migration scenario where a separately-compiled
// extension quoted companion/object references that then failed to resolve downstream.

/** Companion class + object sharing a name (issue #320 case 1: `CrossUnitWrapper.wrap` must not resolve its qualifier
  * to the class at the expansion site).
  */
final class CrossUnitWrapper private (val unwrap: String)
object CrossUnitWrapper {
  def wrap(value: String): CrossUnitWrapper = new CrossUnitWrapper(value)
}

/** A plain object referenced unqualified (issue #320 case 2: `CrossUnitSupport` must not become `not found: value
  * CrossUnitSupport` at the expansion site).
  */
object CrossUnitSupport {
  def describe(wrapper: CrossUnitWrapper): String = "wrapped(" + wrapper.unwrap + ")"
}
