package hearth
package nestedexpansion

// Reading the children of the sealed `Level` makes the Scala 2 compiler complete the members of this package, which
// types `Other.http` (its type is inferred) by expanding its macro INSIDE the expansion for `Root.levels`.
final case class Http(port: Int)
sealed trait Level
case object Debug extends Level

object Root {
  val levels: String = MioIntegrationsFixtures.testDirectChildrenNames[Level]
}
object Other {
  val http = MioIntegrationsFixtures.testDirectChildrenNames[Http]
}

final class NestedExpansionSpec extends MacroSuite {

  test("a macro reading sealed children should compile next to a macro-defined val with an inferred type") {
    Root.levels <==> "Debug"
    Other.http <==> "not sealed"
  }
}
