package hearth
package examples

enum Color { case Red, Green, Blue }

object unions {

  opaque type OpaqueId = Long
  object OpaqueId { def apply(v: Long): OpaqueId = v }

  opaque type OpaqueName = String
  object OpaqueName { def apply(v: String): OpaqueName = v }

  // --- Disjoint (should return Some from directChildren) ---
  type StringOrInt = String | Int
  type BooleanOrDouble = Boolean | Double
  type RedOrBlue = Color.Red.type | Color.Blue.type
  type StringOrIntOrBoolean = String | Int | Boolean

  // --- NOT disjoint (should return None) ---
  type StringOrAnyRef = String | AnyRef // subtype overlap
  type ListIntOrListString = List[Int] | List[String] // same erasure
  type NothingOrString = Nothing | String // Nothing simplifies away → < 2 members
  type StringOrString = String | String // duplicate → < 2 members
  type IntOrAnyVal = Int | AnyVal // subtype overlap

  // --- Opaque types (conservative → None) ---
  type OpaqueIdOrString = OpaqueId | String // can't prove disjoint at runtime
  type OpaqueIdOrLong = OpaqueId | Long // same runtime erasure (both Long)
  type OpaqueIdOrOpaqueName = OpaqueId | OpaqueName // two opaques, can't prove disjoint
}
