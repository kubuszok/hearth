package hearth
package examples

enum Color { case Red, Green, Blue }

object unions {

  opaque type OpaqueId = Long
  object OpaqueId { def apply(v: Long): OpaqueId = v }

  opaque type OpaqueName = String
  object OpaqueName { def apply(v: String): OpaqueName = v }

  object Marker

  // --- Disjoint (should return Some from directChildren) ---
  type StringOrInt = String | Int
  type BooleanOrDouble = Boolean | Double
  type RedOrBlue = Color.Red.type | Color.Blue.type // singletons matched by value, not by class
  type StringOrIntOrBoolean = String | Int | Boolean
  type RedOrString = Color.Red.type | String // mixed: singleton matched by value + class test
  type MarkerOrInt = Marker.type | Int // mixed: module singleton matched by value + class test

  // --- NOT disjoint (should return None) ---
  type StringOrAnyRef = String | AnyRef // subtype overlap
  type ListIntOrListString = List[Int] | List[String] // same erasure
  type ListIntOrSeqString = List[Int] | Seq[String] // related runtime classes (List <: Seq)
  type IntOrJavaInteger = Int | java.lang.Integer // same runtime class after boxing
  type NothingOrString = Nothing | String // Nothing simplifies away → < 2 members
  type StringOrString = String | String // duplicate → < 2 members
  type IntOrAnyVal = Int | AnyVal // subtype overlap

  // --- Opaque types (conservative → None) ---
  type OpaqueIdOrString = OpaqueId | String // can't prove disjoint at runtime
  type OpaqueIdOrLong = OpaqueId | Long // same runtime erasure (both Long)
  type OpaqueIdOrOpaqueName = OpaqueId | OpaqueName // two opaques, can't prove disjoint

  // --- Same-erasure members, accepted ONLY with user-provided TypeTests in scope at the macro call site ---
  type RedOrListIntOrListString = Color.Red.type | List[Int] | List[String] // mixed: eqValue + TypeTest members

  /** Honest runtime discriminators for the `List[Int]` / `List[String]` union members.
    *
    * `scala.reflect.TypeTest` is contravariant in its scrutinee type, so instances defined for `Any` satisfy implicit
    * searches for any union scrutinee (`TypeTest[Any, T] <: TypeTest[Union, T]`) - the same two givens serve both
    * `ListIntOrListString` and `RedOrListIntOrListString`.
    *
    * Empty lists are runtime-AMBIGUOUS (`List[Int]` and `List[String]` erase to the same class, and an empty list
    * carries no element to inspect), so a deterministic policy is needed: empty lists are claimed by the `List[Int]`
    * instance, and the `List[String]` instance only accepts non-empty lists with a `String` head.
    */
  object typetests {

    given listOfInt: scala.reflect.TypeTest[Any, List[Int]] with {
      override def unapply(value: Any): Option[value.type & List[Int]] = value match {
        case list: List[?] if list.isEmpty || list.head.isInstanceOf[Int] =>
          Some(value.asInstanceOf[value.type & List[Int]])
        case _ => None
      }
    }

    given listOfString: scala.reflect.TypeTest[Any, List[String]] with {
      override def unapply(value: Any): Option[value.type & List[String]] = value match {
        case list: List[?] if list.nonEmpty && list.head.isInstanceOf[String] =>
          Some(value.asInstanceOf[value.type & List[String]])
        case _ => None
      }
    }
  }
}
