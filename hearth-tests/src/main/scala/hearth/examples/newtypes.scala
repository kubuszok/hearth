package hearth
package examples

// $SCOVERAGE-OFF$ This file is analyzed mostly in the compile time.

/** Stubs of newtype libraries' encodings, for testing `Type.pseudoCompanionObject`. */
object newtypes {

  /** neotype / zio-prelude / monix-newtypes: `type Type` is declared in the generic parent of the object. */
  abstract class Newtype[A] {
    type Type
    def apply(value: A): Type = value.asInstanceOf[Type]
  }

  object UserName extends Newtype[String] {
    override def toString: String = "UserName"
  }
  type UserName = UserName.Type

  /** estatico/scala-newtype (`@newtype case class Age(value: Int)`): no generic parent, `Repr` is a member. */
  object Age {
    type Repr = Int
    type Base = Any { type Age__newtype }
    trait Tag extends Any
    type Type <: Base with Tag
    def apply(value: Int): Age = value.asInstanceOf[Age]
    override def toString: String = "Age"
  }
  type Age = Age.Type

  /** estatico/scala-newtype for a generic newtype (`@newtype case class Tagged[A](value: A)`). */
  object Tagged {
    type Base = Any { type Tagged__newtype }
    trait Tag[A] extends Any
    type Type[A] <: Base with Tag[A]
    def apply[A](value: A): Tagged[A] = value.asInstanceOf[Tagged[A]]
    override def toString: String = "Tagged"
  }
  type Tagged[A] = Tagged.Type[A]

  type Login = UserName

  /** A plain alias with a same-name object - NOT a newtype: the alias is dealiased to a class without a companion. */
  class NoCompanion
  type Plain = NoCompanion
  object Plain

  /** A case class has a real companion, which wins. */
  case class Real(value: Int)
  object Real {
    override def toString: String = "Real"
  }

  trait HasMember { type T }
  val hasMember: HasMember = new HasMember { type T = Int }
}
