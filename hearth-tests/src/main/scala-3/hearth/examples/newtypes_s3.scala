package hearth
package examples

// $SCOVERAGE-OFF$ This file is analyzed mostly in the compile time.

/** Scala 3 stubs of newtype encodings, for testing `Type.pseudoCompanionObject`. */
object newtypes3 {

  /** neotype: `Type` is an OPAQUE type declared in the object's generic parent. */
  abstract class OpaqueNewtype[A] {
    opaque type Type = A
    def apply(value: A): Type = value
  }

  object Email extends OpaqueNewtype[String] {
    override def toString: String = "Email"
  }
  type Email = Email.Type

  /** An opaque type with its own companion. */
  opaque type Meters = Double
  object Meters {
    def apply(value: Double): Meters = value
    override def toString: String = "Meters"
  }
}

/** A top-level opaque type without a companion: its prefix is the synthetic `newtypes_s3$package` object. */
opaque type TopLevelOpaque = Int
