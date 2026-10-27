package hearth
package examples

// $SCOVERAGE-OFF$ This file is analyzed mostly in the compile time.

/** Scala 3-only edge cases for the standard extensions. */
object stdedgeopaque {

  opaque type NoCompanionOpaque = String
  def makeNoCompanionOpaque(value: String): NoCompanionOpaque = value

  opaque type GenericOpaque[A] = A
  object GenericOpaque {
    def apply[A](value: A): GenericOpaque[A] = value
  }

  type GenericOpaqueOfString = GenericOpaque[String]
}
