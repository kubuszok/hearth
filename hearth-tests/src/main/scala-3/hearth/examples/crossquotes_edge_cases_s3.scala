package hearth
package examples
package crossquotesedge

object opaques {
  opaque type Wrapped[A] = Option[A]
  object Wrapped {
    def apply[A](a: A): Wrapped[A] = Some(a)
  }

  opaque type Plain = Int
  object Plain {
    def apply(i: Int): Plain = i
  }
}
