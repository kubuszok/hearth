package hearth

package object fp {

  /** Identity type.
    *
    * Useful when we don't want to do anything, but type system expects is to provide some "wrapper"-type.
    *
    * @since 0.1.0
    */
  type Id[A] = A

  /** Explicitly ignore value(s) to silence compiler warnings about unused non-Unit values.
    *
    * @since 0.1.0
    */
  def ignore(a: Any*): Unit = {
    val _ = a
  }
}
