package hearth

package object fp {

  type Id[A] = A

  /** Explicitly ignore value(s) to silence compiler warnings about unused non-Unit values. */
  def ignore(a: Any*): Unit = {
    val _ = a
  }
}
