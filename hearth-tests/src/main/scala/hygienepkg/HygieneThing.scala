package hygienepkg

// A top-level package (like `cats` or `io`) whose name can be shadowed by a member at the macro expansion site.
final case class HygieneThing(value: Int)
object HygieneThing {
  def make(value: Int): HygieneThing = HygieneThing(value)
}
