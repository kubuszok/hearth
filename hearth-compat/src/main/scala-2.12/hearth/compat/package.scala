package hearth

object compat {

  implicit final class OrderingCompatOps[T](private val outer: Ordering[T]) extends AnyVal {

    def orElse(other: Ordering[T]): Ordering[T] = (x, y) => {
      val res1 = outer.compare(x, y)
      if (res1 != 0) res1 else other.compare(x, y)
    }

    def orElseBy[S](f: T => S)(implicit ord: Ordering[S]): Ordering[T] = (x, y) => {
      val res1 = outer.compare(x, y)
      if (res1 != 0) res1 else ord.compare(f(x), f(y))
    }
  }

  implicit final class RegexpCompatOps(private val r: scala.util.matching.Regex) extends AnyVal {

    def matches(input: CharSequence): Boolean = r.pattern.matcher(input).matches()
  }
}
