package hearth

object compat {

  implicit final class RegexpOps(private val r: scala.util.matching.Regex) extends AnyVal {

    def matches(input: CharSequence): Boolean = r.pattern.matcher(input).matches()
  }
}
