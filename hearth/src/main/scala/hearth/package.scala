import scala.util.control.NonFatal

package object hearth {

  private[hearth] val AnsiControlCode = "\u001b\\[([0-9]+)m".r
  private[hearth] def removeAnsiColors(str: String): String = AnsiControlCode.replaceAllIn(str, "")

  // The `$macro$n` suffixes of fresh names make the output unstable in tests and harder to read.
  private[hearth] def removeMacroSuffix(str: String): String = str
    .replaceAll("\\$macro\\$\\d+", "")

  implicit final private[hearth] class HearthChaining[A](private val a: A) extends AnyVal {

    def attemptPipe[B](attempt: A => B)(orElse: A => B): B = try
      attempt(a)
    catch {
      case NonFatal(_) => orElse(a)
    }
  }
}
