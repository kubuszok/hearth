import scala.util.control.NonFatal

package object hearth {

  // Useful to remove all ANSI coloring from the text
  private[hearth] val AnsiControlCode = "\u001b\\[([0-9]+)m".r
  private[hearth] def removeAnsiColors(str: String): String = AnsiControlCode.replaceAllIn(str, "")

  implicit final private[hearth] class HearthChaining[A](private val a: A) extends AnyVal {

    def attemptPipe[B](attempt: A => B)(orElse: A => B): B = try
      attempt(a)
    catch {
      case NonFatal(_) => orElse(a)
    }
  }
}
