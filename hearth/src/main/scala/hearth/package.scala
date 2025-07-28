import scala.util.control.NonFatal

package object hearth {

  // Useful to remove all ANSI coloring from the text
  private[hearth] val AnsiControlCode = "\u001b\\[([0-9]+)m".r
  private[hearth] def removeAnsiColors(str: String): String = AnsiControlCode.replaceAllIn(str, "")

  // TODO: create our own compat-module
  // TODO: if we'll get rid of 2.12 this will not be needed

  implicit final private[hearth] class Chaining[A](private val a: A) extends AnyVal {

    def pipe[B](f: A => B): B = f(a)
    def attemptPipe[B](attempt: A => B)(orElse: A => B): B = try
      attempt(a)
    catch {
      case NonFatal(_) => orElse(a)
    }
    def tap[B](f: A => B): A = { fp.ignore(f(a)); a }
  }
}
