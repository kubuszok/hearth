import scala.util.control.NonFatal

package object hearth {

  // Useful to remove all ANSI coloring from the text
  private[hearth] val AnsiControlCode = "\u001b\\[([0-9]+)m".r
  private[hearth] def removeAnsiColors(str: String): String = AnsiControlCode.replaceAllIn(str, "")

  // TODO: create our own compat-module

  implicit private[hearth] class Chaining[A](private val a: A) extends AnyVal {

    def pipe[B](f: A => B): B = f(a)
    def attemptPipe[B](attept: A => B)(orElse: A => B): B = try
      attept(a)
    catch {
      case NonFatal(_) => orElse(a)
    }
    def tap[B](f: A => B): A = { val _ = f(a); a }
  }
}
