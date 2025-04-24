package hearth
package fp

import scala.math.Ordered.orderingToOrdered

package object effect {

  type Logs = Vector[Log]
  implicit final class LogsOps(logs: Logs) {

    /** Various ways of rendering [[Logs]] as a String. */
    object render {

      def apply(rootScopeName: String)(filter: Log.Level => Boolean): String = Log.render(rootScopeName, logs)(filter)

      def fromInfo(rootScopeName: String): String = render(rootScopeName)(_ >= Log.Level.Info)
      def fromWarn(rootScopeName: String): String = render(rootScopeName)(_ >= Log.Level.Warn)
      def fromError(rootScopeName: String): String = render(rootScopeName)(_ >= Log.Level.Error)

      def onlyInfo(rootScopeName: String): String = render(rootScopeName)(_ == Log.Level.Info)
      def onlyWarn(rootScopeName: String): String = render(rootScopeName)(_ == Log.Level.Warn)
      def onlyError(rootScopeName: String): String = render(rootScopeName)(_ == Log.Level.Error)
    }
  }

  /** Macro errors */
  type MErrors = data.NonEmptyVector[Throwable]

  /** Eager results of a macro expansion */
  type MResult[+A] = Either[MErrors, A]
  object MResult {

    def pure[A](a: A): MResult[A] = Right(a)
    def fail[A](head: Throwable, tail: Throwable*): MResult[A] = Left(fp.data.NonEmptyVector(head, tail.toVector))
    def fail[A](errs: MErrors): MResult[A] = Left(errs)

    val void: MResult[Unit] = Right(())
  }
  implicit final class MResultOps[A](result: => MResult[A]) {

    def suspend: MIO[A] = MIO.suspend(result)
  }
}
