package hearth
package fp

package object effect {

  type Logs = Vector[Log]

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
  implicit class MResultOps[A](result: => MResult[A]) {
    
    def suspend: MIO[A] = MIO.suspend(result)
  }
}
