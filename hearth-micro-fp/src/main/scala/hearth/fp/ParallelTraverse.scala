package hearth
package fp

/** Dirty workaround to the problem that [[Parallel]] and [[ApplicativeTraverse]] both inherit from [[Applicative]], and
  * if both would be in implicit scope as separate values, then when we asked for a [[Applicative]] compiler would be
  * confused and return us none.
  *
  * @since 0.1.0
  */
trait ParallelTraverse[F[_]] extends ApplicativeTraverse[F] with Parallel[F]
object ParallelTraverse {

  def apply[F[_]](implicit F: ParallelTraverse[F]): ParallelTraverse[F] = F
}
