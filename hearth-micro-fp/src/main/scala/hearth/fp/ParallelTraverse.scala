package hearth
package fp

trait ParallelTraverse[F[_]] extends ApplicativeTraverse[F] with Parallel[F]
object ParallelTraverse {

  def apply[F[_]](implicit F: ParallelTraverse[F]): ParallelTraverse[F] = F
}
