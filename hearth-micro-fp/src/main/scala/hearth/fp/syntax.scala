package hearth
package fp

import scala.language.implicitConversions

object syntax {

  implicit def pureSyntax[A](a: A): Applicative.PureOps[A] = new Applicative.PureOps(a)
  implicit def functorSyntax[F[_], A](fa: F[A]): Functor.Ops[F, A] = new Functor.Ops(fa)
  implicit def applicativeSyntax[F[_], A](fa: F[A]): Applicative.Ops[F, A] = new Applicative.Ops(fa)
  implicit def parallelSyntax[F[_], A](fa: F[A]): Parallel.Ops[F, A] = new Parallel.Ops(fa)
  implicit def sequenceSyntax[F[_], G[_], A](fga: F[G[A]]): Traverse.SequenceOps[F, G, A] =
    new Traverse.SequenceOps(fga)
  implicit def traverseSyntax[F[_], A](fa: F[A]): Traverse.Ops[F, A] = new Traverse.Ops(fa)
}
