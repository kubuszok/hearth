package hearth
package fp

import scala.language.implicitConversions

/** Implicit ops enabling postfix `.map` / `.map2` / `.parMap2` / `.traverse` / `.parTraverse` / `.sequence` / `.pure`
  * syntax on any type with the matching typeclass instance in scope.
  *
  * Import `hearth.fp.syntax.*` to bring the conversions into scope.
  *
  * @since 0.1.0
  */
object syntax {

  implicit def pureSyntax[A](a: A): Applicative.PureOps[A] = new Applicative.PureOps(a)
  implicit def functorSyntax[F[_], A](fa: F[A]): Functor.Ops[F, A] = new Functor.Ops(fa)
  implicit def applicativeSyntax[F[_], A](fa: F[A]): Applicative.Ops[F, A] = new Applicative.Ops(fa)
  implicit def parallelSyntax[F[_], A](fa: F[A]): Parallel.Ops[F, A] = new Parallel.Ops(fa)
  implicit def sequenceSyntax[F[_], G[_], A](fga: F[G[A]]): Traverse.SequenceOps[F, G, A] =
    new Traverse.SequenceOps(fga)
  implicit def traverseSyntax[F[_], A](fa: F[A]): Traverse.Ops[F, A] = new Traverse.Ops(fa)
}
