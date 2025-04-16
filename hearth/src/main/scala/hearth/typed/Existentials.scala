package hearth
package typed

trait Existentials { this: MacroCommons =>

  /** Represents value with some existential type `t` both for `Type[t]` as well as `F[t]`.
    *
    * Since Scala 3 removed a lot of cases for existential types we cannot just use `Type[?]` in shared code.
    * Additionally, we might need to have something to prove that our `Type[?]` is has the same `?` as some `Value[?]`.
    * For that, this utility would be useful.
    *
    * When we obtain value whose type is forgotten we can still access it, and give it a name the following way:
    *
    * {{{
    * val existentialExpr: Existential[Expr] = ...
    * import existentialExpr.{ Underlying as ExprType, expr }
    * // now we have:
    * // implicit ExprType: Type[ExprType]
    * // expr: Expr[ExprType]
    * }}}
    *
    * It works similarly with `Existential` values of other types.
    *
    * Both Types and Exprs have useful aliases provided:
    *   - `??` indicate no type bounds
    *   - `??>:[L]` indicate the lower bound (`Underlying >: L`)
    *   - `??<:[U]` indicate the upper bound (`Underlying <: U`)
    *   - `<:??<:[L, U]` indicate both upper and lower bounds (`Underlying >: L <: U`)
    */
  final type Existential[F[_]] = Existential.Bounded[Nothing, Any, F]
  object Existential {

    def apply[F[_], A: Type](value: F[A]): Existential[F] = Bounded[Nothing, Any, F, A](value)

    sealed trait Bounded[L, U >: L, F[_ >: L <: U]] {

      type Underlying >: L <: U
      implicit val Underlying: Type[Underlying]

      val value: F[Underlying]

      def mapK[G[_]](f: Type[Underlying] => F[Underlying] => G[Underlying]): Bounded[L, U, G] =
        Bounded[L, U, G, Underlying](f(Underlying)(value))(Underlying)
    }
    object Bounded {
      def apply[L, U >: L, F[_ >: L <: U], A >: L <: U: Type](value: F[A]): Bounded[L, U, F] =
        new Impl[L, U, F, A](implicitly[Type[A]], value) // implicitly instead of Type[A] to avoid initialization issues
    }

    type LowerBounded[L, F[_ >: L]] = Existential.Bounded[L, Any, F]
    object LowerBounded {
      def apply[L, F[_ >: L], A >: L](value: F[A])(implicit A: Type[A]): LowerBounded[L, F] =
        Existential.Bounded[L, Any, F, A](value)
    }

    type UpperBounded[U, F[_ <: U]] = Existential.Bounded[Nothing, U, F]
    object UpperBounded {
      def apply[U, F[_ <: U], A <: U](value: F[A])(implicit A: Type[A]): UpperBounded[U, F] =
        Existential.Bounded[Nothing, U, F, A](value)
    }

    private class Impl[L, U >: L, F[_ >: L <: U], A >: L <: U](
        val Underlying: Type[A],
        val value: F[A]
    ) extends Existential.Bounded[L, U, F] {
      type Underlying = A
    }
  }
}
