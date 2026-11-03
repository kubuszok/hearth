package hearth
package examples
package crossquotesedge

import hearth.examples.kinds.Arity2

object SingletonObject

trait MarkerA
trait MarkerB

class Outer {
  class Inner
  class InnerGeneric[A]
}

object PathDependent {
  object first extends Outer
  object second extends Outer
}

/** Fixes the last argument of its parent, so the parent's arguments are not the subtype's own arguments. */
trait FixingSub[A] extends Arity2[A, Int]

/** Passes its own arguments to the parent in reversed order. */
trait SwappingSub[A, B] extends Arity2[B, A]

object aliases {
  type IntOption = Option[Int]
  type IntToString = Int => String
  type StringEither[A] = Either[String, A]
}

/** Type parameter bounded both ways, to check that `Bounded` constructors reject arguments outside of the bounds. */
trait Animal
trait Dog extends Animal
final class Puppy extends Dog
trait Kennel[A >: Puppy <: Animal]
