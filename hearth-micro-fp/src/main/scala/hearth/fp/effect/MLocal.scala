package hearth
package fp
package effect

/** Handle allowing to store and retrieve some value inside [[MIO]] computations.
  *
  * It differs from using a global var in that:
  *   - separate runs of the same [[MIO]] program would have separate lifecycles of the local value (each would
  *     initialize it, then modify its own copy of the value)
  *   - when combining [[MIO]] programs sequentially, the value would be passed under the hood without us having to
  *     carry it around
  *   - when using thr "parallel" semantics ([[MIO.parMap2]], [[MIO.parTuple]]), we are able to modify value for each
  *     "fiber", and provide a reasonable way of combining values from 2 different "fibers" back into a single value
  */
final class MLocal[A] private (
    private[effect] val initial: A,
    private[effect] val fork: A => A,
    private[effect] val join: (A, A) => A
) {

  def get: MIO[A] = MIO.get(this)

  def set(a: A): MIO[Unit] = MIO.set(this, a)
}

object MLocal {

  /** Creates a new [[MLocal]] value - each value, even if initialized the same way! - would be a separate instance.
    *
    * @param initial
    *   the initial value of the local variable, each indifidual [[MIO.unsafe.runSync]] would start from it
    * @param fork
    *   the function that modifies the local value for each new "fiber" (before running [[MIO.parMap2]] or
    *   [[MIO.parTuple]])
    * @param join
    *   the function that is run after "fibers" are done (after [[MIO.parMap2]] or [[MIO.parTuple]]) to combine values
    *   from 2 different "fibers" back into a single value
    */
  def apply[A](initial: A)(fork: A => A)(join: (A, A) => A): MLocal[A] =
    new MLocal(initial, fork, join)
}
