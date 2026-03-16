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
  *   - when using the "parallel" semantics ([[MIO.parMap2]], [[MIO.parTuple]]), we are able to modify value for each
  *     "fiber", and provide a reasonable way of combining values from 2 different "fibers" back into a single value
  *
  * There are two variants:
  *   - [[MLocal.ForkJoinParallel]] — each parallel branch gets an independent copy via `fork`, results are combined
  *     with `join`
  *   - [[MLocal.SequentialOnParallel]] — parallel branches share the same state sequentially (branch B sees branch A's
  *     writes)
  *
  * @since 0.1.0
  */
sealed trait MLocal[A] extends Product with Serializable {

  private[effect] val initial: A

  def get: MIO[A] = MIO.get(this)

  def set(a: A): MIO[Unit] = MIO.set(this, a)
}

object MLocal {

  final private[effect] case class ForkJoinParallel[A](
      initial: A,
      fork: A => A,
      join: (A, A) => A
  ) extends MLocal[A] {
    private val version = new java.util.concurrent.atomic.AtomicLong(1L)
    private[effect] def nextVersion: Long = version.getAndIncrement()
  }

  final private[effect] case class SequentialOnParallel[A](
      initial: A
  ) extends MLocal[A] {
    private val version = new java.util.concurrent.atomic.AtomicLong(1L)
    private[effect] def nextVersion: Long = version.getAndIncrement()
  }

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
    *
    * @since 0.1.0
    */
  def apply[A](initial: A)(fork: A => A)(join: (A, A) => A): MLocal[A] =
    new ForkJoinParallel(initial, fork, join)

  /** Creates a new [[MLocal]] with shared-parallel semantics.
    *
    * Unlike the standard [[MLocal]], which forks state for each parallel branch and joins the results, a shared
    * [[MLocal]] passes the same state sequentially through all branches: branch B sees branch A's writes, branch C sees
    * both, etc.
    *
    * This is useful for caches and deduplication maps where parallel branches should see each other's entries rather
    * than independently building duplicate state.
    *
    * The `join` function is still required because it is used both by [[MIO.parMap2]] (to combine branch results) and
    * internally by [[MIO]]'s direct-style machinery. With shared-parallel fork semantics, branch B already includes
    * branch A's writes, so `join` typically just needs to combine any independently-added entries (e.g. a cache merge).
    *
    * '''Unsafe''' because it breaks branch independence — the second branch depends on the first branch's execution
    * order. Not suitable for state where fork isolation matters (error counters, independent accumulators, etc.).
    *
    * @since 0.3.0
    */
  def unsafeSharedParallel[A](initial: A): MLocal[A] =
    new SequentialOnParallel(initial)
}
