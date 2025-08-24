package hearth
package fp
package effect

/** State of the [[MIO]] computation - currently stores [[Logs]] and [[MLocal]]s.
  *
  * Used by [[MIO]] to store inner state of the computation.
  *
  * @since 0.1.0
  */
final case class MState private[effect] (
    locals: Map[MLocal[?], MState.Value],
    logs: Logs
) {
  import MState.Value

  // --------------------------------------------- Implementation details ---------------------------------------------

  // The whole complexity of this class is due to 2 reasons:
  //
  // 1. MIO always stores `MState` next to `MResult` - it **significantly** simplifies the MIO.run implementation,
  //    but it also means that each MIO (including `MIO.Pure`) would introduce some state, whether user actually
  //    modified it or not. That forces us to detect whether the state was actually modified, and merging changes.
  // 2. the parallel semantics is only simulated - we need to store state before computation, process 1 "fiber", and
  //    then rewind the state before processing the next "fiber".
  //
  // In the sequential code, to resolve variables we can just pick the newest MLocal. Logs are a bit more complex:
  // `Log.namedScope` should remove some logs, and re-add them wrapped in a new scope - but when merging we could see the
  // same logs twice - once in Scope and once present as top level Entries, so we have to deduplicate them.
  //
  // In the parallel code, we have to rewind the state. BUT parallelism is still only simulated, we would still sequentially
  // merge states, and override old values with new ones. If there is a `MLocal` not used before forking, and used in
  // the first fork, it would always override the empty value of pre-fork state. So we have to force-initialize it with
  // the newer timestamp.

  // ----------------------------------------------- API called by MIO  -----------------------------------------------

  private[effect] def ++(state: MState): MState =
    MState(appendLocals(locals, state.locals), combineLogs(logs, state.logs))

  private[effect] def fork(explicitlyRewind: MState): MState =
    MState(forkLocals(locals, explicitlyRewind), logs)
  private[effect] def join(state: MState): MState =
    MState(joinLocals(locals, state.locals), combineLogs(logs, state.logs))

  private[effect] def get[A](local: MLocal[A]): A = locals.get(local).fold(local.initial)(_.as[A])
  private[effect] def set[A](local: MLocal[A], a: A): MState = MState(locals + (local -> Value(a)), logs)

  private[effect] def log(log: Log): MState = MState(locals, logs :+ log)
  private[effect] def nameLogsScope(name: String, previous: MState): MState =
    MState(locals, recursiveNestedLogsMerge(previous.logs, logs, name))

  // ----------------------------------------------- MLocal operations ------------------------------------------------

  private def appendLocals(locals1: Map[MLocal[?], Value], locals2: Map[MLocal[?], Value]): Map[MLocal[?], Value] =
    if (locals1 eq locals2) locals1
    else
      handleLocalsCasting(locals1.keySet, locals2.keySet) {
        _.flatMap { local =>
          (locals1.get(local), locals2.get(local)) match {
            case (Some(a), Some(b)) => Iterator(local -> Ordering[Value].max(a, b))
            case (Some(a), None)    => Iterator(local -> a)
            case (None, Some(b))    => Iterator(local -> b)
            case (None, None)       => Iterator(local -> local.initial)
          }
        }.toMap
      }

  private def forkLocals(locals: Map[MLocal[?], Value], explicitlyRewind: MState): Map[MLocal[?], Value] =
    if (explicitlyRewind eq MState.empty) locals
    else
      handleLocalsCasting(locals.keySet, explicitlyRewind.locals.keySet) {
        _.map { local =>
          locals.get(local) match {
            case Some(a) => local -> Value(local.asInstanceOf[MLocal[Any]].fork(a.value))
            case None    =>
              local -> Value(
                local.initial
              ) // We do this so that None won't fall back on value from another computation.
          }
        }.toMap
      }

  private def joinLocals(locals1: Map[MLocal[?], Value], locals2: Map[MLocal[?], Value]): Map[MLocal[?], Value] =
    if (locals1 eq locals2) locals1
    else
      handleLocalsCasting(locals1.keySet, locals2.keySet) {
        _.map { local =>
          (locals1.get(local), locals2.get(local)) match {
            case (Some(a), Some(b)) => local -> Value(local.join(a.value, b.value))
            case (Some(a), None)    => local -> a
            case (None, Some(b))    => local -> b
            case (None, None)       => local -> Value(local.initial)
          }
        }.toMap
      }

  private def handleLocalsCasting(keys1: Set[MLocal[?]], keys2: Set[MLocal[?]])(
      f: Set[MLocal[Any]] => Map[MLocal[Any], Any]
  ): Map[MLocal[?], Value] = {
    val keys = (keys1 ++ keys2).asInstanceOf[Set[MLocal[Any]]]
    f(keys).asInstanceOf[Map[MLocal[?], Value]]
  }

  // ------------------------------------------------ Logs operations -------------------------------------------------

  private def combineLogs(logs1: Logs, logs2: Logs): Logs =
    if ((logs1 eq logs2) || (logs2.isEmpty)) logs1
    else if (logs1.isEmpty) logs2
    else {
      def appendLastScopeToNew = logs2 match {
        case Vector(Log.Scope(name, nestedLogs)) if nestedLogs.exists(logs1.contains) =>
          Some(Vector(Log.Scope(name, recursiveNestedLogsMerge(logs1, nestedLogs, name))))
        case _ =>
          None
      }

      def justMerge = (logs1 ++ logs2).distinct

      val result = appendLastScopeToNew getOrElse justMerge

      // It was PITA trying to fix this properly, so instead we just make sure _here_, that we don't repeat nested logs.
      // We need to compate by reference to avoid removing _reused_ logs (e.g. same MIO put into multiple named scopes).
      result.reverse.collectFirst {
        case Log.Scope(name, nestedLogs) if nestedLogs.exists(l => result.exists(_ eq l)) =>
          result.filterNot(l => nestedLogs.exists(_ eq l))
      } getOrElse result
    }

  private def recursiveNestedLogsMerge(previous: Vector[Log], current: Vector[Log], name: String): Vector[Log] = {
    var foundInPrevous = false
    val common = previous.view
      .map {
        case Log.Scope(`name`, nestedPrevious) =>
          foundInPrevous = true
          Log.Scope(`name`, recursiveNestedLogsMerge(nestedPrevious, current, name))
        case otherwise => otherwise
      }
      .zip(current)
      .map {
        case (l1, l2) if l1 == l2                                 => Some(l1)
        case (Log.Scope(n1, ls1), Log.Scope(n2, ls2)) if n1 == n2 =>
          Some(Log.Scope(n1, recursiveNestedLogsMerge(ls1, ls2, n1)))
        case _ => None
      }
      .takeWhile(_.isDefined)
      .map(_.get)
      .toVector
    lazy val newPrevious = previous.drop(common.length)
    lazy val newCurrent = current.drop(common.length)
    if (foundInPrevous) common ++ newPrevious
    else if (common.isEmpty) newPrevious ++ Vector(Log.Scope(name, newCurrent))
    else common ++ recursiveNestedLogsMerge(newPrevious, newCurrent, name)
  }
}
object MState {

  // --------------------------------------------- Implementation details ---------------------------------------------

  val empty: MState = MState(Map.empty, logs = Vector.empty)

  final case class Value(value: Any, timestamp: Long = System.nanoTime()) {

    def as[A]: A = value.asInstanceOf[A]
  }

  object Value {

    implicit val ordering: Ordering[Value] = Ordering.by(_.timestamp)
  }
}
