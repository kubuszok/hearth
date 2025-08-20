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
    locals: Map[MLocal[?], Any],
    logs: Logs
) {

  // --------------------------------------------- Implementation details ---------------------------------------------

  private[effect] def ++(state: MState): MState =
    MState(appendLocals(locals, state.locals), combineLogs(logs, state.logs))

  private[effect] def fork: MState =
    MState(forkLocals(locals), logs)
  private[effect] def join(state: MState): MState =
    MState(joinLocals(locals, state.locals), combineLogs(logs, state.logs))

  private[effect] def get[A](local: MLocal[A]): A = locals.get(local) match {
    case Some(a) => a.asInstanceOf[A]
    case None    => local.initial
  }
  private[effect] def set[A](local: MLocal[A], a: A): MState = MState(locals + (local -> a), logs)

  private[effect] def log(log: Log): MState = MState(locals, logs :+ log)

  private[effect] def nameLogsScope(name: String, previous: MState): MState =
    MState(locals, recursiveNestedLogsMerge(previous.logs, logs, name))

  private def appendLocals(locals1: Map[MLocal[?], Any], locals2: Map[MLocal[?], Any]): Map[MLocal[?], Any] =
    locals1 ++ locals2

  private def forkLocals(locals: Map[MLocal[?], Any]): Map[MLocal[?], Any] =
    locals.map { case (local, a) => (local, local.asInstanceOf[MLocal[Any]].fork(a)) }.toMap

  private def joinLocals(locals1: Map[MLocal[?], Any], locals2: Map[MLocal[?], Any]): Map[MLocal[?], Any] =
    if (locals1 eq locals2) locals1
    else {
      (locals1.keySet ++ locals2.keySet)
        .asInstanceOf[Set[MLocal[Any]]]
        .map { local =>
          (locals1.get(local), locals2.get(local)) match {
            case (Some(a), Some(b)) => (local, local.join(a, b))
            case (Some(a), None)    => (local, a)
            case (None, Some(b))    => (local, b)
            case (None, None)       => (local, local.initial)
          }
        }
        .toMap
    }

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

      // It was to PITA properly fix this, so we just make sure here, that we don't repeat nested logs.
      result.reverse.collectFirst {
        case Log.Scope(name, nestedLogs) if nestedLogs.exists(result.contains) =>
          result.filterNot(nestedLogs.contains)
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
    // else if (newPrevious.isEmpty) Vector(Log.Scope(name, newCurrent))
    else common ++ recursiveNestedLogsMerge(newPrevious, newCurrent, name)
  }
}
object MState {

  val empty: MState = MState(Map.empty, logs = Vector.empty)
}
