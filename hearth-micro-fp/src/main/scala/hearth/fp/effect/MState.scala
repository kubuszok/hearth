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

  private[effect] def ++(state: MState): MState =
    MState(appendLocals(locals, state.locals), combineLogs(logs, state.logs))

  private[effect] def fork: MState =
    MState(forkLocals(locals, MState.empty), logs)
  private[effect] def fork(explicitlyIgnore: MState): MState =
    MState(forkLocals(locals, explicitlyIgnore), logs)
  private[effect] def join(state: MState): MState =
    MState(joinLocals(locals, state.locals), combineLogs(logs, state.logs))

  private[effect] def get[A](local: MLocal[A]): A = locals.get(local) match {
    case Some(a) => a.as[A]
    case None    => local.initial
  }
  private[effect] def set[A](local: MLocal[A], a: A): MState = MState(locals + (local -> Value(a)), logs)

  private[effect] def log(log: Log): MState = MState(locals, logs :+ log)

  private[effect] def nameLogsScope(name: String, previous: MState): MState =
    MState(locals, recursiveNestedLogsMerge(previous.logs, logs, name))

  private def appendLocals(locals1: Map[MLocal[?], Value], locals2: Map[MLocal[?], Value]): Map[MLocal[?], Value] =
    if (locals1 eq locals2) locals1
    else {
      (locals1.keySet ++ locals2.keySet)
        .asInstanceOf[Set[MLocal[Any]]]
        .flatMap { local =>
          (locals1.get(local), locals2.get(local)) match {
            case (Some(a), Some(b)) => Iterator(local -> Ordering[Value].max(a, b))
            case (Some(a), None)    => Iterator(local -> a)
            case (None, Some(b))    => Iterator(local -> b)
            case (None, None)       => Iterator(local -> local.initial)
          }
        }
        .toMap
        .asInstanceOf[Map[MLocal[?], Value]]
    }

  private def forkLocals(locals: Map[MLocal[?], Value], explicitlyIgnore: MState): Map[MLocal[?], Value] =
    (locals.keySet ++ explicitlyIgnore.locals.keySet) // We do this so that None won't fall back on value from another computation.
      .asInstanceOf[Set[MLocal[Any]]]
      .map { local =>
        locals.get(local) match {
          case Some(a) => local -> Value(local.asInstanceOf[MLocal[Any]].fork(a.value))
          case None    => local -> Value(local.initial)
        }
      }
      .toMap

  private def joinLocals(locals1: Map[MLocal[?], Value], locals2: Map[MLocal[?], Value]): Map[MLocal[?], Value] =
    if (locals1 eq locals2) locals1
    else {
      (locals1.keySet ++ locals2.keySet)
        .asInstanceOf[Set[MLocal[Any]]]
        .map { local =>
          (locals1.get(local), locals2.get(local)) match {
            case (Some(a), Some(b)) => local -> Value(local.join(a.value, b.value))
            case (Some(a), None)    => local -> a
            case (None, Some(b))    => local -> b
            case (None, None)       => local -> Value(local.initial)
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

  val empty: MState = MState(Map.empty, logs = Vector.empty)

  final case class Value(value: Any, timestamp: Long = System.nanoTime()) {

    def as[A]: A = value.asInstanceOf[A]
  }

  object Value {

    implicit val ordering: Ordering[Value] = Ordering.by(_.timestamp)
  }
}
