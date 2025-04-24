package hearth
package fp
package effect

/** State of the [[MIO]] computation - currently stores [[Logs]] and [[MLocal]]s. */
final case class MState private[effect] (
    locals: Map[MLocal[?], Any],
    logs: Logs
) {

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

  private[effect] def nameLogsScope(name: String): MState = MState(locals, Vector(Log.Scope(name, logs)))

  private def appendLocals(locals1: Map[MLocal[?], Any], locals2: Map[MLocal[?], Any]): Map[MLocal[?], Any] =
    locals1 ++ locals2

  private def forkLocals(locals: Map[MLocal[?], Any]): Map[MLocal[?], Any] =
    locals.map { case (local, a) => (local, local.asInstanceOf[MLocal[Any]].fork(a)) }.toMap

  private def joinLocals(locals1: Map[MLocal[?], Any], locals2: Map[MLocal[?], Any]): Map[MLocal[?], Any] =
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

  private def combineLogs(logs1: Logs, logs2: Logs): Logs = (logs1 ++ logs2).distinct
}
object MState {

  val empty: MState = MState(Map.empty, logs = Vector.empty)
}
