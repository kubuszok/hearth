package hearth.fp.effect

/** State of the [[MIO]] computation - currently stores only logs. */
final case class MState private[effect] (logs: Logs) {

  private[effect] def ++(state: MState): MState = MState((this.logs ++ state.logs).distinct)

  private[effect] def log(log: Log): MState = MState(logs :+ log)

  private[effect] def nameLogsScope(name: String): MState = MState(Vector(Log.Scope(name, logs)))
}
object MState {
  val empty: MState = MState(logs = Vector.empty)
}
