package hearth
package fp
package effect

import scala.collection.immutable.BitSet

/** State of the [[MIO]] computation - stores [[Logs]], [[MLocal]]s, and scope tracking.
  *
  * Logs are stored flat with parent scope IDs. Merging is O(1) (take the longer vector). Tree structure is
  * reconstructed at render time.
  *
  * @since 0.1.0
  */
final case class MState private[effect] (
    locals: Map[MLocal[?], MState.Value],
    logs: Logs,
    scopeStack: List[(String, Int)],
    closedScopes: BitSet
) {
  import MState.Value

  // ----------------------------------------------- API called by MIO  -----------------------------------------------

  private[effect] def ++(state: MState): MState =
    if (this eq MState.empty) state
    else if (state eq MState.empty) this
    else {
      // In sequential execution, state2.logs is always a superset of state1.logs (each step extends).
      // Take the longer vector — no dedup, no iteration, O(1).
      val mergedLogs = if (state.logs.size >= logs.size) state.logs else logs
      MState(appendLocals(locals, state.locals), mergedLogs, state.scopeStack, closedScopes | state.closedScopes)
    }

  private[effect] def fork(explicitlyRewind: MState): MState =
    MState(forkLocals(locals, explicitlyRewind), logs, scopeStack, closedScopes)
  private[effect] def join(state: MState): MState =
    if (this eq MState.empty) state
    else if (state eq MState.empty) this
    else {
      // For parallel branches: this = branchA result, state = branchB result.
      // Branch B was forked from branch A's result, so its logs start with A's logs.
      // Take the longer vector (which includes both branches' logs).
      val joinedLogs = if (state.logs.size >= logs.size) state.logs else logs
      MState(joinLocals(locals, state.locals), joinedLogs, state.scopeStack, closedScopes | state.closedScopes)
    }

  private[effect] def get[A](local: MLocal[A]): A = locals.get(local).fold(local.initial)(_.as[A])
  private[effect] def set[A](local: MLocal[A], a: A): MState = {
    val ver = local match {
      case fjp: MLocal.ForkJoinParallel[A @unchecked]     => fjp.nextVersion
      case sop: MLocal.SequentialOnParallel[A @unchecked] => sop.nextVersion
    }
    MState(locals + (local -> Value(a, ver)), logs, scopeStack, closedScopes)
  }

  private[effect] def log(log: Log): MState = {
    val parentId = scopeStack match {
      case (_, id) :: _ => id
      case Nil          => 0
    }
    val tagged = log match {
      case e: Log.Entry => e.copy(parentScopeId = parentId)
      case s: Log.Scope => s.copy(parentScopeId = parentId)
    }
    MState(locals, logs :+ tagged, scopeStack, closedScopes)
  }

  private[effect] def openScope(name: String, start: Log.Timestamp): (MState, Int) = {
    val id = Log.nextScopeId()
    val parentId = scopeStack match {
      case (_, pid) :: _ => pid
      case Nil           => 0
    }
    val scope = Log.Scope(name, id, parentId, start, Log.Timestamp.empty)
    (MState(locals, logs :+ scope, (name, id) :: scopeStack, closedScopes), id)
  }

  private[effect] def closeScope(scopeId: Int, end: Log.Timestamp): MState = {
    // Update the Scope's end timestamp by searching backwards (scopes close in reverse order)
    val updatedLogs =
      if (end == Log.Timestamp.empty) logs
      else {
        var i = logs.size - 1
        var found = false
        while (i >= 0 && !found)
          logs(i) match {
            case s: Log.Scope if s.scopeId == scopeId =>
              found = true
            case _ =>
              i -= 1
          }
        if (found) logs.updated(i, logs(i).asInstanceOf[Log.Scope].copy(end = end))
        else logs
      }
    MState(
      locals,
      updatedLogs,
      scopeStack.tail,
      closedScopes + scopeId
    )
  }

  // ----------------------------------------------- MLocal operations ------------------------------------------------

  private def nextVersionOf(local: MLocal[?]): Long = local match {
    case fjp: MLocal.ForkJoinParallel[?]     => fjp.nextVersion
    case sop: MLocal.SequentialOnParallel[?] => sop.nextVersion
  }

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
          local match {
            case _: MLocal.SequentialOnParallel[?] =>
              // Shared: take latest value without forking. Prefer explicitlyRewind (branch A's result).
              val fromRewind = explicitlyRewind.locals.get(local.asInstanceOf[MLocal[?]])
              val fromCurrent = locals.get(local.asInstanceOf[MLocal[?]])
              local -> (fromRewind orElse fromCurrent).getOrElse(Value(local.initial, nextVersionOf(local)))
            case fjp: MLocal.ForkJoinParallel[Any @unchecked] =>
              locals.get(local.asInstanceOf[MLocal[?]]) match {
                case Some(a) => local -> Value(fjp.fork(a.value), fjp.nextVersion)
                case None    =>
                  local -> Value(
                    local.initial,
                    fjp.nextVersion
                  ) // We do this so that None won't fall back on value from another computation.
              }
          }
        }.toMap
      }

  private def joinLocals(locals1: Map[MLocal[?], Value], locals2: Map[MLocal[?], Value]): Map[MLocal[?], Value] =
    if (locals1 eq locals2) locals1
    else
      handleLocalsCasting(locals1.keySet, locals2.keySet) {
        _.map { local =>
          (locals1.get(local), locals2.get(local)) match {
            case (Some(a), Some(b)) =>
              local match {
                case fjp: MLocal.ForkJoinParallel[Any @unchecked] =>
                  local -> Value(fjp.join(a.value, b.value), fjp.nextVersion)
                case _: MLocal.SequentialOnParallel[?] =>
                  // Sequential: always prefer the second argument (branch B ran after A, has latest state)
                  local -> b
              }
            case (Some(a), None) => local -> a
            case (None, Some(b)) => local -> b
            case (None, None)    => local -> Value(local.initial, nextVersionOf(local))
          }
        }.toMap
      }

  private def handleLocalsCasting(keys1: Set[MLocal[?]], keys2: Set[MLocal[?]])(
      f: Set[MLocal[Any]] => Map[MLocal[Any], Any]
  ): Map[MLocal[?], Value] = {
    val keys = (keys1 ++ keys2).asInstanceOf[Set[MLocal[Any]]]
    f(keys).asInstanceOf[Map[MLocal[?], Value]]
  }
}
object MState {

  // --------------------------------------------- Implementation details ---------------------------------------------

  val empty: MState = MState(Map.empty, logs = Vector.empty, scopeStack = Nil, closedScopes = BitSet.empty)

  final case class Value(value: Any, version: Long) {

    def as[A]: A = value.asInstanceOf[A]
  }

  object Value {

    implicit val ordering: Ordering[Value] = Ordering.by(_.version)
  }
}
