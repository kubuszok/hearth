package hearth.results

sealed trait MacroResult[+A] {
  import MacroResult.*

  def state: State

  private[results] def appendState(state: State): MacroResult[A] = this match {
    case Success(s, a) => Success(s.append(state), a)
    case Failure(s, e) => Failure(s.append(state), e)
  }
  private[results] def prependState(state: State): MacroResult[A] = this match {
    case Success(s, a) => Success(s.prepend(state), a)
    case Failure(s, e) => Failure(s.prepend(state), e)
  }

  private[results] def nameLogsScope(name: String): MacroResult[A] = this match {
    case Success(State(logs), a) => Success(State(Vector(Log.Scope(name, logs))), a)
    case Failure(State(logs), e) => Failure(State(Vector(Log.Scope(name, logs))), e)
  }
}

object MacroResult {

  def pure[A](a: A): MacroResult[A] = Success(State(), a)
  def fail[A](head: Throwable, tail: Throwable*): MacroResult[A] = Failure(State(), NonEmptyVector(head, tail.toVector))
  def fail[A](errs: NonEmptyVector[Throwable]): MacroResult[A] = Failure(State(), errs)

  def state(state: State): MacroResult[Unit] = Success(state, ())

  def log(log: Log): MacroResult[Unit] = Success(State(logs = Vector(log)), ())

  final case class Success[+A](state: State, a: A) extends MacroResult[A]
  final case class Failure(state: State, e: NonEmptyVector[Throwable]) extends MacroResult[Nothing]

  final case class State private[results] (logs: Logs = Vector.empty) {

    def append(state: State): State = State((this.logs ++ state.logs).distinct)

    def prepend(state: State): State = state.append(this)
  }
}
