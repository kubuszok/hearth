package hearth
package fp
package effect

import scala.util.control.*
import hearth.fp.data.NonEmptyVector

/** Macro IO - lazy result type for safe data transformations in macros.
  *
  * Features:
  *   - stack-safety - it will not explode on deep-nested computations
  *   - structural logging - you can build a structured [[Log]] of results without the limitations of macros reporters
  *     (where only the first message is logged, and each following call is a no-op)
  *   - storing errors within a non-empty vector ([[MErrors]]) - you can use "parallel" semantics and aggregate errors
  *   - catching non-fatal errors - you don't need to worry that accidental exception will make the bug hard to find
  *   - referential transparency - MIO value not used, is computation not executed; when you see how values are
  *     composed, you don't need to look inside to tell if `val` vs `def` makes a differece. You can reuse your
  *     intuitions from Cats/Scalaz/etc, including IOLocal's counter-part ([[MLocal]])
  *   - but without dependencies on Cats/Scalaz/etc - no risk of conflicting versions in macros vs runtime
  *
  * It is synchronous, and does not support true parallelism (Threads/Fibers) - the "parallel" semantics (as Cats would
  * call it) of methods starting with "par" is achieved by running computations independently, from the same initial
  * state, and then combining their results.
  *
  * It is named "MIO", because you can reuse your intuitions from Cats/Scalaz/ZIO/etc, but there is no reason to
  * actually run any serious side-effects in macros, and no reason to use an actual async/parallel execution, though the
  * model mighy be useful for designing how to combine results of a few independent ("parellel") computations.
  *
  * ==Execution model: JVM-only, by design==
  *
  * MIO (and the rest of `hearth.fp.effect`) is '''macro-time''' machinery. A macro always '''runs on the JVM that hosts
  * the compiler''', even when the code being compiled targets Scala.js or Scala Native - the macro expands on the JVM
  * and only its ''output'' is then linked for the target platform. Accordingly, MIO's stack-safety relies on
  * `DirectStyleExecutor`, which uses `java.util.concurrent` and (when available) JDK 17+ virtual threads. These are
  * JVM-only APIs that are '''deliberately not, and never will be, ported''' to Scala.js / Scala Native.
  *
  * Consequence for usage: MIO is meant to be used '''only from inside macros'''. Referencing it from ordinary
  * (non-macro) Scala.js / Scala Native runtime code will fail to ''link'' (missing JVM intrinsics) - this is intended,
  * not a defect.
  *
  * Consequence for testing: the effect specs (`MIO`, `MLocal`, `MEval`, `MState`, the `NonEmpty*` data, etc.) live
  * under `hearth-tests/src/test/scalajvm` '''on purpose''' and must stay there. Making them cross-platform would not
  * increase meaningful coverage (the exercised code only ever executes on the JVM) and would instead break linking on
  * Scala.js / Scala Native, turning the whole suite red. The JVM toolchain used to expand macros for a Scala.js /
  * Native build is the same one the JVM-only specs run against, so the JVM-only specs already cover the cross-platform
  * macro case. ''If a future coverage audit flags "fp/effect tested only on JVM" as a gap, this is the answer: it is a
  * permanent design constraint, not a gap.''
  *
  * Example using [[MLocal]] and [[Log]]:
  *
  * {{{
  * // Defines a mutable reference. We can create it, put into some "globally available" val and refer to it everywhere
  * // in our program definition.
  * // It's not shown in this example, but the difference to a gloval var is, that it allow us to handle mutating
  * // it inside `parMap2`/`parTuple` as if they were different "fibers" with separate copies of this "var", where each
  * // can modify it independently. After "joining" them, we can define how these 2 values should be reconciled.
  * val counter = MLocal(initial = 0, fork = i => i + 1, join = (a, b) => a max b)
  *
  * // This is just a recipe for computation, it's not executed yet.
  * // In this recepe we are reading the current value of the counter, and logging it to 3 different levels.
  * val printSth = for {
  *   i <- counter.get
  *   _ <- Log.info("Print info: counter is now $i")
  *   _ <- Log.warn("Print warning: counter is now $i")
  *   _ <- Log.error("Print error: counter is now $i")
  * } yield 1
  *
  * // We can use the recipe above, to build more complex computations.
  * // Since MIO[Int] is _not_ a computed value, but a recipe for computation, we can reuse multiple times,
  * // and each time the program would run as if we copy-pasted its content: counder will be modified again,
  * // logs will be added again, etc.
  * val printNested = for {
  *   x <- Log.namedScope("Scope 1") {
  *     for {
  *       i <- counter.get
  *       _ <- counter.set(i + 1)
  *       _ <- printSth
  *     } yield i
  *   }
  *   y <- Log.namedScope("Scope 2") {
  *     for {
  *       i <- counter.get
  *       _ <- counter.set(i + 1)
  *       _ <- printSth
  *     } yield i
  *   }
  * } yield x + y
  *
  * // Here, we're finally running the computation. We're obtaining:
  * // - state: MState - contains the final state of all the logs and locals
  * // - result: Either[MErrors, Int] - contains the final result of the computation
  * // Usually, we would do it only once, right before existing the macro, to return the final Expr and/or report
  * // diagnostics and errors.
  * val (state, result) = printNested.unsafe.runSync
  *
  * // We can render the logs, with various levels:
  * println(state.logs.render.fromInfo("Info logs"))
  * println(state.logs.render.fromWarn("Warn logs"))
  * println(state.logs.render.fromError("Error logs"))
  * // There values could be used to build the single String that we would pass to macro reporters (only the first
  * // message of each level is shown in the result of the macro expansion, and the rest is discarded).
  *
  * // The result of our whole program. Usually we would be building some Expr[A], so this would be
  * // Either[MErrors, Expr[A]]. Here we would usually return expr from the Right value or report errors from the Left.
  * println(result)
  * }}}
  *
  * The example above calls `unsafe.runSync` for illustration; in a real macro you would '''not''' run the `MIO`
  * yourself — return it and let [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] run it exactly once, at the top
  * level of the expansion.
  *
  * @see
  *   [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] for the single, top-level entry point that runs an `MIO`
  *
  * @since 0.1.0
  */
sealed trait MIO[+A] { fa =>
  import MIO.*

  // --------------------------------------- Transform both success and failure  --------------------------------------

  /** Handles both outcomes of this `MIO` in one step: on success runs `onSuccess`, on failure runs `onFailure`.
    *
    * This is the primitive that the rest of the combinators (`flatMap`, `map`, `orElse`, `handleErrorWith`, …) delegate
    * to. It also catches [[scala.util.control.NonFatal]] thrown while evaluating `onSuccess`/`onFailure`, turning it
    * into an [[MErrors]] failure (logged via [[Log]]).
    *
    * @since 0.1.0
    *
    * @param onSuccess
    *   continuation applied to the success value
    * @param onFailure
    *   continuation applied to the accumulated [[MErrors]]
    * @return
    *   the `MIO[B]` produced by whichever branch ran
    */
  final def redeemWith[B](onSuccess: A => MIO[B])(onFailure: MErrors => MIO[B]): MIO[B] = :+ { (state, result) =>
    (try
      result.fold(onFailure, onSuccess)
    catch {
      case NonFatal(e) => fail(e).log.error(s"Caught exception ${e.getMessage}")
    }) match {
      case Pure(state2, fb)      => Pure(state ++ state2, fb)
      case Impure(state2, fb, q) => Impure(state ++ state2, fb, q)
    }
  }
  final def redeem[B](onSuccess: A => B)(onFailure: MErrors => B): MIO[B] =
    redeemWith(onSuccess andThen pure)(onFailure andThen pure)

  // ------------------------------------------ Transform to and from MResult -----------------------------------------

  final def attempt: MIO[MResult[A]] = this :+ { (s, r) => Pure(s, Right(r)) }
  final def unattempt[B](implicit ev: A <:< MResult[B]): MIO[B] = flatMap { a =>
    ev(a).fold(fail(_), pure)
  }

  final def attemptFlatTap[B](f: MResult[A] => MIO[B]): MIO[A] = attempt.flatMap(r => f(r) >> MIO.lift(r))
  final def attemptTap[B](f: MResult[A] => B): MIO[A] = attempt.flatMap { r =>
    ignore(f(r)); MIO.lift(r)
  }

  // ----------------------------------------------- Monadic operations -----------------------------------------------

  /** Sequences another `MIO`-producing step after this one succeeds.
    *
    * This is the safe way to use a value that lives inside an `MIO`: instead of starting a nested run to reach it, stay
    * inside the one program — `flatMap` the inner `MIO`, use its value, and return a new `MIO`. For the direct-style
    * alternative, see [[MIO.scoped]].
    *
    * @see
    *   [[MIO.scoped]] for the direct-style alternative to reach a value inside an `MIO`
    *
    * @since 0.1.0
    *
    * @param f
    *   continuation producing the next `MIO` from this one's success value
    */
  final def flatMap[B](f: A => MIO[B]): MIO[B] = redeemWith(f)(fail(_))
  final def flatten[B](implicit ev: A <:< MIO[B]): MIO[B] = flatMap(ev)
  final def flatTap[B](f: A => MIO[B]): MIO[A] = redeemWith(a => f(a).as(a))(fail(_))

  final def map[B](f: A => B): MIO[B] = redeemWith(f andThen pure)(fail(_))
  final def mapTap[B](f: A => B): MIO[A] = map { a =>
    ignore(f(a)); a
  }

  final def map2[B, C](fb: => MIO[B])(f: (A, B) => C): MIO[C] = flatMap(a => fb.map(b => f(a, b)))
  final def tuple[B](fb: => MIO[B]): MIO[(A, B)] = map2(fb)((a, b) => (a, b))

  final def as[B](b: B): MIO[B] = redeemWith(_ => pure(b))(fail(_))
  final def void: MIO[Unit] = as(())

  final def >>[B](fb: => MIO[B]): MIO[B] = flatMap(_ => fb)
  final def <*[B](fb: => MIO[B]): MIO[A] = map2(fb)((a, _) => a)
  final def *>[B](fb: => MIO[B]): MIO[B] = map2(fb)((_, b) => b)

  // --------------------------------------------- Monad error operations ---------------------------------------------

  final def handleErrorWith[A1 >: A](f: MErrors => MIO[A1]): MIO[A1] = redeemWith[A1](pure)(f)
  final def handleError[A1 >: A](f: MErrors => A1): MIO[A1] = redeemWith[A1](pure)(f andThen pure)
  final def recoverWith[A1 >: A](f: PartialFunction[MErrors, MIO[A1]]): MIO[A1] = redeemWith[A1](pure) {
    case e if f.isDefinedAt(e) => f(e)
    case e                     => fail(e)
  }
  final def recover[A1 >: A](f: PartialFunction[MErrors, A1]): MIO[A1] = redeemWith[A1](pure) {
    case e if f.isDefinedAt(e) => pure(f(e))
    case e                     => fail(e)
  }

  /** Falls back to `fb` if this `MIO` fails.
    *
    * If `fb` also fails, the two error vectors are '''aggregated''' (`e1 ++ e2`) rather than replaced — so the failure
    * reported by `orElse` carries the errors of both attempts, not just the last one.
    *
    * @since 0.1.0
    *
    * @param fb
    *   the fallback `MIO` to run if this one fails
    * @return
    *   this `MIO`'s success, or `fb`'s result, or a failure aggregating both error vectors
    */
  final def orElse[A1 >: A](fb: => MIO[A1]): MIO[A1] = redeemWith[A1](pure) { e1 =>
    fb.redeemWith(pure)(e2 => fail(e1 ++ e2))
  }

  // ---------------------------------------------- Parallel operations -----------------------------------------------

  /** Combines two `MIO`s with "parallel" semantics: both branches run from the same forked initial state and their
    * results (and their accumulated errors) are merged afterwards.
    *
    * "Parallel" is a loaded word here: this is NOT real threads/fibers. Execution is sequential, with fork/join
    * [[MState]] reconciliation (this is where [[MLocal]] fork/join runs). The key difference from `map2` is error
    * handling: if BOTH branches fail, their error vectors are aggregated (`e1 ++ e2`), whereas `map2` short-circuits on
    * the first failure.
    *
    * @see
    *   the class-doc note on the "parallel" semantics and [[MLocal]] for the fork/join reconciliation
    *
    * @since 0.1.0
    *
    * @param fb
    *   the second `MIO`, run from the same forked state as this one
    * @param f
    *   combines the two success values
    */
  final def parMap2[B, C](fb: => MIO[B])(f: (A, B) => C): MIO[C] =
    MIO.void :+ { (previousState, _) =>
      def faForked = Pure(previousState.fork(explicitlyRewind = MState.empty), Right(())) >> fa
      def fbForked(faState: MState) = Pure(previousState.fork(explicitlyRewind = faState), Right(())) >> fb

      faForked :+ { (stateA, resultA) =>
        defer(fbForked(stateA)) :+ { (stateB, resultB) =>
          val stateC = stateA join stateB // This join instead of ++ makes the difference in state management!
          try {
            val resultC = (resultA, resultB) match { // It is also important that we merge _after_ we computed results.
              case (Right(a), Right(b)) => Right(f(a, b)) // <-- this can throw!
              case (Left(e), Right(_))  => Left(e)
              case (Right(_), Left(e))  => Left(e)
              case (Left(e1), Left(e2)) => Left(e1 ++ e2)
            }
            Pure(stateC, resultC)
          } catch {
            case NonFatal(e) => Pure(stateC, Left(NonEmptyVector.one(e))).log.error(s"Caught exception ${e.getMessage}")
          }
        }
      }
    }

  final def parTuple[B](fb: => MIO[B]): MIO[(A, B)] = parMap2(fb)((a, b) => (a, b))

  final def <&[B](fb: => MIO[B]): MIO[A] = parMap2(fb)((a, _) => a)
  final def &>[B](fb: => MIO[B]): MIO[B] = parMap2(fb)((_, b) => b)

  // --------------------------------------------------- Utilities ----------------------------------------------------

  /** Structured logging attached to this value: each method adds a [[Log]] entry and passes the value/errors/result
    * through unchanged.
    *
    * @see
    *   [[Log]] for the journal these entries are appended to
    *
    * @since 0.1.0
    */
  object log {

    /** Logs `message` at info level and passes this value through unchanged.
      *
      * @since 0.1.0
      */
    final def info(message: => String): MIO[A] = valueAsInfo(_ => message)

    /** Logs `message` at warn level and passes this value through unchanged.
      *
      * @since 0.1.0
      */
    final def warn(message: => String): MIO[A] = valueAsWarn(_ => message)

    /** Logs `message` at error level and passes this value through unchanged.
      *
      * @since 0.1.0
      */
    final def error(message: => String): MIO[A] = valueAsError(_ => message)

    /** Logs an info/warn/error message derived from this success value, passing the value through unchanged.
      *
      * @since 0.1.0
      */
    final def valueAsInfo(message: A => String): MIO[A] = flatTap(a => Log.info(message(a)))
    final def valueAsWarn(message: A => String): MIO[A] = flatTap(a => Log.warn(message(a)))
    final def valueAsError(message: A => String): MIO[A] = flatTap(a => Log.error(message(a)))

    /** Logs an info/warn/error message derived from the accumulated [[MErrors]], then re-fails with them.
      *
      * @since 0.1.0
      */
    final def errorsAsInfo(message: MErrors => String): MIO[A] = handleErrorWith(e => Log.info(message(e)) >> fail(e))
    final def errorsAsWarn(message: MErrors => String): MIO[A] = handleErrorWith(e => Log.warn(message(e)) >> fail(e))
    final def errorsAsError(message: MErrors => String): MIO[A] = handleErrorWith(e => Log.error(message(e)) >> fail(e))

    /** Logs an info/warn/error message derived from the [[MResult]] (success or failure), passing the result through.
      *
      * @since 0.1.0
      */
    final def resultAsInfo(message: MResult[A] => String): MIO[A] = attemptFlatTap(r => Log.info(message(r)))
    final def resultAsWarn(message: MResult[A] => String): MIO[A] = attemptFlatTap(r => Log.warn(message(r)))
    final def resultAsError(message: MResult[A] => String): MIO[A] = attemptFlatTap(r => Log.error(message(r)))
  }

  /** Low-level escape hatches that run an `MIO` directly.
    *
    * These are '''internal''': `runSync` sets up NONE of the top-level machinery — no timeout deadline, no benchmarking
    * / flame-graph capture, no error aggregation or journal rendering — and threads NO state between nested runs (each
    * `runSync` starts from `MState.empty`). Those settings are global and are installed exactly once, at the edge of
    * the macro, by [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] (the single, top-level entry point). Compose
    * nested derivations with `flatMap`/[[hearth.fp.DirectStyle]] ([[MIO.scoped]]) inside one `MIO` rather than
    * re-running here; using `unsafe` is at your own risk, and it must never be nested inside another run. Violating
    * this is API misuse, not a bug.
    *
    * @see
    *   [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] for the entry point that installs the global machinery
    *
    * @since 0.1.0
    */
  object unsafe {

    final def runSync: (MState, MResult[A]) = MIO.run(fa)
  }

  // --------------------------------------------- Implementation details ---------------------------------------------

  /** Extracted because pattern matching to use ++ inlined did not type-check for some reason. */
  protected def :++[B](f: FnNec[A, B]): MIO[B]
  final protected def :+[B](f: (MState, MResult[A]) => MIO[B]): MIO[B] = this :++ FnNec(f)
}
object MIO {

  /** Lifts an already-computed value into a successful `MIO`.
    *
    * @since 0.1.0
    */
  def pure[A](a: A): MIO[A] = lift(MResult.pure(a))

  /** Creates a failed `MIO` from one or more errors (aggregated into an [[MErrors]]).
    *
    * @since 0.1.0
    */
  def fail[A](head: Throwable, tail: Throwable*): MIO[A] = lift(MResult.fail(head, tail*))

  /** Creates a failed `MIO` from an already-built [[MErrors]] vector.
    *
    * @since 0.1.0
    */
  def fail[A](errs: MErrors): MIO[A] = lift(MResult.fail(errs))
  def void: MIO[Unit] = lift(MResult.void)

  /** Lazily suspends a thunk into an `MIO`, evaluating it only when the program runs.
    *
    * [[scala.util.control.NonFatal]] thrown by `thunk` is caught and turned into a failure; [[MioTimeoutException]] is
    * deliberately re-thrown so a timeout is not swallowed.
    *
    * @since 0.1.0
    *
    * @param thunk
    *   the by-name computation to suspend
    */
  def apply[A](thunk: => A): MIO[A] = defer(pure(thunk))

  /** Lazily suspends a thunk that itself produces an `MIO`, deferring its evaluation until the program runs.
    *
    * @since 0.1.0
    *
    * @param thunk
    *   the by-name `MIO`-producing computation to suspend
    */
  def defer[A](thunk: => MIO[A]): MIO[A] = Impure(
    MState.empty,
    MResult.void,
    FnNec[Unit, A]((_, _) =>
      try
        thunk
      catch {
        case e: MioTimeoutException => throw e
        case NonFatal(e)            => fail(e)
      }
    )
  )

  /** Lazily lifts an eager [[MResult]] (success or failure) into an `MIO`.
    *
    * @since 0.1.0
    *
    * @param thunk
    *   the by-name [[MResult]] to suspend
    */
  def suspend[A](thunk: => MResult[A]): MIO[A] = defer(Pure(MState.empty, thunk))

  def firstOf[A](head: MIO[A], tail: MIO[A]*): MIO[A] = tail.foldLeft(head)(_.orElse(_))

  /** Direct-style block for `MIO`: inside `runSafe => …` you can extract the value of an `MIO[X]` as an `X` (via
    * `runSafe(mio)`), while its errors and logs are still captured into the surrounding `MIO`.
    *
    * Together with [[MIO.flatMap]] this is the recommended way to reach a value that lives inside an `MIO` WITHOUT
    * starting a nested run: pull the value out with `runSafe`, do your work, and let the result re-wrap into the one
    * surrounding `MIO`. Do NOT open a nested [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] run to reach into it
    * — that would violate the single-top-level-run contract.
    *
    * Caution: like all of [[hearth.fp.DirectStyle]], this is not guaranteed stack-safe for arbitrary nesting — see the
    * [[hearth.fp.DirectStyle]] class doc.
    *
    * @see
    *   [[hearth.fp.DirectStyle]] for the underlying direct-style mechanism and its stack-safety caveat
    * @see
    *   [[MIO.flatMap]] for the combinator alternative to reach a value inside an `MIO`
    * @see
    *   the "DirectStyle" section of https://scala-hearth.readthedocs.io/en/stable/micro-fp/#directstyle
    *
    * @since 0.1.0
    *
    * @param runSafe
    *   direct-style body; call `runSafe(mio)` to extract an `MIO[X]`'s value as an `X`
    */
  def scoped[A](runSafe: DirectStyle.RunSafe[MIO] => A): MIO[A] = DirectStyleForMio.scoped(runSafe)

  /** Whether each Log.scoped should also benchmark the scope duration. */
  var benchmarkScopes: Boolean = false

  /** When `true`, [[log]] and [[nameLogsScope]] become no-ops, skipping the per-entry / per-scope allocations.
    *
    * UNSAFE: while set, no `Log` entries or scopes are recorded, so any later log rendering is empty. Only enable when
    * all logs are guaranteed to be discarded. Set (save/restore) via
    * [[hearth.Environments.EnvironmentModule.withMioLoggingDisabled]], which `runToExprOrFail` turns on automatically
    * when every rendering knob is `DontRender`.
    *
    * @since 0.4.1
    */
  var disableLogging: Boolean = false

  /** Reference timestamp captured at the start of macro expansion. Used to convert absolute nanoTime values to
    * relative-to-start values for flame graph rendering.
    */
  var macroStartTimestamp: Log.Timestamp = Log.Timestamp.empty

  /** Deadline in nanoseconds (from System.nanoTime). [[Long.MaxValue]] means no timeout is active.
    *
    * Set via [[hearth.Environments.EnvironmentModule.withMioTimeout]] or directly before calling
    * [[MIO.unsafe.runSync]].
    *
    * @since 0.3.0
    */
  var timeoutDeadlineNanos: Long = Long.MaxValue

  /** Tracks currently-open [[Log.namedScope]] calls so that scope nesting can be reconstructed when a timeout fires.
    *
    * With the flat-log-with-scope-IDs design, open scopes already have correct parent IDs in the log list. The
    * `_openScopes` stack is used during timeout reconstruction to close open scopes (set their end timestamps).
    */
  private[effect] case class OpenScope(name: String, scopeId: Int, start: Log.Timestamp)
  private[effect] var _openScopes: List[OpenScope] = Nil

  // --------------------------------------------- Implementation details ---------------------------------------------

  private def lift[A](result: MResult[A]): MIO[A] = defer(Pure(MState.empty, result))

  /** [[MIO]] that does not define any more work to do. */
  final private[effect] case class Pure[A](state: MState, result: MResult[A]) extends MIO[A] {

    override protected def :++[B](q: FnNec[A, B]): MIO[B] = MIO.Impure(state, result, q)
  }

  /** [[MIO]] that defines some non-empty chain of functions to be applied. */
  final private[effect] case class Impure[A, B](state: MState, result: MResult[A], qab: FnNec[A, B]) extends MIO[B] {

    override protected def :++[C](q: FnNec[B, C]): MIO[C] = MIO.Impure(state, result, qab ++ q)
  }

  // ------------------------------------------- MLocal delegates to these --------------------------------------------

  private[effect] def get[A](local: MLocal[A]): MIO[A] = void :+ {
    case (s, Right(_)) => Pure(s, Right(s.get(local)))
    case (s, Left(e))  => Pure(s, Left(e))
  }
  private[effect] def set[A](local: MLocal[A], a: A): MIO[Unit] = void :+ {
    case (s, Right(_)) => Pure(s.set(local, a), Right(()))
    case (s, Left(e))  => Pure(s, Left(e))
  }

  // --------------------------------------------- Log delegates to these ---------------------------------------------

  private[effect] def log(log: => Log): MIO[Unit] =
    if (disableLogging) void else void :+ ((s, r) => Pure(s.log(log), r))
  private[effect] def nameLogsScope[A](name: String, io: MIO[A]): MIO[A] =
    if (disableLogging) io
    else
      void :+ { (s, _) =>
        val start = if (benchmarkScopes) Log.Timestamp.now else Log.Timestamp.empty
        val (s1, scopeId) = s.openScope(name, start)
        _openScopes = OpenScope(name, scopeId, start) :: _openScopes
        Pure(s1, Right(scopeId))
      } flatMap { scopeId =>
        io :+ { (s, r) =>
          _openScopes = if (_openScopes.nonEmpty) _openScopes.tail else Nil
          val end = if (benchmarkScopes) Log.Timestamp.now else Log.Timestamp.empty
          Pure(s.closeScope(scopeId, end), r)
        }
      }

  /** Allows the [[run]] loop to sync its accumulated state with [[DirectStyle]]'s `ongoingStates`, so that nested
    * `runSafe` calls see up-to-date state and their changes are propagated back into the run loop.
    *
    * Without this, the run loop accumulates state internally while `runSafe` tracks state in a separate mutable map.
    * When a nested `runSafe` is called from within a thunk evaluated by the run loop (e.g. `MIO(expr)` where `expr`
    * calls `runSafe(innerMio)`), the inner call starts from stale state and its changes are lost.
    */
  private trait StateSync {
    def write(state: MState): Unit
    def read(): MState
  }

  /** Stack-safety execution of MIO program.
    *
    * For each [[Impure]], [[FnNec.view]] rewrites a non-empty chain of functions e.g.
    * `(((MResult[A] -> MIO[B], MResult[B] -> MIO[C]), MResult[C] -> MIO[D]), MResult[D] -> MIO[E])` (skipped [[MState]]
    * in inputs for brevity), so that the left-most function is not nested and can be accessed directly, e.g.
    *   - `(((MResult[A] -> MIO[B], MResult[B] -> MIO[C]), MResult[C] -> MIO[D]), MResult[D] -> MIO[E])` into
    *   - `((MResult[A] -> MIO[B], MResult[B] -> MIO[C]), (MResult[C] -> MIO[D], MResult[D] -> MIO[E]))` into
    *   - `(MResult[A] -> MIO[B], (MResult[B] -> MIO[C], (MResult[C] -> MIO[D], MResult[D] -> MIO[E])))`
    *
    * Once rewritten, we can safely advance the computation: apply the left-most function, combine the NEC of functions
    * from returned MIO with the NEC that we already have, and pass newly computed state and result as the input for the
    * next iteration.
    *
    * Then we can repeat that process, until we're left with [[Pure]] MIO (with no more functions to run), when the
    * program is fully executed.
    *
    * Since both [[FnNec.view]] and [[run]]ning the left-most function are tail-recursive, and all applied functions (in
    * practice: [[MIO.redeemWith]], [[MIO.orElse]] and [[MIO.parMap2]], everything else delegate to them) should compute
    * only 1 level of nesting (more levels should be lazily deferred), the whole process is stack-safe.
    *
    * @param stateSync
    *   optional sync mechanism for DirectStyle. When non-null, the run loop writes its accumulated state before
    *   evaluating each function (so nested `runSafe` starts from the correct state), and reads back after (to
    *   incorporate any changes made by nested `runSafe`).
    */
  @scala.annotation.tailrec
  private def run[A](io: MIO[A], stateSync: StateSync = null): (MState, MResult[A]) = io match {
    case Impure(state, resultC, fnNecCToA) =>
      checkTermination(state, resultC, fnNecCToA)
      // Sync accumulated state to DirectStyle before evaluating functions, so nested runSafe sees it.
      if (stateSync ne null) stateSync.write(state)
      val nextMio = fnNecCToA.view match {
        case FnNec.View.One(fnCToA)             => fnCToA(state, resultC)
        case FnNec.View.Cons(fnCToB, fnNecBToA) => fnCToB(state, resultC) :++ fnNecBToA
      }
      // Read back from DirectStyle — nested runSafe may have modified the state.
      val baseState = if (stateSync ne null) stateSync.read() else state
      run(
        nextMio match {
          // Previous methods could have created new Pure or Impure without merging states, so we have to do it here.
          case Pure(state2, result)        => Pure(baseState ++ state2, result)
          case Impure(state2, result, qab) => Impure(baseState ++ state2, result, qab)
        },
        stateSync
      )
    case Pure(state, resultA) => state -> resultA
  }

  implicit final val DirectStyleForMio: fp.DirectStyle[MIO] = new fp.DirectStyle[MIO] {
    final private case class PassErrors(owner: Any, errors: MErrors) extends ControlThrowable with NoStackTrace

    private val ongoingStates = scala.collection.mutable.Map.empty[Any, MState]
    private def getState(owner: DirectStyle.ScopeOwner[MIO]): MState = ongoingStates(owner)
    private def setState(owner: DirectStyle.ScopeOwner[MIO], state: MState): Unit = ignore(
      ongoingStates.put(owner, state)
    )
    private def removeState(owner: DirectStyle.ScopeOwner[MIO]): Unit = ignore(ongoingStates.remove(owner))

    @scala.annotation.nowarn
    override protected def scopedUnsafe[A](owner: DirectStyle.ScopeOwner[MIO])(thunk: => A): MIO[A] = void :+ {
      case (initialState, Right(_)) =>
        // We're keeping the track of ownership because, there can be nested awaits.
        // And we have to consolidate state because there might be multiple awaits in the thunk.
        try {
          setState(owner, initialState)
          val a = thunk // We have to trigger side-effect before we'll extract current state
          val newState = getState(owner)
          Pure(newState, MResult.pure(a)) // There might have been no call to `await` in the thunk.
        } catch {
          case PassErrors(`owner`, errors) =>
            val newState = getState(owner) // There should be some state, otherwise it's a bug.
            Pure(newState, MResult.fail(errors))
        } finally
          removeState(owner)
      case (initialState, Left(e)) => Pure(initialState, Left(e))
    }
    override protected def runUnsafe[A](owner: DirectStyle.ScopeOwner[MIO])(mio: => MIO[A]): A = {
      // Sync mechanism: the run loop writes its accumulated state before each function evaluation, and reads back
      // after, so that nested runSafe calls see up-to-date state and their changes are incorporated into the run loop.
      val sync = new StateSync {
        def write(state: MState): Unit = setState(owner, state)
        def read(): MState = getState(owner)
      }

      // We're running the MIO in a virtual thread, to avoid StackOverflowError when using recursive MIO with direct style.
      val (computedState, result) = DirectStyleExecutor {
        // Evaluate the by-name MIO first — it may call nested runSafe which modifies ongoingStates(owner).
        // Then re-read the current state AFTER evaluation, so we start the run loop from up-to-date state.
        val evaluatedMio = mio
        val currentState = getState(owner)
        run(
          evaluatedMio match {
            case Pure(state, result)      => Pure(currentState ++ state, result)
            case Impure(state, result, q) => Impure(currentState ++ state, result, q)
          },
          sync
        )
      }

      setState(owner, computedState)

      result match {
        case Right(a) => a
        case Left(e)  => throw PassErrors(owner, e)
      }
    }
  }

  implicit final val ParallelForMio: fp.Parallel[MIO] = new fp.Parallel[MIO] {
    // Members declared in hearth.fp.Applicative
    override def pure[A](a: A): MIO[A] = MIO.pure(a)
    override def map2[A, B, C](fa: MIO[A], fb: => MIO[B])(f: (A, B) => C): MIO[C] = fa.map2(fb)(f)

    // Members declared in hearth.fp.Parallel
    override def parMap2[A, B, C](fa: MIO[A], fb: => MIO[B])(f: (A, B) => C): MIO[C] = fa.parMap2(fb)(f)
  }

  private def checkTermination[A, B](state: MState, result: MResult[A], ftc: FnNec[A, B]): Unit = {
    if (TerminationObserver.isTerminated) {
      throw MioTerminationException(state, result, ftc)
    }
    val deadline = timeoutDeadlineNanos
    if (deadline != Long.MaxValue && System.nanoTime() > deadline) {
      throw MioTimeoutException(state, result, ftc)
    }
  }

  /** We need to use something ignored by NonFatal. */
  final case class MioTerminationException(prettyPrintedMessage: String)
      extends java.lang.InterruptedException("\u001b\\[([0-9]+)m".r.replaceAllIn(prettyPrintedMessage, "")) {

    def prettyPrintedMessageWithStackTrace: String = {
      val stackTrace = getStackTrace.map(e => s"  ${Console.RED}at $e${Console.RESET}").mkString("\n")
      s"""$prettyPrintedMessage
         |
         |${Console.BLUE}Stack trace at the time of termination${Console.RESET}:
         |$stackTrace""".stripMargin
    }

    def prettyPrintMessageWithStackTrace(): Unit = java.lang.System.err.println(prettyPrintedMessageWithStackTrace)
  }
  object MioTerminationException {

    def apply[A, B](state: MState, result: MResult[A], ftc: FnNec[A, B]): MioTerminationException = {
      val locals =
        if (state.locals.isEmpty) s"  ${Console.BLUE}No MLocal values${Console.RESET}"
        else
          state.locals
            .map { case (local, value) =>
              s"    ${Console.BLUE}$local${Console.RESET}: ${Console.GREEN}$value${Console.RESET}"
            }
            .mkString(s"  ${Console.BLUE}MLocal values at the time of termination${Console.RESET}:\n", "\n", "")
      val logs =
        if (state.logs.isEmpty) s"  ${Console.BLUE}No Logs${Console.RESET}"
        else
          state.logs.render
            .fromInfo(s"${Console.BLUE}Logs at the time of termination${Console.RESET}")
            .split("\n")
            .map(l => s"  $l")
            .mkString("\n")
      val exceptionMsg = s"""${Console.BLUE}Terminated compilation${Console.RESET}:
                            |
                            |${Console.BLUE}MIO execution at the time of termination${Console.RESET}:
                            |  ${Console.BLUE}Last MResult${Console.RESET}:     ${Console.GREEN}$result${Console.RESET}
                            |  ${Console.BLUE}Next computation${Console.RESET}: ${Console.YELLOW}$ftc${Console.RESET}
                            |$locals
                            |$logs""".stripMargin
      new MioTerminationException(exceptionMsg)
    }
  }

  /** Thrown when MIO execution exceeds the configured [[timeoutDeadlineNanos]].
    *
    * Captures the full [[MState]] (with scope nesting reconstructed from [[_openScopes]]) so that flame graphs can
    * still be rendered from the partial execution.
    *
    * Extends [[InterruptedException]] so it is NOT caught by [[scala.util.control.NonFatal]] — this prevents MIO's own
    * `redeemWith`/`handleErrorWith` from swallowing the timeout.
    *
    * @since 0.3.0
    */
  final case class MioTimeoutException(capturedState: MState, prettyPrintedMessage: String)
      extends RuntimeException("\u001b\\[([0-9]+)m".r.replaceAllIn(prettyPrintedMessage, "")) {

    def prettyPrintedMessageWithStackTrace: String = {
      val stackTrace = getStackTrace.map(e => s"  ${Console.RED}at $e${Console.RESET}").mkString("\n")
      s"""$prettyPrintedMessage
         |
         |${Console.BLUE}Stack trace at the time of timeout${Console.RESET}:
         |$stackTrace""".stripMargin
    }

    def prettyPrintMessageWithStackTrace(): Unit = java.lang.System.err.println(prettyPrintedMessageWithStackTrace)
  }
  object MioTimeoutException {

    def apply[A, B](state: MState, result: MResult[A], ftc: FnNec[A, B]): MioTimeoutException = {
      val reconstructed = reconstructScopes(state)
      val locals =
        if (reconstructed.locals.isEmpty) s"  ${Console.BLUE}No MLocal values${Console.RESET}"
        else
          reconstructed.locals
            .map { case (local, value) =>
              s"    ${Console.BLUE}$local${Console.RESET}: ${Console.GREEN}$value${Console.RESET}"
            }
            .mkString(s"  ${Console.BLUE}MLocal values at the time of timeout${Console.RESET}:\n", "\n", "")
      val logs =
        if (reconstructed.logs.isEmpty) s"  ${Console.BLUE}No Logs${Console.RESET}"
        else
          reconstructed.logs.render
            .fromInfo(s"${Console.BLUE}Logs at the time of timeout${Console.RESET}")
            .split("\n")
            .map(l => s"  $l")
            .mkString("\n")
      val exceptionMsg = s"""${Console.BLUE}MIO timed out${Console.RESET}:
                            |
                            |${Console.BLUE}MIO execution at the time of timeout${Console.RESET}:
                            |  ${Console.BLUE}Last MResult${Console.RESET}:     ${Console.GREEN}$result${Console.RESET}
                            |  ${Console.BLUE}Next computation${Console.RESET}: ${Console.YELLOW}$ftc${Console.RESET}
                            |$locals
                            |$logs""".stripMargin
      new MioTimeoutException(reconstructed, exceptionMsg)
    }

    /** Close open scopes and add timeout marker.
      *
      * With flat-log-with-scope-IDs, open scopes already have correct parent IDs. We just need to:
      *   1. Add a timeout marker log entry
      *   2. Close all open scopes (set end timestamps, add to closedScopes)
      */
    private def reconstructScopes(state: MState): MState = {
      val now = Log.Timestamp.now
      // Add timeout marker with correct parent scope ID
      val parentId = state.scopeStack match {
        case (_, id) :: _ => id
        case Nil          => 0
      }
      var result = state.copy(logs = state.logs :+ Log.Entry(Log.Level.Error, () => "MIO timed out", parentId))
      // Close all open scopes (innermost first, which is the list order)
      for (OpenScope(_, scopeId, _) <- _openScopes)
        result = result.closeScope(scopeId, now)
      _openScopes = Nil
      result
    }
  }
}

// ---------------------------------------------- Implementation details ----------------------------------------------

/** (Specialized) Fast Type-aligned Constant-time queue (FTC Queue), it's basically a non-empty chain of functions. */
sealed private[effect] trait FnNec[-A, +B] {
  final def :+[C](f: (MState, MResult[B]) => MIO[C]): FnNec[A, C] = FnNec.Node(this, FnNec(f))
  final def ++[C](fbc: FnNec[B, C]): FnNec[A, C] = FnNec.Node(this, fbc)

  /** View of FTC Queue exposing the head in a stack-safe way. */
  final def view: FnNec.View[A, B] = this match {
    case FnNec.Leaf(f)        => FnNec.View.One(f)
    case FnNec.Node(fab, fbc) => rewrite(fab, fbc)
  }

  /** Recursively rewrite `((qxv, qvy), qyz)` into `(qxv, (qvy, qyz))` until the left-most function is not nested. */
  @scala.annotation.tailrec
  private def rewrite[X, Y, Z](qxy: FnNec[X, Y], qyz: FnNec[Y, Z]): FnNec.View[X, Z] = qxy match {
    case FnNec.Leaf(f)        => FnNec.View.Cons(f, qyz)
    case FnNec.Node(qxv, qvy) => rewrite(qxv, FnNec.Node(qvy, qyz))
  }
}
private[effect] object FnNec {
  def apply[A, B](f: (MState, MResult[A]) => MIO[B]): FnNec[A, B] = Leaf(f)

  final case class Leaf[A, B](f: (MState, MResult[A]) => MIO[B]) extends FnNec[A, B]
  final case class Node[A, B, C](fab: FnNec[A, B], fbc: FnNec[B, C]) extends FnNec[A, C]

  /** View of FTC Queue exposing the head in a stack-safe way. */
  sealed private[effect] trait View[-A, +B]
  private[effect] object View {
    final case class One[A, B](f: (MState, MResult[A]) => MIO[B]) extends View[A, B]
    final case class Cons[A, B, C](f: (MState, MResult[A]) => MIO[B], q: FnNec[B, C]) extends View[A, C]
  }
}
