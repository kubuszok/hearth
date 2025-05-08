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
  */
sealed trait MIO[+A] { fa =>
  import MIO.*

  // --------------------------------------- Transform both success and failure  --------------------------------------

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
  final def *>[B](fb: => MIO[B]): MIO[A] = map2(fb)((a, _) => a)
  final def <*[B](fb: => MIO[B]): MIO[B] = map2(fb)((_, b) => b)

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

  final def orElse[A1 >: A](fb: => MIO[A1]): MIO[A1] = redeemWith[A1](pure) { e1 =>
    fb.redeemWith(pure)(e2 => fail(e1 ++ e2))
  }

  // ---------------------------------------------- Parallel operations -----------------------------------------------

  final def parMap2[B, C](fb: => MIO[B])(f: (A, B) => C): MIO[C] =
    fa.forked :+ { (stateA, resultA) =>
      defer(fb.forked) :+ { (stateB, resultB) =>
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

  final def parTuple[B](fb: => MIO[B]): MIO[(A, B)] = parMap2(fb)((a, b) => (a, b))

  final def &>[B](fb: => MIO[B]): MIO[A] = parMap2(fb)((a, _) => a)
  final def <&[B](fb: => MIO[B]): MIO[B] = parMap2(fb)((_, b) => b)

  // --------------------------------------------------- Utilities ----------------------------------------------------

  object log {

    final def info(message: => String): MIO[A] = valueAsInfo(_ => message)
    final def warn(message: => String): MIO[A] = valueAsWarn(_ => message)
    final def error(message: => String): MIO[A] = valueAsError(_ => message)

    final def valueAsInfo(message: A => String): MIO[A] = flatTap(a => Log.info(message(a)))
    final def valueAsWarn(message: A => String): MIO[A] = flatTap(a => Log.warn(message(a)))
    final def valueAsError(message: A => String): MIO[A] = flatTap(a => Log.error(message(a)))

    final def errorsAsInfo(message: MErrors => String): MIO[A] = handleErrorWith(e => Log.info(message(e)) >> fail(e))
    final def errorsAsWarn(message: MErrors => String): MIO[A] = handleErrorWith(e => Log.warn(message(e)) >> fail(e))
    final def errorsAsError(message: MErrors => String): MIO[A] = handleErrorWith(e => Log.error(message(e)) >> fail(e))

    final def resultAsInfo(message: MResult[A] => String): MIO[A] = attemptFlatTap(r => Log.info(message(r)))
    final def resultAsWarn(message: MResult[A] => String): MIO[A] = attemptFlatTap(r => Log.warn(message(r)))
    final def resultAsError(message: MResult[A] => String): MIO[A] = attemptFlatTap(r => Log.error(message(r)))
  }

  object unsafe {

    final def runSync: (MState, MResult[A]) = MIO.run(fa)
  }

  // --------------------------------------------- Implementation details ---------------------------------------------

  /** Extracted because pattern matching to use ++ inlined did not type-check for some reason. */
  protected def :++[B](f: FnNec[A, B]): MIO[B]
  final protected def :+[B](f: (MState, MResult[A]) => MIO[B]): MIO[B] = this :++ FnNec(f)

  final protected def forked: MIO[A] = fa :+ { (state, result) => Pure(state.fork, result) }
}
object MIO {

  def pure[A](a: A): MIO[A] = lift(MResult.pure(a))
  def fail[A](head: Throwable, tail: Throwable*): MIO[A] = lift(MResult.fail(head, tail*))
  def fail[A](errs: MErrors): MIO[A] = lift(MResult.fail(errs))
  def void: MIO[Unit] = lift(MResult.void)

  def apply[A](thunk: => A): MIO[A] = defer(pure(thunk))
  def defer[A](thunk: => MIO[A]): MIO[A] = Impure(
    MState.empty,
    MResult.void,
    FnNec[Unit, A]((_, _) =>
      try
        thunk
      catch {
        case NonFatal(e) => fail(e)
      }
    )
  )
  def suspend[A](thunk: => MResult[A]): MIO[A] = defer(Pure(MState.empty, thunk))

  def firstOf[A](head: MIO[A], tail: MIO[A]*): MIO[A] = tail.foldLeft(head)(_.orElse(_))

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

  private[effect] def get[A](local: MLocal[A]): MIO[A] = void :+ {
    case (s, Right(_)) => Pure(s, Right(s.get(local)))
    case (s, Left(e))  => Pure(s, Left(e))
  }
  private[effect] def set[A](local: MLocal[A], a: A): MIO[Unit] = void :+ {
    case (s, Right(_)) => Pure(s.set(local, a), Right(()))
    case (s, Left(e))  => Pure(s, Left(e))
  }

  private[effect] def log(log: Log): MIO[Unit] = void :+ ((s, r) => Pure(s.log(log), r))
  private[effect] def nameLogsScope[A](name: String, io: MIO[A]): MIO[A] =
    io :+ ((s, r) => Pure(s.nameLogsScope(name), r))

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
    */
  @scala.annotation.tailrec
  private def run[A](io: MIO[A]): (MState, MResult[A]) = io match {
    case Impure(state, resultC, fnNecCToA) =>
      checkTermination(state, resultC, fnNecCToA)
      run(fnNecCToA.view match {
        case FnNec.View.One(fnCToA)             => fnCToA(state, resultC)
        case FnNec.View.Cons(fnCToB, fnNecBToA) => fnCToB(state, resultC) :++ fnNecBToA
      })
    case Pure(state, resultA) => state -> resultA
  }

  final private case class PassErrors(errors: MErrors, owner: Any) extends ControlThrowable with NoStackTrace
  implicit final val MioDirectStyle: fp.DirectStyle[MIO] = new fp.DirectStyle[MIO] {

    private val openedAwaits = ThreadLocal.withInitial { () =>
      scala.collection.mutable.Queue.empty[Any]
    }
    private val ongoingStates = ThreadLocal.withInitial { () =>
      scala.collection.mutable.Map.empty[Any, MState]
    }
    private def openAwait: Any = {
      val owner = new AnyRef
      openedAwaits.get() += owner
      ignore(ongoingStates.get().put(owner, MState.empty))
      owner
    }
    private def closeAwait(owner: Any): Unit =
      ignore(openedAwaits.get().dequeue(), ongoingStates.get().remove(owner))

    private def getCurrentOwner: Any =
      openedAwaits.get().headOption.getOrElse(throw new IllegalStateException("No open await!"))
    private def getCurrentState(owner: Any): MState =
      ongoingStates.get()(owner)
    private def appendCurrentState(owner: Any, state: MState): Unit = {
      val currentState = ongoingStates.get()(owner)
      val newState = currentState ++ state
      ignore(ongoingStates.get().put(owner, newState))
    }

    override protected def asyncUnsafe[A](thunk: => A): MIO[A] = {
      // We're keeping the track of ownership because, there can be nested awaits.
      // And we have to consolidate state because there might be multiple awaits in the thunk.
      val owner = openAwait
      try {
        val a = thunk // We have to trigger side-effect before we'll extract current state
        Pure(getCurrentState(owner), MResult.pure(a))
      } catch {
        case PassErrors(errors, `owner`) =>
          Pure(getCurrentState(owner), MResult.fail(errors))
      } finally
        closeAwait(owner)
    }

    override protected def awaitUnsafe[A](mio: MIO[A]): A = {
      val owner = getCurrentOwner
      val (status, result) = run(mio)
      appendCurrentState(owner, status) // Whether it's a success or failure, we have to append state.
      result match {
        case Right(a) => a
        case Left(e)  => throw PassErrors(e, owner)
      }
    }
  }

  implicit final val MioParalle: fp.Parallel[MIO] = new fp.Parallel[MIO] {
    // Members declared in hearth.fp.Applicative
    def pure[A](a: A): MIO[A] = MIO.pure(a)
    def map2[A, B, C](fa: MIO[A], fb: => MIO[B])(f: (A, B) => C): MIO[C] = fa.map2(fb)(f)

    // Members declared in hearth.fp.Parallel
    def parMap2[A, B, C](fa: MIO[A], fb: => MIO[B])(f: (A, B) => C): MIO[C] = fa.parMap2(fb)(f)
  }

  private val terminated = {
    val handler = new java.util.concurrent.atomic.AtomicBoolean(false)
    java.lang.Runtime.getRuntime.addShutdownHook(new Thread(() => handler.set(true)))
    sun.misc.Signal.handle(new sun.misc.Signal("INT"), _ => handler.set(true))
    sun.misc.Signal.handle(new sun.misc.Signal("TERM"), _ => handler.set(true))
    handler
  }
  private def checkTermination[A, B](state: MState, result: MResult[A], ftc: FnNec[A, B]): Unit =
    if (Thread.interrupted() || terminated.get()) {
      val msg = s"""Terminated compilation:
                   |
                   |Last result:      $result
                   |Next computation: $ftc
                   |
                   |${state.logs.render.fromInfo("Logs at the time of termination")}""".stripMargin
      throw new scala.concurrent.CancellationException(msg)
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
