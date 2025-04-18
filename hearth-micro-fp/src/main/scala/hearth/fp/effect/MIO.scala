package hearth
package fp
package effect

import scala.util.control.*

/** Macro IO - lazy result type for safe data composition in macros.
  *
  * Features:
  *   - stack-safety, it will not explode on deep nested computations
  *   - structural logging, you can build a log of results without the limitations of macros reporters (only first
  *     message is logged, each following call is a no-op)
  *   - storing errors as non-empty vector - you can store use "parallel" semantics and aggregate errors
  *   - catches non-fatal errors, so you don't need to handle them in the code
  *   - referential transparency, you can use your intuitions from Cats/Scalaz/etc
  *   - but without dependency on Cats/Scalaz/etc - no risk of conflicting versions in macros vs runtime
  *
  * It is synchronous, and does not support parallelism (Threads/Fibers) - parallel semantics is achieved by running
  * computations sequentially and combining results of several steps together.
  */
sealed trait MIO[+A] { fa =>
  import MIO.*

  // --------------------------------------- Transform both success and failure  --------------------------------------

  final def redeemWith[B](onSuccess: A => MIO[B])(onFailure: MErrors => MIO[B]): MIO[B] = :+ { (state, result) =>
    (try
      result.fold(onFailure, onSuccess)
    catch {
      case NonFatal(e) => fail(e)
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

  // ----------------------------------------------- Monadic operations -----------------------------------------------

  final def flatMap[B](f: A => MIO[B]): MIO[B] = redeemWith(f)(fail(_))
  final def flatten[B](implicit ev: A <:< MIO[B]): MIO[B] = flatMap(ev)
  final def flatTap[B](f: A => MIO[B]): MIO[A] = redeemWith(a => f(a).as(a))(fail(_))

  final def map[B](f: A => B): MIO[B] = redeemWith(f andThen pure)(fail(_))
  final def mapTap[B](f: A => B): MIO[A] = map { a =>
    val _ = f(a); a
  }

  final def map2[B, C](fb: => MIO[B])(f: (A, B) => C): MIO[C] = flatMap(a => fb.map(b => f(a, b)))
  final def tuple[B](fb: => MIO[B]): MIO[(A, B)] = map2(fb)((a, b) => (a, b))

  final def as[B](b: B): MIO[B] = redeemWith(_ => pure(b))(fail(_))
  final def void: MIO[Unit] = as(())

  final def >>[B](fb: => MIO[B]): MIO[B] = flatMap(_ => fb)
  final def *>[B](fb: => MIO[B]): MIO[A] = map2(fb)((a, _) => a)
  final def <*[B](fb: => MIO[B]): MIO[B] = map2(fb)((_, b) => b)
  final def <*>[B](fb: => MIO[B]): MIO[(A, B)] = tuple(fb)

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

  // ---------------------------------------------- Parallel operations -----------------------------------------------

  final def parMap2[B, C](fb: => MIO[B])(f: (A, B) => C): MIO[C] = redeemWith { a =>
    fb.map(b => f(a, b))
  } { e =>
    fb.redeemWith(_ => fail(e))(eb => fail(e ++ eb))
  }
  final def parTuple[B](fb: => MIO[B]): MIO[(A, B)] = parMap2(fb)((a, b) => (a, b))

  final def orElse[A1 >: A](fb: => MIO[A1]): MIO[A1] = redeemWith[A1](pure) { e1 =>
    fb.redeemWith(pure)(e2 => fail(e1 ++ e2))
  }

  // --------------------------------------------------- Utilities ----------------------------------------------------

  object log {

    final def info(message: => String): MIO[A] = flatTap(_ => Log.info(message))
    final def warn(message: => String): MIO[A] = flatTap(_ => Log.warn(message))
    final def error(message: => String): MIO[A] = flatTap(_ => Log.error(message))

    final def resultAsInfo(message: A => String): MIO[A] = flatTap(a => Log.info(message(a)))
    final def resultAsWarn(message: A => String): MIO[A] = flatTap(a => Log.warn(message(a)))
    final def resultAsError(message: A => String): MIO[A] = flatTap(a => Log.error(message(a)))
  }

  object unsafe {

    final def runSync: (MState, MResult[A]) = MIO.run(fa)
  }

  // --------------------------------------------- Implementation details ---------------------------------------------

  /** Extracted because pattern matching to use ++ inlined did not type-check for some reason. */
  protected def :++[B](f: FnNec[A, B]): MIO[B]
  final protected def :+[B](f: (MState, MResult[A]) => MIO[B]): MIO[B] = this :++ FnNec(f)
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

  final private[effect] case class Pure[A](state: MState, result: MResult[A]) extends MIO[A] {

    override protected def :++[B](q: FnNec[A, B]): MIO[B] = MIO.Impure(state, result, q)
  }
  final private[effect] case class Impure[A, B](state: MState, fa: MResult[A], qab: FnNec[A, B]) extends MIO[B] {

    override protected def :++[C](q: FnNec[B, C]): MIO[C] = MIO.Impure(state, fa, qab ++ q)
  }

  private[effect] def log(log: Log): MIO[Unit] = void :+ ((s, r) => Pure(s.log(log), r))
  private[effect] def nameLogsScope[A](name: String, io: MIO[A]): MIO[A] =
    io :+ ((s, r) => Pure(s.nameLogsScope(name), r))

  @scala.annotation.tailrec
  private def run[A](io: MIO[A]): (MState, MResult[A]) = io match {
    case Pure(state, result)    => state -> result
    case Impure(state, fa, ftc) => run(runStep(state, fa, ftc))
  }

  @scala.annotation.tailrec
  private def runStep[A, B](state: MState, result: MResult[A], ftc: FnNec[A, B]): MIO[B] = ftc.view match {
    case View.One(f) => f(state, result)
    case View.Cons(f, fcb) =>
      f(state, result) match {
        case Pure(state2, c) => runStep(state2, c, fcb)
        case impure          => impure :++ fcb
      }
  }

  implicit final val MioDirectStyle: fp.DirectStyle[MIO] = new fp.DirectStyle[MIO] {

    final private case class PassErrors(errors: MErrors, owner: Any) extends ControlThrowable with NoStackTrace

    private val openedAwaits = ThreadLocal.withInitial { () =>
      scala.collection.mutable.Queue.empty[Any]
    }
    private val ongoingStates = ThreadLocal.withInitial { () =>
      scala.collection.mutable.Map.empty[Any, MState]
    }
    private def openAwait: Any = {
      val owner = new AnyRef
      openedAwaits.get() += owner
      val _ = ongoingStates.get().put(owner, MState.empty)
      owner
    }
    private def closeAwait(owner: Any): Unit = {
      // Scala 2.12 complains if there is more than one _ in the same scope -_-"
      locally {
        val _ = openedAwaits.get().dequeue()
      }
      locally {
        val _ = ongoingStates.get().remove(owner)
      }
    }

    private def getCurrentOwner: Any =
      openedAwaits.get().headOption.getOrElse(throw new IllegalStateException("No open await!"))
    private def getCurrentState(owner: Any): MState =
      ongoingStates.get()(owner)
    private def appendCurrentState(owner: Any, state: MState): Unit = {
      val currentState = ongoingStates.get()(owner)
      val newState = currentState ++ state
      val _ = ongoingStates.get().put(owner, newState)
    }

    def asyncUnsafe[A](thunk: => A): MIO[A] = {
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

    def awaitUnsafe[A](mio: MIO[A]): A = {
      val owner = getCurrentOwner
      val (status, result) = run(mio)
      appendCurrentState(owner, status) // Whether it's a success or failure, we have to append state.
      result match {
        case Right(a) => a
        case Left(e)  => throw PassErrors(e, owner)
      }
    }
  }

  implicit final val MioParallelTraverse: fp.ParallelTraverse[MIO] = new fp.ParallelTraverse[MIO] {
    // Members declared in hearth.fp.Applicative
    def pure[A](a: A): MIO[A] = MIO.pure(a)
    def map2[A, B, C](fa: MIO[A], fb: => MIO[B])(f: (A, B) => C): MIO[C] = fa.map2(fb)(f)

    // Members declared in hearth.fp.Parallel
    def parMap2[A, B, C](fa: MIO[A], fb: => MIO[B])(f: (A, B) => C): MIO[C] = fa.parMap2(fb)(f)

    // It seems that we cannot run it in the "safe" way, because we have to run the effects.
    // Perhaps we should remove the Traverse?

    // Members declared in hearth.fp.Traverse
    def traverse[G[_], A, B](fa: MIO[A])(f: A => G[B])(implicit G: fp.Applicative[G]): G[MIO[B]] = {
      val (state, result) = run(fa)
      result match {
        case Right(a) => G.map(f(a))(b => Pure(state, Right(b)))
        case Left(e)  => G.pure(Pure(state, Left(e)))
      }
    }
    def parTraverse[G[_], A, B](fa: MIO[A])(f: A => G[B])(implicit G: fp.Parallel[G]): G[MIO[B]] = {
      val (state, result) = run(fa)
      result match {
        case Right(a) => G.map(f(a))(b => Pure(state, Right(b)))
        case Left(e)  => G.pure(Pure(state, Left(e)))
      }
    }
  }
}

// ---------------------------------------------- Implementation details ----------------------------------------------

/** (Specialized) Fast Type-aligned Constant-time queue (FTC Queue), it's basically non-empty chain of functions. */
sealed private[effect] trait FnNec[-A, +B] {
  final def :+[C](f: (MState, MResult[B]) => MIO[C]): FnNec[A, C] = FnNec.Node(this, FnNec(f))
  final def ++[C](fbc: FnNec[B, C]): FnNec[A, C] = FnNec.Node(this, fbc)

  /** View of FTC Queue exposing the head in a stack-safe way. */
  final def view: View[A, B] = this match {
    case FnNec.Leaf(f)        => View.One(f)
    case FnNec.Node(fab, fbc) => rewrite(fab, fbc)
  }

  @scala.annotation.tailrec
  private def rewrite[X, Y, Z](qxy: FnNec[X, Y], qyz: FnNec[Y, Z]): View[X, Z] = qxy match {
    case FnNec.Leaf(f)        => View.Cons(f, qyz)
    case FnNec.Node(qxv, qvy) => rewrite(qxv, FnNec.Node(qvy, qyz))
  }
}
private[effect] object FnNec {
  def apply[A, B](f: (MState, MResult[A]) => MIO[B]): FnNec[A, B] = Leaf(f)

  final case class Leaf[A, B](f: (MState, MResult[A]) => MIO[B]) extends FnNec[A, B]
  final case class Node[A, B, C](fab: FnNec[A, B], fbc: FnNec[B, C]) extends FnNec[A, C]
}

/** View of FTC Queue exposing the head in a stack-safe way. */
sealed private[effect] trait View[-A, +B]
private[effect] object View {
  final case class One[A, B](f: (MState, MResult[A]) => MIO[B]) extends View[A, B]
  final case class Cons[A, B, C](f: (MState, MResult[A]) => MIO[B], q: FnNec[B, C]) extends View[A, C]
}
