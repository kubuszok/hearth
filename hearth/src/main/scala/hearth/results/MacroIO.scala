package hearth.results

import scala.util.control.*

/** Result type for safe data composition in macros.
  *
  * Features:
  *   - stack-safety, it will not explode on deep nested computations
  *   - structural logging, you can build a log of results without the limitations of macros reporters (only first
  *     message is logged, each following call is a no-op)
  *   - storing errors as non-empty vector - you can store use "parallel" semantics and aggregate errors
  *   - catches non-fatal errors, so you don't need to handle them in the code
  *   - referential transparency, you can use your intuitions from Cats/Scalaz/etc
  *   - but without dependency on Cats/Scalaz/etc - no risk of conflicting versions in macros vs runtime
  */
sealed trait MacroIO[+A] {
  import MacroIO.*

  final def transform[B](onSuccess: A => MacroIO[B], onFailure: Errors => MacroIO[B]): MacroIO[B] =
    append(FnNec[A, B] { result =>
      val (io, state) =
        try
          result match {
            case MacroResult.Success(state, a) => onSuccess(a) -> state
            case MacroResult.Failure(state, e) => onFailure(e) -> state
          }
        catch {
          case NonFatal(e) => fail(e) -> result.state
        }
      io match {
        case Pure(fb)      => Pure(fb.prependState(state))
        case Impure(fb, q) => Impure(fb.prependState(state), q)
      }
    })
  final def attempt: MacroIO[Either[Errors, A]] = append(
    FnNec[A, Either[Errors, A]] {
      case MacroResult.Success(state, a) => Pure(MacroResult.Success(state, Right(a)))
      case MacroResult.Failure(state, e) => Pure(MacroResult.Success(state, Left(e)))
    }
  )

  final def flatMap[B](f: A => MacroIO[B]): MacroIO[B] = transform(f, fail(_))
  final def flatTap[B](f: A => MacroIO[B]): MacroIO[A] = transform(a => f(a).as(a), fail(_))
  final def map[B](f: A => B): MacroIO[B] = transform(f andThen pure, fail(_))
  final def as[B](b: B): MacroIO[B] = transform(_ => pure(b), fail(_))

  final def recoverWith[A1 >: A](f: Errors => MacroIO[A1]): MacroIO[A1] = transform(pure, f)
  final def recover[A1 >: A](f: Errors => A1): MacroIO[A1] = transform(pure, f andThen pure)

  def logInfo(message: => String): MacroIO[A] = flatTap(_ => Log.info(message))
  def logWarn(message: => String): MacroIO[A] = flatTap(_ => Log.warn(message))
  def logError(message: => String): MacroIO[A] = flatTap(_ => Log.error(message))

  final def run: MacroResult[A] = MacroIO.run(this)

  protected def append[B](f: FnNec[A, B]): MacroIO[B]
}
object MacroIO {

  def lift[A](a: MacroResult[A]): MacroIO[A] = Pure(a)

  def pure[A](a: A): MacroIO[A] = Pure(MacroResult.pure(a))
  def fail[A](head: Throwable, tail: Throwable*): MacroIO[A] = Pure(MacroResult.fail(head, tail*))
  def fail[A](errs: Errors): MacroIO[A] = Pure(MacroResult.fail(errs))

  def apply[A](thunk: => A): MacroIO[A] = try
    pure(thunk)
  catch {
    case NonFatal(e) => fail(e)
  }

  // Implementation details

  final private[results] case class Pure[A](fa: MacroResult[A]) extends MacroIO[A] {

    override protected def append[B](q: FnNec[A, B]): MacroIO[B] = MacroIO.Impure(fa, q)
  }
  final private[results] case class Impure[A, B](fa: MacroResult[A], qab: FnNec[A, B]) extends MacroIO[B] {

    override protected def append[C](q: FnNec[B, C]): MacroIO[C] = MacroIO.Impure(fa, qab ++ q)
  }

  private[results] def nameLogsScope[A](name: String, io: MacroIO[A]): MacroIO[A] = io.append(FnNec[A, A] { result =>
    Pure(result.nameLogsScope(name))
  })

  @scala.annotation.tailrec
  private def run[A](io: MacroIO[A]): MacroResult[A] = io match {
    case Pure(fa)        => fa
    case Impure(fa, ftc) => run(runStep(fa, ftc))
  }

  @scala.annotation.tailrec
  private def runStep[A, B](a: MacroResult[A], ftc: FnNec[A, B]): MacroIO[B] = ftc.view match {
    case View.One(f) => f(a)
    case View.Cons(f, fcb) =>
      f(a) match {
        case Pure(c) => runStep(c, fcb)
        case impure  => impure.append(fcb)
      }
  }
}

// Implementation details

/** Fast Type-aligned Constant-time queue (FTC Queue), but we can think it's non-empty chain of functions. */
sealed private[results] trait FnNec[-A, +B] {
  final def :+[C](f: MacroResult[B] => MacroIO[C]): FnNec[A, C] = FnNec.Node(this, FnNec(f))
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
private[results] object FnNec {
  def apply[A, B](f: MacroResult[A] => MacroIO[B]): FnNec[A, B] = Leaf(f)

  final case class Leaf[A, B](f: MacroResult[A] => MacroIO[B]) extends FnNec[A, B]
  final case class Node[A, B, C](fab: FnNec[A, B], fbc: FnNec[B, C]) extends FnNec[A, C]
}

/** View of FTC Queue exposing the head in a stack-safe way. */
sealed private[results] trait View[-A, +B]
private[results] object View {
  final case class One[A, B](f: MacroResult[A] => MacroIO[B]) extends View[A, B]
  final case class Cons[A, B, C](f: MacroResult[A] => MacroIO[B], q: FnNec[B, C]) extends View[A, C]
}
