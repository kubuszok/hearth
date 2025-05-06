package hearth
package fp
package effect

/** Stack-safe evaluation for recursive and nested computations.
  *
  * Does NOT handle: errors, exceptions, side effects, logging,etc. For that use [[MIO]].
  */
sealed trait MEval[A] { fa =>
  import MEval.*

  // ----------------------------------------------- Monadic operations -----------------------------------------------

  final def flatMap[B](f: A => MEval[B]): MEval[B] = Impure(fa, f)
  final def flatten[B](implicit ev: A <:< MEval[B]): MEval[B] = flatMap(ev)
  final def flatTap[B](f: A => MEval[B]): MEval[A] = flatMap(a => f(a).as(a))

  final def map[B](f: A => B): MEval[B] = flatMap(a => Pure(f(a)))
  final def mapTap[B](f: A => B): MEval[A] = map { a =>
    ignore(f(a)); a
  }

  final def map2[B, C](fb: => MEval[B])(f: (A, B) => C): MEval[C] = flatMap(a => fb.map(b => f(a, b)))
  final def tuple[B](fb: => MEval[B]): MEval[(A, B)] = map2(fb)((a, b) => (a, b))

  final def as[B](b: B): MEval[B] = map(_ => b)
  final def void: MEval[Unit] = as(())

  final def >>[B](fb: => MEval[B]): MEval[B] = flatMap(_ => fb)
  final def *>[B](fb: => MEval[B]): MEval[A] = map2(fb)((a, _) => a)
  final def <*[B](fb: => MEval[B]): MEval[B] = map2(fb)((_, b) => b)

  // --------------------------------------------------- Utilities ----------------------------------------------------

  final def run: A = MEval.run(fa)
}
object MEval {

  def pure[A](value: A): MEval[A] = Pure(value)
  def void: MEval[Unit] = pure(())

  def apply[A](value: => A): MEval[A] = defer(pure(value))
  def defer[A](thunk: => MEval[A]): MEval[A] = void.flatMap(_ => thunk)

  // --------------------------------------------- Implementation details ---------------------------------------------

  final private case class Pure[A](value: A) extends MEval[A]
  final private case class Impure[B, A](eval: MEval[B], f: B => MEval[A]) extends MEval[A]

  private def run[A](eval: MEval[A]): A = eval match {
    case Pure(value)                 => value
    case Impure(Pure(value), f)      => run(f(value))
    case Impure(Impure(eval, f), f2) => run(Impure(eval, f andThen (_.flatMap(f2))))
  }

  implicit val MEvalApplicative: Applicative[MEval] = new Applicative[MEval] {

    def pure[A](value: A): MEval[A] = MEval.pure(value)
    def map2[A, B, C](fa: MEval[A], fb: => MEval[B])(f: (A, B) => C): MEval[C] = fa.map2(fb)(f)
  }
}
