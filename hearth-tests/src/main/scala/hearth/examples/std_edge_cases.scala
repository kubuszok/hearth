package hearth
package examples

// $SCOVERAGE-OFF$ This file is analyzed mostly in the compile time.

/** Edge cases for the standard extensions (`IsCollection`, `IsMap`, `IsOption`, `IsEither`, `IsValueType`,
  * `CtorLikes`).
  */
object stdedge {

  type StringList = List[String]
  type IntKeyed[V] = Map[Int, V]
  type FlippedMap[V, K] = Map[K, V]
  type FlippedEither[R, L] = Either[L, R]
  type Maybe[A] = Option[A]
  type StringWrapperAlias = StringWrapper

  final case class StringWrapper(value: String) extends AnyVal
  final case class GenericWrapper[A](value: A) extends AnyVal

  final class PrivateCtorWrapper private (val value: String) extends AnyVal

  final case class NotAValueClass(value: String)

  final class PublicSingleArgCtor(val value: String) {
    override def toString: String = s"PublicSingleArgCtor($value)"
  }

  final class TwoArgCtor(val a: String, val b: Int)

  final class ThrowingCtor private (val value: Int)
  object ThrowingCtor {
    def attempt(value: Int): Either[Throwable, ThrowingCtor] =
      if (value > 0) Right(new ThrowingCtor(value)) else Left(new IllegalArgumentException("must be positive"))
  }

  final class AccumulatingThrowingCtor private (val value: Int)
  object AccumulatingThrowingCtor {
    def validateAll(value: Int): Either[Iterable[Throwable], AccumulatingThrowingCtor] =
      if (value > 0) Right(new AccumulatingThrowingCtor(value))
      else Left(List(new IllegalArgumentException("must be positive")))
  }
}
