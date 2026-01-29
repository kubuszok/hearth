package hearth
package examples

object opaqueid {

  opaque type OpaqueId = Long

  object OpaqueId {
    def apply(v: Long): OpaqueId = v

    def fromString(s: String): Either[String, OpaqueId] =
      scala.util.Try(s.toLong).toEither.left.map(_.getMessage).map(apply)
  }

  extension (id: OpaqueId) {
    def value: Long = id
  }
}

/** Example opaque type with only [[CtorLikeOf.PlainValue]] smart constructor. */
object plainctor {

  opaque type PlainCtorOpaque = String

  object PlainCtorOpaque {
    def apply(v: String): PlainCtorOpaque = v
  }

  extension (o: PlainCtorOpaque) {
    def value: String = o
  }
}

/** Example opaque type with [[CtorLikeOf.EitherStringOrValue]] smart constructor. */
object eitherstringctor {

  opaque type EitherStringCtorOpaque = Int

  object EitherStringCtorOpaque {
    def parse(s: String): Either[String, EitherStringCtorOpaque] =
      scala.util.Try(s.toInt).toEither.left.map(_.getMessage)
  }

  extension (o: EitherStringCtorOpaque) {
    def value: Int = o
  }
}

/** Example opaque type with [[CtorLikeOf.EitherIterableStringOrValue]] smart constructor. */
object eitheriterablestringctor {

  opaque type EitherIterableStringCtorOpaque = Int

  object EitherIterableStringCtorOpaque {
    def validate(v: Int): Either[Iterable[String], EitherIterableStringCtorOpaque] =
      if v >= 0 then Right(v)
      else Left(List("must be non-negative", s"got: $v"))
  }

  extension (o: EitherIterableStringCtorOpaque) {
    def value: Int = o
  }
}

/** Example opaque type with [[CtorLikeOf.EitherThrowableOrValue]] smart constructor. */
object eitherthrowablector {

  opaque type EitherThrowableCtorOpaque = Double

  object EitherThrowableCtorOpaque {
    def safeDivide(v: Double): Either[Throwable, EitherThrowableCtorOpaque] =
      if v != 0 then Right(1.0 / v)
      else Left(new ArithmeticException("division by zero"))
  }

  extension (o: EitherThrowableCtorOpaque) {
    def value: Double = o
  }
}

/** Example opaque type with [[CtorLikeOf.EitherIterableThrowableOrValue]] smart constructor. */
object eitheriterablethrowablector {

  opaque type EitherIterableThrowableCtorOpaque = String

  object EitherIterableThrowableCtorOpaque {
    def validateAll(s: String): Either[Iterable[Throwable], EitherIterableThrowableCtorOpaque] = {
      val errors = List.newBuilder[Throwable]
      if s.isEmpty then errors += new IllegalArgumentException("must not be empty")
      if s.length > 10 then errors += new IllegalArgumentException("must not exceed 10 characters")
      val errs = errors.result()
      if errs.isEmpty then Right(s) else Left(errs)
    }
  }

  extension (o: EitherIterableThrowableCtorOpaque) {
    def value: String = o
  }
}
