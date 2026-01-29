package hearth
package examples

final case class ExampleValueClass(a: Int) extends AnyVal

/** Example value class with smart constructor returning Either[String, A]. */
final case class ValidatedInt private (value: Int) extends AnyVal
object ValidatedInt {
  def parse(s: String): Either[String, ValidatedInt] =
    scala.util.Try(s.toInt).toEither.left.map(_.getMessage).map(ValidatedInt(_))
}

/** Example regular class with private constructor and smart constructor. */
final class PositiveInt private (val value: Int)
object PositiveInt {
  def apply(value: Int): Either[String, PositiveInt] =
    if (value > 0) Right(new PositiveInt(value))
    else Left(s"Value must be positive, got: $value")
}

/** Example class with multiple smart constructors of different types. */
final class Email private (val value: String)
object Email {
  def fromString(s: String): Either[String, Email] =
    if (s.contains("@")) Right(new Email(s))
    else Left("Invalid email: missing @")

  def unsafeFromString(s: String): Email =
    new Email(s)
}

/** Example case class with validation returning Iterable errors. */
final case class Username private (value: String)
object Username {
  def validate(s: String): Either[Iterable[String], Username] = {
    val errors = List.newBuilder[String]
    if (s.length < 3) errors += "Username must be at least 3 characters"
    if (s.length > 20) errors += "Username must be at most 20 characters"
    if (!s.matches("^[a-zA-Z0-9_]+$")) errors += "Username must contain only alphanumeric characters and underscores"
    val errs = errors.result()
    if (errs.isEmpty) Right(Username(s)) else Left(errs)
  }
}
