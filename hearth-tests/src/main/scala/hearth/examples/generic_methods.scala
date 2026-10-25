package hearth
package examples
package genericmethods

// Test data for `Method` chains of GENERIC methods: the return type references the method's own type parameters, so it
// is known only after `ApplyTypes` (reported by kindlings: di-cats `Resource[F, A]` factories).

// Local stand-ins for cats-effect `Resource` / `Sync` (no cats dependency).
trait Res[F[_], A]
trait SyncLike[F[_]]
trait SomeF[A]
object SomeF {
  implicit val syncLike: SyncLike[SomeF] = new SyncLike[SomeF] {}
}
final class Config

final class Widget
object Widget {
  // type clause only: `Widget.resource[SomeF]` is fully applied after `ApplyTypes`.
  def resource[F[_]]: Res[F, Widget] = null
}

final class Service
object Service {
  // type clause, value clause, implicit (context-bound) clause.
  @scala.annotation.nowarn
  def resource[F[_]: SyncLike](config: Config): Res[F, Service] = null
}

final class Plain
object Plain {
  // NOT a `Res` - a macro must be able to reject it by its (substituted) result type.
  def resource[F[_]]: Option[Plain] = None
}

// Both the class's and the method's type parameters appear in the result type.
final class Holder[T] {
  def pair[B]: (T, B) = null
}
