package hearth
package examples
package genericmethods

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
  def resource[F[_]]: Res[F, Widget] = null
}

final class Service
object Service {
  @scala.annotation.nowarn
  def resource[F[_]: SyncLike](config: Config): Res[F, Service] = null
}

final class Plain
object Plain {
  // NOT a `Res` - a macro must be able to reject it by its (substituted) result type.
  def resource[F[_]]: Option[Plain] = None
}

final class Holder[T] {
  def pair[B]: (T, B) = null
}
