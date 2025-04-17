package hearth
package fp
package effect

/** Structured log stored as data. Can be turned into messages for macro reporters before exiting the macro. */
sealed trait Log extends Product with Serializable
object Log {
  def info(message: => String): MIO[Unit] = MIO.log(Entry(Level.Info, () => message))
  def warn(message: => String): MIO[Unit] = MIO.log(Entry(Level.Warn, () => message))
  def error(message: => String): MIO[Unit] = MIO.log(Entry(Level.Error, () => message))

  def namedScope[A](name: String)(io: MIO[A]): MIO[A] = MIO.nameLogsScope(name, io)

  sealed trait Level extends Product with Serializable
  object Level {
    case object Info extends Level
    case object Warn extends Level
    case object Error extends Level
  }

  final case class Entry(level: Level, message: () => String) extends Log

  final case class Scope(name: String, entries: Logs) extends Log
}
