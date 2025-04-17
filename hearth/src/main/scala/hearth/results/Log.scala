package hearth.results

sealed trait Log extends Product with Serializable
object Log {
  def info(message: => String): MacroIO[Unit] = MacroIO.lift(MacroResult.log(Entry(Level.Info, () => message)))
  def warn(message: => String): MacroIO[Unit] = MacroIO.lift(MacroResult.log(Entry(Level.Warn, () => message)))
  def error(message: => String): MacroIO[Unit] = MacroIO.lift(MacroResult.log(Entry(Level.Error, () => message)))

  def namedScope[A](name: String)(io: MacroIO[A]): MacroIO[A] = MacroIO.nameLogsScope(name, io)

  sealed trait Level extends Product with Serializable
  object Level {
    case object Info extends Level
    case object Warn extends Level
    case object Error extends Level
  }

  final case class Entry(level: Level, message: () => String) extends Log

  final case class Scope(name: String, entries: Logs) extends Log
}
