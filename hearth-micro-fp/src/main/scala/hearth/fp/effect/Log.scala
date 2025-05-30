package hearth
package fp
package effect

import fp.instances.*
import fp.syntax.*

/** Structured log stored as data. Can be turned into messages for macro reporters before exiting the macro.
  *
  * Can be used together with [[MIO.log]] to build a log of messages without `println`s, or single-time macro reporters.
  */
sealed trait Log extends Product with Serializable
object Log {

  /** Logs an info message. */
  def info(message: => String): MIO[Unit] = MIO.log(Entry(Level.Info, () => message))

  /** Logs a warning message. */
  def warn(message: => String): MIO[Unit] = MIO.log(Entry(Level.Warn, () => message))

  /** Logs an error message. */
  def error(message: => String): MIO[Unit] = MIO.log(Entry(Level.Error, () => message))

  /** Creates a new scope with a given name. All logs created inside the scope nested under it. */
  def namedScope[A](name: String)(io: MIO[A]): MIO[A] = MIO.nameLogsScope(name, io)

  /** Logging levels - corresponds with reporting levels available in macro reporters. */
  sealed abstract class Level(private val value: Int) extends Product with Serializable {

    final def prefix: String = this match {
      case Level.Info  => "[Info]  "
      case Level.Warn  => "[Warn]  "
      case Level.Error => "[Error] "
    }
  }
  object Level {
    case object Info extends Level(0)
    case object Warn extends Level(1)
    case object Error extends Level(2)

    implicit val ordering: Ordering[Level] = Ordering.by(_.value)
  }

  /** Single log entry, with lazily computed message (messages can be expensive, and usually we don't display logs). */
  final case class Entry(level: Level, message: () => String) extends Log

  /** A group of logs with named scope (will be displayed with nesting) */
  final case class Scope(name: String, entries: Logs) extends Log

  // --------------------------------------------- Implementation details ---------------------------------------------

  /** Render structured [[Logs]] into a String, in a stack-safe way. */
  private[effect] def render(rootScopeName: String, logs: Logs)(filter: Level => Boolean): String =
    renderLevels(new StringBuilder, Vector(Item(Scope(rootScopeName, filterLevels(logs, filter).run), "", "")))

  /** Filter out arbitrarily large nested logs without stack overflow. */
  private def filterLevels(logs: Logs, filter: Level => Boolean): MEval[Logs] = MEval.defer {
    for {
      filteredLogs <- logs.traverse[MEval, Vector[Log]] {
        case entry: Entry if filter(entry.level) => MEval.pure(Vector(entry))
        case _: Entry                            => MEval.pure(Vector.empty[Log])
        case Scope(name, entries)                =>
          filterLevels(entries, filter).map {
            case filteredEntries if filteredEntries.nonEmpty => Vector(Scope(name, filteredEntries))
            case _                                           => Vector.empty[Log]
          }
      }
    } yield filteredLogs.flatten
  }

  /** Render arbitrarily large nested logs in a queue without stack overflow. */
  @scala.annotation.tailrec
  private def renderLevels(sb: StringBuilder, queue: Vector[Item]): String = queue match {
    case currentItem +: remaining => renderLevels(sb, currentItem.step(sb) ++ remaining)
    case _                        => sb.toString()
  }

  /** Render either a single log [[Entry]] or a nested [[Scope]] name, returning a queue of remaining items. */
  final private case class Item(log: Log, indentHead: String, indentTail: String) {

    def init(logs: Logs): Vector[Item] = logs.init.map(Item(_, indentTail + "├ ", indentTail + "│ "))
    def last(logs: Logs): Vector[Item] = Vector(Item(logs.last, indentTail + "└ ", indentTail + "  "))

    @scala.annotation.nowarn("msg=unused")
    def step(sb: StringBuilder): Vector[Item] = log match {
      case Entry(level, message) =>
        val msg = message().split("\n")
        sb.append(indentHead).append(level.prefix).append(msg.head).append("\n")
        msg.tail.foreach(line => sb.append(indentTail).append("        ").append(line).append("\n"))
        Vector.empty
      case Scope(name, entries) =>
        val msg = (name + ":").split("\n")
        sb.append(indentHead).append(msg.head).append("\n")
        msg.tail.foreach(line => sb.append(indentTail).append(line).append("\n"))

        entries.size match {
          case 0 => Vector.empty
          case 1 => last(entries)
          case _ => init(entries) ++ last(entries)
        }
    }
  }
}
