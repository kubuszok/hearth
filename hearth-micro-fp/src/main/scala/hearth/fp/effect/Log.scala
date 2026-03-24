package hearth
package fp
package effect

/** Structured log stored as data. Can be turned into messages for macro reporters before exiting the macro.
  *
  * Can be used together with [[MIO.log]] to build a log of messages without `println`s, or single-time macro reporters.
  *
  * Logs are stored flat in a [[Vector]] with parent scope IDs linking entries to their enclosing scopes. Tree structure
  * is reconstructed at render time. This design makes state merging O(1) instead of O(n²).
  *
  * @since 0.1.0
  */
sealed trait Log extends Product with Serializable {

  /** The scope ID this log belongs to. 0 means root level (no parent scope). */
  def parentScopeId: Int
}
object Log {

  /** Logs an info message.
    *
    * @since 0.1.0
    */
  def info(message: => String): MIO[Unit] = MIO.log(Entry(Level.Info, () => message, parentScopeId = 0))

  /** Logs a warning message.
    *
    * @since 0.1.0
    */
  def warn(message: => String): MIO[Unit] = MIO.log(Entry(Level.Warn, () => message, parentScopeId = 0))

  /** Logs an error message.
    *
    * @since 0.1.0
    */
  def error(message: => String): MIO[Unit] = MIO.log(Entry(Level.Error, () => message, parentScopeId = 0))

  /** Creates a new scope with a given name. All logs created inside the scope nested under it.
    *
    * @since 0.1.0
    */
  def namedScope[A](name: String)(io: MIO[A]): MIO[A] = MIO.nameLogsScope(name, io)

  /** Logging levels - corresponds with reporting levels available in macro reporters.
    *
    * @since 0.1.0
    */
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

  /** Single log entry, with lazily computed message (messages can be expensive, and usually we don't display logs).
    *
    * @param parentScopeId
    *   the scope this entry belongs to (0 = root)
    * @since 0.1.0
    */
  final case class Entry(level: Level, message: () => String, parentScopeId: Int) extends Log {

    override def toString: String = s"${level.prefix}${message()}"
  }

  /** A named scope marker in the flat log list. Entries belonging to this scope have `parentScopeId == scopeId`.
    *
    * @param scopeId
    *   globally unique ID for this scope
    * @param parentScopeId
    *   the scope this scope is nested inside (0 = root)
    * @param end
    *   [[Timestamp.empty]] until the scope is closed
    * @since 0.1.0
    */
  final case class Scope(name: String, scopeId: Int, parentScopeId: Int, start: Timestamp, end: Timestamp) extends Log {

    override def toString: String = {
      val duration = if (start != Timestamp.empty && end != Timestamp.empty) s" (${end - start}ns)" else ""
      s"$name$duration"
    }
  }

  // ----------------------------------------------- Scope ID generation -------------------------------------------------

  private val scopeIdCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  /** Generate a new globally unique scope ID.
    *
    * @since 0.3.0
    */
  private[effect] def nextScopeId(): Int = scopeIdCounter.incrementAndGet()

  /** Reset the scope ID counter. Called at the start of each macro expansion.
    *
    * @since 0.3.0
    */
  def resetScopeIds(): Unit = scopeIdCounter.set(0)

  // ---------------------------------------------- Tree reconstruction ------------------------------------------------

  /** Reconstruct a tree structure from the flat log list with parent IDs.
    *
    * Used by rendering code. Returns a vector of tree nodes representing the root-level entries and scopes.
    */
  private[effect] def toTree(logs: Logs): Vector[TreeNode] = {
    // Group logs by parent scope ID
    val byParent = new java.util.HashMap[Int, scala.collection.mutable.ArrayBuffer[Log]]()
    var i = 0
    while (i < logs.size) {
      val log = logs(i)
      val pid = log.parentScopeId
      var buf = byParent.get(pid)
      if (buf == null) {
        buf = scala.collection.mutable.ArrayBuffer.empty[Log]
        byParent.put(pid, buf): Unit
      }
      buf += log
      i += 1
    }

    def buildChildren(parentId: Int): Vector[TreeNode] = {
      val children = byParent.get(parentId)
      if (children == null) Vector.empty
      else
        children.iterator.map {
          case Entry(level, message, _)            => TreeEntry(level, message)
          case Scope(name, scopeId, _, start, end) => TreeScope(name, buildChildren(scopeId), start, end)
        }.toVector
    }

    buildChildren(0) // 0 = root
  }

  /** Tree node for rendering — reconstructed from flat logs. */
  sealed private[effect] trait TreeNode
  final private[effect] case class TreeEntry(level: Level, message: () => String) extends TreeNode
  final private[effect] case class TreeScope(name: String, entries: Vector[TreeNode], start: Timestamp, end: Timestamp)
      extends TreeNode

  // --------------------------------------------- Implementation details ---------------------------------------------

  /** Render structured [[Logs]] into a String, in a stack-safe way. */
  private[effect] def render(rootScopeName: String, logs: Logs, start: Log.Timestamp, end: Log.Timestamp)(
      filter: Level => Boolean
  ): String = {
    val tree = toTree(logs)
    val filtered = filterTree(tree, filter)
    val rootNode = TreeScope(rootScopeName, filtered, start, end)
    renderTree(new StringBuilder, Vector(Item(rootNode, "", "")))
  }

  /** Filter tree nodes by level, collapsing empty scopes. */
  private def filterTree(nodes: Vector[TreeNode], filter: Level => Boolean): Vector[TreeNode] =
    nodes.flatMap {
      case TreeEntry(level, message) if filter(level) => Vector(TreeEntry(level, message))
      case _: TreeEntry                               => Vector.empty
      case TreeScope(name, entries, start, end)       =>
        val filtered = filterTree(entries, filter)
        if (filtered.nonEmpty) Vector(TreeScope(name, filtered, start, end))
        else Vector.empty
    }

  /** Render tree nodes in a queue without stack overflow. */
  @scala.annotation.tailrec
  private def renderTree(sb: StringBuilder, queue: Vector[Item]): String = queue match {
    case currentItem +: remaining => renderTree(sb, currentItem.step(sb) ++ remaining)
    case _                        => sb.toString()
  }

  /** Render either a single log [[TreeEntry]] or a nested [[TreeScope]] name, returning a queue of remaining items. */
  final private case class Item(node: TreeNode, indentHead: String, indentTail: String) {

    def init(nodes: Vector[TreeNode]): Vector[Item] = nodes.init.map(Item(_, indentTail + "├ ", indentTail + "│ "))
    def last(nodes: Vector[TreeNode]): Vector[Item] = Vector(Item(nodes.last, indentTail + "└ ", indentTail + "  "))

    @scala.annotation.nowarn("msg=unused")
    def step(sb: StringBuilder): Vector[Item] = node match {
      case TreeEntry(level, message) =>
        val msg = message().split("\n")
        sb.append(indentHead).append(level.prefix).append(msg.head).append("\n")
        msg.tail.foreach(line => sb.append(indentTail).append("        ").append(line).append("\n"))
        Vector.empty
      case TreeScope(name, entries, start, end) =>
        val duration = if (start != Log.Timestamp.empty && end != Log.Timestamp.empty) s" (${end - start}ns)" else ""
        val msg = (name + duration + ":").split("\n")
        sb.append(indentHead).append(msg.head).append("\n")
        msg.tail.foreach(line => sb.append(indentTail).append(line).append("\n"))

        entries.size match {
          case 0 => Vector.empty
          case 1 => last(entries)
          case _ => init(entries) ++ last(entries)
        }
    }
  }

  type Timestamp = Long
  object Timestamp {
    def now: Timestamp = System.nanoTime()
    val empty: Timestamp = Long.MinValue
  }
}
