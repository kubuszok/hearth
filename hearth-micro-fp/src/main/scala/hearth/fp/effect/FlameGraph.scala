package hearth
package fp
package effect

/** Renders [[Logs]] as a speedscope-compatible JSON flame graph.
  *
  * The output can be opened at https://www.speedscope.app/ or any tool supporting the speedscope format.
  *
  * Requires benchmark timestamps to be present in [[Log.Scope]] entries (i.e. `MIO.benchmarkScopes = true`).
  *
  * @since 3.0
  */
object FlameGraph {

  /** Render logs as a speedscope JSON string.
    *
    * @param name
    *   the profile name (e.g. macro name, file:line)
    * @param logs
    *   the logs to render
    * @param macroStart
    *   the reference timestamp captured at the start of macro expansion; all scope timestamps are relative to this
    * @return
    *   a speedscope-format JSON string, or None if there are no benchmarked scopes
    */
  def renderSpeedscope(name: String, logs: Logs, macroStart: Log.Timestamp): Option[String] = {
    val frames = scala.collection.mutable.ArrayBuffer.empty[String]
    val events = scala.collection.mutable.ArrayBuffer.empty[Event]

    def stripAnsi(s: String): String = s.replaceAll("\u001b\\[[0-9;]*m", "")

    def frameIndex(frameName: String): Int = {
      val idx = frames.indexOf(frameName)
      if (idx >= 0) idx
      else {
        frames += frameName
        frames.length - 1
      }
    }

    def traverse(logs: Logs): Unit = logs.foreach {
      case Log.Scope(scopeName, entries, start, end) if start != Log.Timestamp.empty && end != Log.Timestamp.empty =>
        val idx = frameIndex(stripAnsi(scopeName))
        val relStart = start - macroStart
        val relEnd = end - macroStart
        events += Event("O", idx, relStart)
        traverse(entries)
        events += Event("C", idx, relEnd)
      case Log.Scope(_, entries, _, _) =>
        traverse(entries)
      case _: Log.Entry =>
      // Entries don't have timing, skip
    }

    traverse(logs)

    if (events.isEmpty) None
    else {
      // Sort events by timestamp; at same timestamp, close before open to maintain valid nesting.
      // Deduplicate consecutive identical events caused by MState log merging.
      val sorted = events
        .sortBy(e => (e.at, if (e.tpe == "C") 0 else 1))
        .foldLeft(List.empty[Event]) {
          case (acc, event) if acc.headOption.contains(event) => acc
          case (acc, event)                                   => event :: acc
        }
        .reverse
      val startValue = sorted.head.at
      val endValue = sorted.last.at

      val sb = new StringBuilder
      sb.append("{\n")
      sb.append("""  "$schema": "https://www.speedscope.app/file-format-schema.json",""")
      sb.append("\n")
      sb.append("  \"shared\": {\n")
      sb.append("    \"frames\": [\n")
      frames.zipWithIndex.foreach { case (frame, i) =>
        sb.append("      {\"name\": ")
        sb.append(escapeJson(frame))
        sb.append("}")
        if (i < frames.length - 1) sb.append(",")
        sb.append("\n")
      }
      sb.append("    ]\n")
      sb.append("  },\n")
      sb.append("  \"profiles\": [\n")
      sb.append("    {\n")
      sb.append("      \"type\": \"evented\",\n")
      sb.append("      \"name\": ")
      sb.append(escapeJson(name))
      sb.append(",\n")
      sb.append("      \"unit\": \"nanoseconds\",\n")
      sb.append("      \"startValue\": ")
      sb.append(startValue)
      sb.append(",\n")
      sb.append("      \"endValue\": ")
      sb.append(endValue)
      sb.append(",\n")
      sb.append("      \"events\": [\n")
      sorted.zipWithIndex.foreach { case (event, i) =>
        sb.append("        {\"type\": \"")
        sb.append(event.tpe)
        sb.append("\", \"at\": ")
        sb.append(event.at)
        sb.append(", \"frame\": ")
        sb.append(event.frame)
        sb.append("}")
        if (i < sorted.length - 1) sb.append(",")
        sb.append("\n")
      }
      sb.append("      ]\n")
      sb.append("    }\n")
      sb.append("  ],\n")
      sb.append("  \"name\": ")
      sb.append(escapeJson(name))
      sb.append("\n")
      sb.append("}\n")
      Some(sb.toString())
    }
  }

  final private case class Event(tpe: String, frame: Int, at: Long)

  private def escapeJson(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"'          => sb.append("\\\"")
      case '\\'         => sb.append("\\\\")
      case '\n'         => sb.append("\\n")
      case '\r'         => sb.append("\\r")
      case '\t'         => sb.append("\\t")
      case c if c < ' ' => sb.append("\\u%04x".format(c.toInt))
      case c            => sb.append(c)
    }
    sb.append("\"")
    sb.toString()
  }
}
