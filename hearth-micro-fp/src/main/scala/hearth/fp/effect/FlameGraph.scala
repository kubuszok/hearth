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
    val sw = new java.io.StringWriter()
    if (renderSpeedscopeTo(sw, name, logs, macroStart)) Some(sw.toString)
    else None
  }

  /** Render logs as speedscope JSON directly to an [[Appendable]] (e.g. Writer, StringBuilder).
    *
    * This avoids building the entire JSON in memory, which is important for large flame graphs.
    *
    * @param out
    *   the output to write to
    * @param name
    *   the profile name (e.g. macro name, file:line)
    * @param logs
    *   the logs to render
    * @param macroStart
    *   the reference timestamp captured at the start of macro expansion; all scope timestamps are relative to this
    * @return
    *   true if events were written, false if there are no benchmarked scopes
    * @since 3.0
    */
  def renderSpeedscopeTo(out: java.lang.Appendable, name: String, logs: Logs, macroStart: Log.Timestamp): Boolean = {
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

    // Stack-safe traversal using explicit work queue instead of recursion
    val workQueue = new java.util.ArrayDeque[AnyRef]()
    workQueue.add(logs)
    while (!workQueue.isEmpty)
      workQueue.poll() match {
        case currentLogs: Logs @unchecked =>
          // Process in reverse so that items end up in the correct order on the queue
          var i = currentLogs.size - 1
          while (i >= 0) {
            currentLogs(i) match {
              case Log.Scope(scopeName, entries, start, end)
                  if start != Log.Timestamp.empty && end != Log.Timestamp.empty =>
                val idx = frameIndex(stripAnsi(scopeName))
                val relStart = start - macroStart
                val relEnd = end - macroStart
                // Push close marker first (will be processed after children)
                workQueue.addFirst(CloseMarker(idx, relEnd))
                // Then the child logs
                workQueue.addFirst(entries)
                // Record the open event immediately
                events += Event("O", idx, relStart)
              case Log.Scope(_, entries, _, _) =>
                workQueue.addFirst(entries)
              case _: Log.Entry =>
              // Entries don't have timing, skip
            }
            i -= 1
          }
        case CloseMarker(idx, relEnd) =>
          events += Event("C", idx, relEnd)
      }

    if (events.isEmpty) false
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

      out.append("{\n")
      out.append("""  "$schema": "https://www.speedscope.app/file-format-schema.json",""")
      out.append("\n")
      out.append("  \"shared\": {\n")
      out.append("    \"frames\": [\n")
      frames.zipWithIndex.foreach { case (frame, i) =>
        out.append("      {\"name\": ")
        escapeJsonTo(out, frame)
        out.append("}")
        if (i < frames.length - 1) { out.append(","); () }
        out.append("\n")
      }
      out.append("    ]\n")
      out.append("  },\n")
      out.append("  \"profiles\": [\n")
      out.append("    {\n")
      out.append("      \"type\": \"evented\",\n")
      out.append("      \"name\": ")
      escapeJsonTo(out, name)
      out.append(",\n")
      out.append("      \"unit\": \"nanoseconds\",\n")
      out.append("      \"startValue\": ")
      out.append(startValue.toString)
      out.append(",\n")
      out.append("      \"endValue\": ")
      out.append(endValue.toString)
      out.append(",\n")
      out.append("      \"events\": [\n")
      sorted.zipWithIndex.foreach { case (event, i) =>
        out.append("        {\"type\": \"")
        out.append(event.tpe)
        out.append("\", \"at\": ")
        out.append(event.at.toString)
        out.append(", \"frame\": ")
        out.append(event.frame.toString)
        out.append("}")
        if (i < sorted.length - 1) { out.append(","); () }
        out.append("\n")
      }
      out.append("      ]\n")
      out.append("    }\n")
      out.append("  ],\n")
      out.append("  \"name\": ")
      escapeJsonTo(out, name)
      out.append("\n")
      out.append("}\n")
      true
    }
  }

  final private case class Event(tpe: String, frame: Int, at: Long)

  final private case class CloseMarker(frame: Int, relEnd: Long)

  private def escapeJsonTo(out: java.lang.Appendable, s: String): Unit = {
    out.append('"')
    var i = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '"'          => out.append("\\\"")
        case '\\'         => out.append("\\\\")
        case '\n'         => out.append("\\n")
        case '\r'         => out.append("\\r")
        case '\t'         => out.append("\\t")
        case c if c < ' ' => out.append("\\u%04x".format(c.toInt))
        case c            => out.append(c)
      }
      i += 1
    }
    out.append('"')
    ()
  }
}
