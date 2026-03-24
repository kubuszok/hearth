package hearth
package fp
package effect

/** Renders [[Logs]] as a speedscope-compatible JSON flame graph.
  *
  * The output can be opened at https://www.speedscope.app/ or any tool supporting the speedscope format.
  *
  * Requires benchmark timestamps to be present in [[Log.Scope]] entries (i.e. `MIO.benchmarkScopes = true`).
  *
  * @since 0.3.0
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
    * @since 0.3.0
    */
  def renderSpeedscopeTo(out: java.lang.Appendable, name: String, logs: Logs, macroStart: Log.Timestamp): Boolean = {
    val frameMap = new java.util.HashMap[String, java.lang.Integer]()
    val frameNames = scala.collection.mutable.ArrayBuffer.empty[String]
    val events = scala.collection.mutable.ArrayBuffer.empty[Event]

    def stripAnsi(s: String): String = s.replaceAll("\u001b\\[[0-9;]*m", "")

    def frameIndex(frameName: String): Int = {
      val existing = frameMap.get(frameName)
      if (existing != null) existing.intValue()
      else {
        val idx = frameNames.length
        frameNames += frameName
        frameMap.put(frameName, java.lang.Integer.valueOf(idx))
        idx
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
      // Sort events in-place by timestamp; at same timestamp, close before open to maintain valid nesting.
      events.sortInPlace()(eventOrdering)
      val startValue = events.head.at
      val endValue = events.last.at

      out.append("{\n")
      out.append("""  "$schema": "https://www.speedscope.app/file-format-schema.json",""")
      out.append("\n")
      out.append("  \"shared\": {\n")
      out.append("    \"frames\": [\n")
      var fi = 0
      while (fi < frameNames.length) {
        out.append("      {\"name\": ")
        escapeJsonTo(out, frameNames(fi))
        out.append("}")
        if (fi < frameNames.length - 1) { out.append(","); () }
        out.append("\n")
        fi += 1
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
      // Write events, deduplicating consecutive identical events inline (caused by MState log merging).
      // This avoids allocating an intermediate deduped collection.
      var prevEvent: Event = null
      var needsComma = false
      var ei = 0
      while (ei < events.length) {
        val event = events(ei)
        if (prevEvent == null || event != prevEvent) {
          if (needsComma) { out.append(",\n"); () }
          out.append("        {\"type\": \"")
          out.append(event.tpe)
          out.append("\", \"at\": ")
          out.append(event.at.toString)
          out.append(", \"frame\": ")
          out.append(event.frame.toString)
          out.append("}")
          needsComma = true
          prevEvent = event
        }
        ei += 1
      }
      if (needsComma) { out.append("\n"); () }
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

  // Sort by timestamp; at same timestamp, close before open to maintain valid nesting.
  // Custom ordering avoids boxing (Long, Int) tuples on every comparison.
  private val eventOrdering: Ordering[Event] = (a: Event, b: Event) => {
    val cmp = java.lang.Long.compare(a.at, b.at)
    if (cmp != 0) cmp
    else (if (a.tpe == "C") 0 else 1) - (if (b.tpe == "C") 0 else 1)
  }

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
