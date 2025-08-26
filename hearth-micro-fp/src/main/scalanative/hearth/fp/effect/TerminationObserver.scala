package hearth
package fp
package effect

private[effect] object TerminationObserver {

  /** Returns true if the JVM is terminating or the thread is interrupted (Native).
    *
    * Allows us to escape infinite loop in MIO by pressing Ctrl+C or sending SIGTERM.
    *
    * @since 0.1.0
    */
  def isTerminated: Boolean = Thread.interrupted() || terminated.get()

  private val terminated = {
    val handler = new java.util.concurrent.atomic.AtomicBoolean(false)
    val _ = sys.addShutdownHook(handler.set(true))
    handler
  }
}
