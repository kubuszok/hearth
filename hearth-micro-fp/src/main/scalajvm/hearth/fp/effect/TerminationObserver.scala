package hearth
package fp
package effect

private[effect] object TerminationObserver {

  /** Returns true if the JVM is terminating or the thread is interrupted (JVM).
    *
    * Allows us to escape infinite loop in MIO by pressing Ctrl+C or sending SIGTERM.
    *
    * @since 0.1.0
    */
  def isTerminated: Boolean = Thread.interrupted() || terminated.get()

  private val terminated = {
    val handler = new java.util.concurrent.atomic.AtomicBoolean(false)
    java.lang.Runtime.getRuntime.addShutdownHook(new Thread(() => handler.set(true)))
    sun.misc.Signal.handle(new sun.misc.Signal("INT"), _ => handler.set(true))
    sun.misc.Signal.handle(new sun.misc.Signal("TERM"), _ => handler.set(true))
    handler
  }
}
