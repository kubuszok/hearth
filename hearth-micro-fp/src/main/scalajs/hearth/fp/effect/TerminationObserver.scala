package hearth
package fp
package effect

private[effect] object TerminationObserver {

  /** Returns true if the Thread is interrupted (JS).
    *
    * Allows us to escape infinite loop in MIO by pressing Ctrl+C or sending SIGTERM.
    *
    * @since 0.1.0
    */
  def isTerminated: Boolean = Thread.interrupted()
}
