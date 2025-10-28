package hearth
package treeprinter

// Exception thrown when the StringBuilder approaches its maximum length limit.
// We catch it, to unwind the stack and return the truncated string.
final case class StringBuildingTerminated(sb: StringBuilder) extends scala.util.control.NoStackTrace
