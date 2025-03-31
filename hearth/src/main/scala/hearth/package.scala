package object hearth {

  /** Convenient for literal singletons */
  type Id[A] = A

  // Useful to remove all ANSI coloring from the text
  private[hearth] val AnsiControlCode = "\u001b\\[([0-9]+)m".r
  private[hearth] def removeAnsiColors(str: String): String = AnsiControlCode.replaceAllIn(str, "")
}
