package hearth

trait EnvironmentCrossQuotesSupport { this: Environments =>

  /** Scala 2 does not need to provide any platform-specific utilities to use in macros. */
  private[hearth] trait CrossQuotesSupport { this: CrossQuotes.type => }
}
