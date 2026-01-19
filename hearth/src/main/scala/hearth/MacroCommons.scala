package hearth

trait MacroUntypedCommons extends untyped.UntypedTypes with untyped.UntypedExprs with untyped.UntypedMethods {
  this: MacroCommons =>
}

trait MacroTypedCommons
    extends Environments
    with MIOIntegrations
    with typed.Types
    with typed.Exprs
    with typed.Methods
    with typed.Existentials
    with typed.Classes {
  this: MacroCommons =>
}

trait MacroCommons extends MacroUntypedCommons with MacroTypedCommons {

  /** Throws an [[AssertionError]] with the given message.
    *
    * Intended to signal that there is an invalid path in macro that hasn't been properly handled.
    *
    * @since 0.1.0
    */
  final def assertionFailed(message: String): Nothing = throw new AssertionError(message)

  /** Throws an [[HearthAssertionError]] with the given description.
    *
    * Intended to signal that there is an invalid path in macro that hasn't been properly handled, that should be
    * reported as an issue.
    *
    * @since 0.1.0
    */
  final private[hearth] def hearthAssertionFailed(description: String): Nothing = throw HearthAssertionError(
    description,
    hearthVersion = HearthVersion.byHearthLibrary,
    scalaVersion = Environment.currentScalaVersion,
    platform = Environment.currentPlatform,
    jdkVersion = Environment.currentJDKVersion
  )

  /** Throws an [[HearthRequirementError]] with the given description.
    *
    * Used to inform users that they are using Hearth in an invalid way, and should fix their code.
    *
    * @since 0.2.0
    */
  final private[hearth] def hearthRequirementFailed(description: String): Nothing = throw HearthRequirementError(
    description,
    hearthVersion = HearthVersion.byHearthLibrary,
    scalaVersion = Environment.currentScalaVersion,
    platform = Environment.currentPlatform,
    jdkVersion = Environment.currentJDKVersion
  )
}
