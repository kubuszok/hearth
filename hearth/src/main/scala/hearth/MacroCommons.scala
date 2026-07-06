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
    with typed.Classes
    with typed.ExprCodecDerivation {
  this: MacroCommons =>
}

/** The root cake of Hearth's macro API: mix this into your macro implementation to get the typed ([[typed.Types]],
  * [[typed.Exprs]], ...) and untyped layers plus [[Environments]].
  *
  * @since 0.1.0
  */
trait MacroCommons extends MacroUntypedCommons with MacroTypedCommons {

  /** Throws an [[AssertionError]] with the given message.
    *
    * Intended to signal that there is an invalid path in macro that hasn't been properly handled.
    *
    * This is the assertion a macro author calls for an unreachable/unhandled path in ''their own'' macro. The
    * Hearth-internal [[hearthAssertionFailed]]/[[hearthRequirementFailed]] pair is different: it distinguishes a Hearth
    * bug that should be reported from a user misusing a Hearth API.
    *
    * @since 0.1.0
    *
    * @param message
    *   the message describing the unhandled path
    */
  final def assertionFailed(message: String): Nothing = throw new AssertionError(message)

  /** Throws an [[HearthAssertionError]] with the given description.
    *
    * Intended to signal that there is an invalid path in macro that hasn't been properly handled, that should be
    * reported as an issue.
    *
    * @see
    *   [[HearthAssertionError]]
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
    * @see
    *   [[HearthRequirementError]]
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
