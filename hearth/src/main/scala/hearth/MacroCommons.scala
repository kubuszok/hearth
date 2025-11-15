package hearth

trait MacroUntypedCommons extends untyped.UntypedTypes with untyped.UntypedExprs with untyped.UntypedMethods {
  this: MacroCommons =>
}

trait MacroTypedCommons
    extends Environments
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

  implicit final class MioExprOps[A](private val io: fp.effect.MIO[Expr[A]]) {

    /** Expand the final result of the MIO, or fail with a message.
      *
      * @since 0.1.0
      *
      * @param macroName
      *   name of the macro that is being expanded, it will be used the the top scope of the logs tree
      * @param infoRendering
      *   how to render info logs, if [[DontRender]] is used, info logs will not be rendered
      * @param warnRendering
      *   how to render warn logs, if [[DontRender]] is used, warn logs will not be rendered
      * @param errorRendering
      *   how to render error logs, if [[DontRender]] is used, error logs will not be rendered
      * @param failOnErrorLog
      *   whether to fail if there are error logs, if true, the macro expansion will fail if there is any error log
      * @param renderFailure
      *   if macro expansion failed and there are both errors logs anf exceptions, this function will be called to
      *   render the error message
      * @return
      *   the final expression OR fails the macro expansion with the error message
      */
    def runToExprOrFail(
        macroName: String,
        infoRendering: hearth.fp.effect.LogRendering = hearth.fp.effect.DontRender,
        warnRendering: hearth.fp.effect.LogRendering = hearth.fp.effect.RenderFrom(hearth.fp.effect.Log.Level.Warn),
        errorRendering: hearth.fp.effect.LogRendering = hearth.fp.effect.RenderFrom(hearth.fp.effect.Log.Level.Error),
        failOnErrorLog: Boolean = false
    )(
        renderFailure: (String, fp.data.NonEmptyVector[Throwable]) => String
    ): Expr[A] = Environment.handleMioTerminationException {
      import fp.effect.LogsOps

      val (state, result) = io.unsafe.runSync
      result match {
        case Right(expr) =>
          state.logs
            .render(macroName, infoRendering)
            .filter(_.length - 2 > macroName.length)
            .foreach(Environment.reportInfo)
          state.logs
            .render(macroName, warnRendering)
            .filter(_.length - 2 > macroName.length)
            .foreach(Environment.reportWarn)
          state.logs
            .render(macroName, errorRendering)
            .filter(_.length - 2 > macroName.length && failOnErrorLog)
            .foreach(Environment.reportErrorAndAbort)
          expr
        case Left(errors) =>
          Environment.reportErrorAndAbort(
            state.logs
              .render(macroName, infoRendering)
              .map(renderFailure(_, errors))
              .orElse(state.logs.render(macroName, warnRendering).map(renderFailure(_, errors)))
              .orElse(state.logs.render(macroName, errorRendering).map(renderFailure(_, errors)))
              .filter(_.trim.count(_ == '\n') > 0)
              .getOrElse("")
          )
      }
    }
  }

  implicit final class MLocalCacheOps(private val cache: fp.effect.MLocal[ValDefsCache]) {

    def forwardDeclare[Signature, Returned, Value](
        key: String,
        builder: ValDefBuilder[Signature, Returned, Value]
    ): fp.effect.MIO[Unit] = for {
      cache1 <- cache.get
      cache2 = builder.forwardDeclare(cache1, key)
      _ <- cache.set(cache2)
    } yield ()

    def get1[A: Type, Returned: Type](key: String): fp.effect.MIO[Option[Expr[A] => Expr[Returned]]] =
      cache.get.map(_.get1(key))
  }
}
