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
  def assertionFailed(message: String): Nothing = throw new AssertionError(message)

  implicit final class MioExprOps[A](private val io: fp.effect.MIO[Expr[A]]) {

    /** Expand the final result of the MIO, or fail with a message.
      *
      * @since 0.1.0
      *
      * @param macroName
      *   name of the macro that is being expanded, it will be used the the top scope of the logs tree
      * @param renderInfoLogs
      *   whether to render info logs
      * @param renderWarnLogs
      *   whether to render warn logs
      * @param failOnErrorLog
      *   whether to fail if there are error logs
      * @param renderFailure
      *   if macro expansion failed and there are both errors logs anf exceptions, this function will be called to
      *   render the error message
      * @return
      *   the final expression OR fails the macro expansion with the error message
      */
    def expandFinalResultOrFail(
        macroName: String,
        renderInfoLogs: Boolean = false,
        renderWarnLogs: Boolean = true,
        failOnErrorLog: Boolean = true
    )(
        renderFailure: (String, fp.data.NonEmptyVector[Throwable]) => String
    ): Expr[A] = {
      import fp.effect.LogsOps

      val (state, result) = io.unsafe.runSync
      result match {
        case Right(expr) =>
          lazy val info = state.logs.render.onlyInfo(macroName)
          if (renderInfoLogs && info.length - 2 > macroName.length) {
            Environment.reportInfo(info)
          }
          lazy val warnings = state.logs.render.onlyWarn(macroName)
          if (renderWarnLogs && warnings.length - 2 > macroName.length) {
            Environment.reportWarn(warnings)
          }
          lazy val errors = state.logs.render.onlyError(macroName)
          if (failOnErrorLog && errors.length - 2 > macroName.length) {
            Environment.reportErrorAndAbort(errors)
          }
          expr
        case Left(errors) =>
          Environment.reportErrorAndAbort(renderFailure(state.logs.render.onlyError(macroName), errors))
      }
    }
  }
}
