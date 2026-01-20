package hearth

import hearth.fp.effect.*
import scala.reflect.{classTag, ClassTag}

trait MIOIntegrations { this: MacroTypedCommons =>

  implicit final class MioExprOps[A](private val io: MIO[Expr[A]]) {

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
        infoRendering: LogRendering = DontRender,
        warnRendering: LogRendering = RenderFrom(Log.Level.Warn),
        errorRendering: LogRendering = RenderFrom(Log.Level.Error),
        failOnErrorLog: Boolean = false
    )(
        renderFailure: (String, fp.data.NonEmptyVector[Throwable]) => String
    ): Expr[A] = Environment.handleMioTerminationException {
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
          def infoLogs = state.logs.render(macroName, infoRendering)
          def warnLogs = state.logs.render(macroName, warnRendering)
          def errorLogs = state.logs.render(macroName, errorRendering)

          val logs = infoLogs.orElse(warnLogs).orElse(errorLogs).filter(_.trim.count(_ == '\n') > 0)
          val msg = logs.map(renderFailure(_, errors)).getOrElse(renderFailure("", errors))

          def fallbackMessage =
            errors
              .map(e => e.getMessage.split("\n").map("  " + _).mkString("\n"))
              .mkString("Macro failed with NonEmptyVector(\n", ",\n", "\n)")

          Environment.reportErrorAndAbort(if (msg.nonEmpty) msg else fallbackMessage)
      }
    }
  }

  implicit final class MLocalCacheOps(private val cache: MLocal[ValDefsCache]) {

    def forwardDeclare[Signature, Returned, Value](
        key: String,
        builder: ValDefBuilder[Signature, Returned, Value]
    ): MIO[Unit] = for {
      cache1 <- cache.get
      cache2 = builder.forwardDeclare(cache1, key)
      _ <- cache.set(cache2)
    } yield ()

    def buildCachedWith[Signature, Returned, Value](
        key: String,
        builder: ValDefBuilder[Signature, Returned, Value]
    )(f: Value => Expr[Returned]): MIO[Unit] = for {
      cache1 <- cache.get
      cache2 = builder.buildCachedWith(cache1, key)(f)
      _ <- cache.set(cache2)
    } yield ()

    def buildCached[Signature, Returned](
        key: String,
        builder: ValDefBuilder[Signature, Returned, Expr[Returned]]
    ): MIO[Unit] = buildCachedWith(key, builder)(identity)

    // format: off
    def get0Ary[Returned: Type](key: String): MIO[Option[Expr[Returned]]] =
      cache.get.map(_.get0Ary(key))
    def get1Ary[A: Type, Returned: Type](key: String): MIO[Option[Expr[A] => Expr[Returned]]] =
      cache.get.map(_.get1Ary(key))
    def get2Ary[A: Type, B: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B]) => Expr[Returned]]] =
      cache.get.map(_.get2Ary(key))
    def get3Ary[A: Type, B: Type, C: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C]) => Expr[Returned]]] =
      cache.get.map(_.get3Ary(key))
    def get4Ary[A: Type, B: Type, C: Type, D: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D]) => Expr[Returned]]] =
      cache.get.map(_.get4Ary(key))
    def get5Ary[A: Type, B: Type, C: Type, D: Type, E: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E]) => Expr[Returned]]] =
      cache.get.map(_.get5Ary(key))
    def get6Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F]) => Expr[Returned]]] =
      cache.get.map(_.get6Ary(key))
    def get7Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G]) => Expr[Returned]]] =
      cache.get.map(_.get7Ary(key))
    def get8Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H]) => Expr[Returned]]] =
      cache.get.map(_.get8Ary(key))
    def get9Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I]) => Expr[Returned]]] =
      cache.get.map(_.get9Ary(key))
    def get10Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J]) => Expr[Returned]]] =
      cache.get.map(_.get10Ary(key))
    def get11Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K]) => Expr[Returned]]] =
      cache.get.map(_.get11Ary(key))
    def get12Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L]) => Expr[Returned]]] =
      cache.get.map(_.get12Ary(key))
    def get13Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M]) => Expr[Returned]]] =
      cache.get.map(_.get13Ary(key))
    def get14Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N]) => Expr[Returned]]] =
      cache.get.map(_.get14Ary(key))
    def get15Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O]) => Expr[Returned]]] =
      cache.get.map(_.get15Ary(key))
    def get16Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P]) => Expr[Returned]]] =
      cache.get.map(_.get16Ary(key))
    def get17Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q]) => Expr[Returned]]] =
      cache.get.map(_.get17Ary(key))
    def get18Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R]) => Expr[Returned]]] =
      cache.get.map(_.get18Ary(key))
    def get19Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S]) => Expr[Returned]]] =
      cache.get.map(_.get19Ary(key))
    def get20Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T]) => Expr[Returned]]] =
      cache.get.map(_.get20Ary(key))
    def get21Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U]) => Expr[Returned]]] =
      cache.get.map(_.get21Ary(key))
    def get22Ary[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Returned: Type](key: String): MIO[Option[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V]) => Expr[Returned]]] =
      cache.get.map(_.get22Ary(key))
    // format: on
  }

  implicit final class ExtensionLoadingResultMioOps[Extension: ClassTag](result: => ExtensionLoadingResult[Extension]) {

    /** Lazyly evaluate the result of the extension loading, and return a MIO that will log the result.
      *
      * @since 0.3.0
      *
      * @param allowFailures
      *   whether to allow failures, if true, the MIO will return the loaded extensions even if some extensions failed
      *   to load
      * @return
      *   a MIO that will log the result of the extension loading
      */
    def toMIO(allowFailures: Boolean = false): MIO[ExtensionLoadingResult.Loaded[Extension]] = MIO(result).flatMap {
      case ExtensionLoadingResult.AllLoaded(loadedExtensions) =>
        MIO
          .pure(loadedExtensions)
          .attemptFlatTap(_ =>
            Log.info(
              s"""${classTag[Extension].runtimeClass.getName} - Successfully loaded ${loadedExtensions.size} extensions:
                 |${loadedExtensions.map(ex => "  - " + ex.getClass.getName).mkString("\n")}""".stripMargin
            )
          )
      case ExtensionLoadingResult.SomeFailed(loadedExtensions, errors) =>
        if (allowFailures)
          MIO
            .pure(loadedExtensions)
            .attemptFlatTap(_ =>
              Log.info(
                s"""${classTag[
                    Extension
                  ].runtimeClass.getName} - Successfully loaded ${loadedExtensions.size} extensions:
                   |${loadedExtensions.map(e => s"  - ${e.getClass.getName}").mkString("\n")}
                   |Failed to load ${errors.size} extensions:
                   |${errors.toNonEmptyVector
                    .map { case (e, t) => s"  - ${e.getClass.getName}: ${t.getMessage}" }
                    .mkString("\n")}
                   |but failures were allowed""".stripMargin
              )
            )
        else
          MIO
            .fail(errors.toNonEmptyVector.map(_._2))
            .attemptFlatTap(_ =>
              Log.error(
                s"""${classTag[Extension].runtimeClass.getName} - Failed to load ${errors.size} extensions:
                   |${errors.toNonEmptyVector
                    .map { case (e, t) => s"  - ${e.getClass.getName}: ${t.getMessage}" }
                    .mkString("\n")}
                   |Successfully loaded ${loadedExtensions.size} extensions:
                   |${loadedExtensions.map(ex => s"  - ${ex.getClass.getName}").mkString("\n")}
                   |but failures were not allowed""".stripMargin
              )
            )
      case ExtensionLoadingResult.LoaderFailed(error) =>
        MIO
          .fail(error)
          .attemptFlatTap(_ =>
            Log.error(
              s"""${classTag[Extension].runtimeClass.getName} - Failed to load extensions:
                 |${error.getMessage}""".stripMargin
            )
          )
    }
  }
}
