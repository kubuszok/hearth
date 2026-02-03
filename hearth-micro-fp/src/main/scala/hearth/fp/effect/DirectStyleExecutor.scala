package hearth
package fp
package effect

import java.util.concurrent.*
import scala.concurrent.*

private[fp] object DirectStyleExecutor {

  /** Run the thunk in a separate thread to avoid StackOverflowError when using recursive MIO with direct style.
    *
    * @since 0.1.0
    */
  def apply[A](thunk: => A): A =
    Await
      .result(
        scala.concurrent.Future {
          scala.concurrent.blocking {
            try
              Right(thunk)
            catch {
              // Catch _everything_ including fatal errors to rethrow them outside,
              // to prevent hanging infinitely on e.g. DirectStyle's `PassError` OR `hearthRequirementFailed`.
              case e: Throwable => Left(e)
            }
          }
        },
        duration.Duration.Inf
      )
      .fold(throw _, identity)

  /** Virtual threads are only avilable on JVM and only from JDK 17+.
    *
    * So we need to discover if they are available and fall back if they are not.
    *
    * @since 0.1.0
    */
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService {
    try
      //  Use virtual thread from JDK 17+ if available
      newVirtualThreadPerTaskExecutor()
    catch {
      // Fallback to normal thread if virtual threads are not available
      case _: Throwable => new FallbackExecutor
    }
  }

  private def newVirtualThreadPerTaskExecutor(): ExecutorService = {
    val method = classOf[java.util.concurrent.Executors].getDeclaredMethod("newVirtualThreadPerTaskExecutor")
    method.setAccessible(true)
    method.invoke(null).asInstanceOf[java.util.concurrent.ExecutorService]
  }

  private class FallbackExecutor extends AbstractExecutorService with AutoCloseable {

    private val factory = new ThreadFactory {
      override def newThread(r: Runnable): Thread = new Thread(r, "Hearth MIO direct-style runner")
    }
    private val shutdownFlag = new java.util.concurrent.atomic.AtomicBoolean(false)
    private val phaser = new Phaser(1) // self

    override def execute(command: Runnable): Unit = {
      if (isShutdown) throw new RejectedExecutionException("Executor is shut down")
      phaser.register()
      val wrapped = new Runnable {
        override def run(): Unit =
          try command.run()
          finally ignore(phaser.arriveAndDeregister())
      }
      val t = factory.newThread(wrapped)
      t.start()
    }

    // ---- lifecycle ----
    override def shutdown(): Unit =
      if (shutdownFlag.compareAndSet(false, true))
        ignore(phaser.arriveAndDeregister())

    override def shutdownNow(): java.util.List[Runnable] = {
      shutdown()
      java.util.Collections.emptyList()
    }

    override def isShutdown: Boolean = shutdownFlag.get()
    override def isTerminated: Boolean = isShutdown && phaser.isTerminated

    override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = {
      if (!isShutdown) return false
      val phase = phaser.getPhase
      try {
        phaser.awaitAdvanceInterruptibly(phase, timeout, unit)
        true
      } catch {
        case _: TimeoutException     => false
        case e: InterruptedException => throw e
      }
    }

    override def close(): Unit = shutdown()
  }
}
