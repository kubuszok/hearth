package hearth
package fp
package effect

import scala.math.Ordering.Implicits.*
import hearth.fp.data.NonEmptyVector
import org.scalacheck.Prop.{Exception as _, *}

final class MioSpec extends ScalaCheckSuite with Laws {

  group("MIO's redeem's family of methods'") {

    test(".redeemWith should combine .flatMap and .handleErrorWith") {
      forAll { (fa: MIO[Int]) =>
        fa.redeemWith { a =>
          MIO.pure(a + 1)
        } { _ =>
          MIO.pure(0)
        } === fa.map(a => a + 1).handleErrorWith(_ => MIO.pure(0))
      }
    }

    test(".redeem should combine .map and .handleError") {
      forAll { (fa: MIO[Int]) =>
        fa.redeem { a =>
          a + 1
        } { _ =>
          0
        } === fa.map(a => a + 1).handleError(_ => 0)
      }
    }
  }

  group("MIO's attempt's family of methods'") {

    test(".attempt should combine .map to Right and .handleError to Left") {
      forAll { (fa: MIO[Int]) =>
        fa.attempt === fa.map(Right(_)).handleError(errors => Left(errors))
      }
    }

    test(".unattempt should turn Right to success and Left to failure") {
      forAll { (fa: MIO[Int]) =>
        fa.map(Right(_)).handleError(errors => Left(errors)).unattempt === fa
      }
    }

    test(".attemptFlatTap should combine .attempt and .flatTap (run side-effects without recovering)") {
      forAll { (fa: MIO[Int]) =>
        var used = false // We're testing side-effects !!!
        val _ = fa
          .attemptFlatTap {
            case Right(a) =>
              used = true
              MIO.pure(a + 1)
            case Left(_) =>
              used = true
              MIO.pure(0)
          }
          .unsafe
          .runSync
        used === true
      }
    }

    test(".attemptTap should combine .attempt and .tap (run side-effects without recovering)") {
      forAll { (fa: MIO[Int]) =>
        var used = false // We're testing side-effects !!!
        val _ = fa
          .attemptTap {
            case Right(a) =>
              used = true
              a + 1
            case Left(_) =>
              used = true
              0
          }
          .unsafe
          .runSync
        used === true
      }
    }
  }

  group("MIO's monadic family of methods'") {

    test(".flatMap should follow Left Identity law") {
      forAll { (a: Int, f: Int => MIO[Int]) =>
        MIO.pure(a).flatMap(f) === f(a)
      }
    }

    test(".flatMap should follow Right Identity law") {
      forAll { (fa: MIO[Int]) =>
        fa.flatMap(MIO.pure) === fa
      }
    }

    test(".flatMap should follow Associative law") {
      forAll { (fa: MIO[Int], f: Int => MIO[Int], g: Int => MIO[Int]) =>
        fa.flatMap(f).flatMap(g) === fa.flatMap(a => f(a).flatMap(g))
      }
    }

    test(".flatten should behave like .flatMap(identity)") {

      forAll { (fa: MIO[MIO[Int]]) =>
        fa.flatten === fa.flatMap(identity)
      }
    }

    test(".flatTap should behave like .flatMap(a => f(a).as(a)) (perform side-effects, but discarding results)") {
      forAll { (fa: MIO[Int], f: Int => MIO[Unit]) =>
        fa.flatTap(f) === fa.flatMap(a => f(a).as(a))
      }
    }

    test(".map should follow Identity law") {
      forAll { (fa: MIO[Int]) =>
        fa.map(identity) === fa
      }
    }

    test(".map should follow Composition law") {
      forAll { (fa: MIO[Int], f: Int => Int, g: Int => Int) =>
        fa.map(f).map(g) === fa.map(f andThen g)
      }
    }

    test(".mapTap should behave like .map(a => ignore(f(a)); a)") {
      forAll { (fa: MIO[Int], f: Int => Int) =>
        fa.mapTap(f) === fa.map { a => ignore(f(a)); a }
      }
    }

    test(".map2 should follow Associativity law") {
      forAll { (fa: MIO[Int], fb: MIO[Int], fc: MIO[Int]) =>
        fa.map2(fb)(_ -> _).map2(fc) { case ((a, b), c) => (a, b, c) } === fa.map2(fb.map2(fc)(_ -> _)) {
          case (a, (b, c)) => (a, b, c)
        }
      }
    }

    test(".map2 should follow Left Identity law") {
      forAll { (fa: MIO[Int]) =>
        fa.map2(MIO.void)((a, _) => a) === fa
      }
    }

    test(".map2 should follow Right Identity law") {
      forAll { (fa: MIO[Int]) =>
        MIO.void.map2(fa)((_, b) => b) === fa
      }
    }

    test(".tuple should follow Left Identity law") {
      forAll { (fa: MIO[Int]) =>
        fa.tuple(MIO.void).map(_._1) === fa
      }
    }

    test(".tuple should follow Right Identity law") {
      forAll { (fa: MIO[Int]) =>
        MIO.void.tuple(fa).map(_._2) === fa
      }
    }

    test(".as should map successful value to pure value") {
      forAll { (fa: MIO[Int], b: Int) =>
        fa.as(b) === fa.map(_ => b)
      }
    }

    test(".void should map successful value to Unit value") {
      forAll { (fa: MIO[Int]) =>
        fa.void === fa.map(_ => ())
      }
    }

    test(">> should behave like .flatMap(_ => fb)") {
      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        fa >> fb === fa.flatMap(_ => fb)
      }
    }

    test("<* should behave like .map2(fb)((a, _) => a)") {
      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        fa <* fb === fa.map2(fb)((a, _) => a)
      }
    }

    test("*> should behave like .map2(fb)((_, b) => b)") {
      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        fa *> fb === fa.map2(fb)((_, b) => b)
      }
    }
  }

  group("MIO's monad error family of methods'") {

    test(".handleErrorWith should handle every error") {
      forAll { (msg: String) =>
        MIO.fail(ExampleError(msg)).handleErrorWith { errors =>
          MIO.pure(errors.map(_.getMessage).mkString(", "))
        } === MIO.pure(msg)
      }
    }

    test(".handleError should handle every error") {
      forAll { (msg: String) =>
        MIO.fail(ExampleError(msg)).handleError { errors =>
          errors.map(_.getMessage).mkString(", ")
        } === MIO.pure(msg)
      }
    }

    test(".recoverWith should handle only some errors") {
      forAll { (msg: String, switch: Boolean) =>
        MIO.fail(if (switch) new IllegalArgumentException(msg) else ExampleError(msg)).recoverWith {
          case NonEmptyVector(_: IllegalArgumentException, Vector()) => MIO.pure(msg)
        } === (if (switch) MIO.pure(msg) else MIO.fail(ExampleError(msg)))
      }
    }

    test(".recover should handle only some errors") {
      forAll { (msg: String, switch: Boolean) =>
        MIO.fail(if (switch) new IllegalArgumentException(msg) else ExampleError(msg)).recover {
          case NonEmptyVector(_: IllegalArgumentException, Vector()) => msg
        } === (if (switch) MIO.pure(msg) else MIO.fail(ExampleError(msg)))
      }
    }

    test(".orElse should fall back on second MIO if first MIO fails (aggregating errors if both fail)") {
      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        fa.orElse(fb) === fa.handleErrorWith(errors => fb.handleErrorWith(errors2 => MIO.fail(errors ++ errors2)))
      }
    }
  }

  group("MIO's parallel family of methods'") {

    test(".parMap2 should behave like .map2 but in \"parallel\" (aggregating errors if both fail, state, logs...)") {

      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        fa.parMap2(fb)(_ -> _) === fa.attempt
          .map2(fb.attempt) {
            case (Right(a), Right(b)) => Right((a, b))
            case (Left(e1), Left(e2)) => Left(e1 ++ e2)
            case (Left(e), Right(_))  => Left(e)
            case (Right(_), Left(e))  => Left(e)
          }
          .unattempt
      }
    }

    test(".parTuple should behave like .parMap2(fb)(_ -> _)") {
      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        fa.parTuple(fb) === fa.parMap2(fb)(_ -> _)
      }
    }

    test("<& should behave like .parMap2(fb)((a, _) => a)") {
      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        (fa <& fb) === fa.parMap2(fb)((a, _) => a)
      }
    }

    test("&> should behave like .parMap2(fb)((_, b) => b)") {
      forAll { (fa: MIO[Int], fb: MIO[Int]) =>
        (fa &> fb) === fa.parMap2(fb)((_, b) => b)
      }
    }
  }

  group("MIO parallel logs") {

    test("parMap2 should preserve logs from both branches") {
      val fa = Log.info("from branch A") >> MIO.pure(1)
      val fb = Log.info("from branch B") >> MIO.pure(2)
      val mio = fa.parMap2(fb)(_ + _)

      val (state, result) = mio.unsafe.runSync
      result ==> Right(3)

      // Explicitly count entries — the bug would show as missing one branch's logs
      val entries = state.logs.collect { case e: Log.Entry => e.message() }
      assert(entries.contains("from branch A"), s"Missing branch A in entries: $entries")
      assert(entries.contains("from branch B"), s"Missing branch B in entries: $entries")
      assert(entries.size == 2, s"Expected exactly 2 entries, got ${entries.size}: $entries")

      val rendered = state.logs.render.fromInfo("root")
      assert(rendered.contains("from branch A"), s"Missing branch A log in:\n$rendered")
      assert(rendered.contains("from branch B"), s"Missing branch B log in:\n$rendered")
    }

    test("parMap2 should preserve logs from both branches inside a named scope") {
      val mio = Log.namedScope("outer") {
        val fa = Log.info("A log") >> MIO.pure(1)
        val fb = Log.info("B log") >> MIO.pure(2)
        fa.parMap2(fb)(_ + _)
      }

      val (state, result) = mio.unsafe.runSync
      result ==> Right(3)
      val rendered = state.logs.render.fromInfo("root")
      assert(rendered.contains("A log"), s"Missing A log in:\n$rendered")
      assert(rendered.contains("B log"), s"Missing B log in:\n$rendered")
      assert(rendered.contains("outer"), s"Missing outer scope in:\n$rendered")
    }

    test("parMap2 should preserve logs from both branches with nested scopes") {
      val mio = Log.namedScope("parent") {
        val fa = Log.namedScope("branch-a") {
          Log.info("log from A")
        }
        val fb = Log.namedScope("branch-b") {
          Log.info("log from B")
        }
        fa.parMap2(fb)((_, _))
      }

      val (state, result) = mio.unsafe.runSync
      assert(result.isRight, s"Expected success, got: $result")
      val rendered = state.logs.render.fromInfo("root")
      assert(rendered.contains("branch-a"), s"Missing branch-a scope in:\n$rendered")
      assert(rendered.contains("branch-b"), s"Missing branch-b scope in:\n$rendered")
      assert(rendered.contains("log from A"), s"Missing log from A in:\n$rendered")
      assert(rendered.contains("log from B"), s"Missing log from B in:\n$rendered")
    }

    test("parMap2 should preserve many logs from both branches") {
      val fa = (1 to 5).foldLeft(MIO.pure(())) { (acc, i) =>
        acc >> Log.info(s"A-$i")
      } >> MIO.pure("a")
      val fb = (1 to 5).foldLeft(MIO.pure(())) { (acc, i) =>
        acc >> Log.info(s"B-$i")
      } >> MIO.pure("b")

      val mio = fa.parMap2(fb)(_ + _)
      val (state, _) = mio.unsafe.runSync
      val rendered = state.logs.render.fromInfo("root")
      (1 to 5).foreach { i =>
        assert(rendered.contains(s"A-$i"), s"Missing A-$i in:\n$rendered")
        assert(rendered.contains(s"B-$i"), s"Missing B-$i in:\n$rendered")
      }
    }

    test("parMap2 with DirectStyle runSafe in one branch should preserve all logs") {
      val fa = MIO.scoped { runSafe =>
        runSafe(Log.info("direct-A1"))
        runSafe(Log.info("direct-A2"))
        runSafe(MIO.pure(1))
      }
      val fb = Log.info("monadic-B") >> MIO.pure(2)

      val mio = Log.namedScope("par-scope") {
        fa.parMap2(fb)(_ + _)
      }

      val (state, result) = mio.unsafe.runSync
      result ==> Right(3)
      val rendered = state.logs.render.fromInfo("root")
      assert(rendered.contains("direct-A1"), s"Missing direct-A1 in:\n$rendered")
      assert(rendered.contains("direct-A2"), s"Missing direct-A2 in:\n$rendered")
      assert(rendered.contains("monadic-B"), s"Missing monadic-B in:\n$rendered")
      assert(rendered.contains("par-scope"), s"Missing par-scope in:\n$rendered")
    }

    test("parMap2 with DirectStyle runSafe in both branches should preserve all logs") {
      val fa = MIO.scoped { runSafe =>
        runSafe(Log.namedScope("scope-A")(Log.info("msg-A")))
        runSafe(MIO.pure("a"))
      }
      val fb = MIO.scoped { runSafe =>
        runSafe(Log.namedScope("scope-B")(Log.info("msg-B")))
        runSafe(MIO.pure("b"))
      }

      val mio = fa.parMap2(fb)(_ + _)
      val (state, result) = mio.unsafe.runSync
      result ==> Right("ab")
      val rendered = state.logs.render.fromInfo("root")
      assert(rendered.contains("scope-A"), s"Missing scope-A in:\n$rendered")
      assert(rendered.contains("msg-A"), s"Missing msg-A in:\n$rendered")
      assert(rendered.contains("scope-B"), s"Missing scope-B in:\n$rendered")
      assert(rendered.contains("msg-B"), s"Missing msg-B in:\n$rendered")
    }

    test("parMap2 inside DirectStyle with DirectStyle in branches should preserve all logs") {
      val mio = MIO.scoped { outerRunSafe =>
        outerRunSafe(Log.info("before par"))

        val fa = MIO.scoped { runSafe =>
          runSafe(Log.namedScope("ds-A")(Log.info("inner-A")))
          runSafe(MIO.pure(1))
        }
        val fb = MIO.scoped { runSafe =>
          runSafe(Log.namedScope("ds-B")(Log.info("inner-B")))
          runSafe(MIO.pure(2))
        }

        val result = outerRunSafe(fa.parMap2(fb)(_ + _))
        outerRunSafe(Log.info("after par"))
        result
      }

      val (state, result) = mio.unsafe.runSync
      result ==> Right(3)
      val rendered = state.logs.render.fromInfo("root")
      assert(rendered.contains("before par"), s"Missing 'before par' in:\n$rendered")
      assert(rendered.contains("after par"), s"Missing 'after par' in:\n$rendered")
      assert(rendered.contains("ds-A"), s"Missing ds-A scope in:\n$rendered")
      assert(rendered.contains("inner-A"), s"Missing inner-A in:\n$rendered")
      assert(rendered.contains("ds-B"), s"Missing ds-B scope in:\n$rendered")
      assert(rendered.contains("inner-B"), s"Missing inner-B in:\n$rendered")
    }

    test("nested parMap2 should preserve all logs") {
      val mio = Log.namedScope("level-1") {
        val fa = Log.namedScope("level-2a") {
          val inner1 = Log.info("inner-1a") >> MIO.pure(1)
          val inner2 = Log.info("inner-2a") >> MIO.pure(2)
          inner1.parMap2(inner2)(_ + _)
        }
        val fb = Log.namedScope("level-2b") {
          Log.info("inner-b") >> MIO.pure(10)
        }
        fa.parMap2(fb)(_ + _)
      }

      val (state, result) = mio.unsafe.runSync
      result ==> Right(13)
      val rendered = state.logs.render.fromInfo("root")
      assert(rendered.contains("inner-1a"), s"Missing inner-1a in:\n$rendered")
      assert(rendered.contains("inner-2a"), s"Missing inner-2a in:\n$rendered")
      assert(rendered.contains("inner-b"), s"Missing inner-b in:\n$rendered")
    }
  }

  group("MIO") {

    test("should be stack-safe with .flatMap") {
      def countdown(n: Int): MIO[Unit] =
        if (n <= 0) MIO.void
        else MIO(n - 1).flatMap(countdown)

      countdown(100000) === MIO.void
    }

    test("should be stack-safe with .scoped") {
      def countdown(n: Int): MIO[Unit] = MIO.scoped { runSafe =>
        if (n <= 0) ()
        else runSafe(countdown(n - 1))
      }

      // Before JDK 21, virtual threads are not available, we're falling back to normal threads and deep nesting is stack-safe
      // but can cause OutOfMemoryError.
      val n =
        if (JDKVersion.runtimeJDKVersion < JDKVersion(1, 21)) 1000
        else 100000

      countdown(n) === MIO.void
    }

    test("should not hang infinitely on FatalErrors") {
      val error = new OutOfMemoryError("Out of memory")

      MIO
        .scoped { runSafe =>
          runSafe(MIO.fail(error).as(10))
        }
        .unsafe
        .runSync
        ._2 === Left(NonEmptyVector(error))
    }
  }

  group("Instances for MIO") {

    group("should follow Functor laws") {
      functorLaws[MIO, Int]
    }

    group("should follow Applicative laws") {
      applicativeLaws[MIO, Int, String, Int]
    }

    group("should follow Parallel laws") {
      parallelLaws[MIO, Int, String]
    }

    group("should follow DirectStyle laws") {
      directStyleLaws[MIO, Int]
    }
  }

  group("MIO timeout") {

    def withTimeout[A](deadlineMillis: Long)(thunk: => A): A = {
      val prevDeadline = MIO.timeoutDeadlineNanos
      val prevScopes = MIO._openScopes
      MIO.timeoutDeadlineNanos = System.nanoTime() + deadlineMillis * 1000000L
      try thunk
      finally {
        MIO.timeoutDeadlineNanos = prevDeadline
        MIO._openScopes = prevScopes
      }
    }

    def infiniteLoop: MIO[Nothing] = MIO.defer(infiniteLoop)
    def loopForever[A]: MIO[A] = MIO.defer(loopForever)

    test("should throw MioTimeoutException when deadline is exceeded") {
      intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          infiniteLoop.unsafe.runSync
        }
      }
    }

    test("should capture MState with logs when timeout fires") {
      val mio = Log.info("before loop") >> infiniteLoop

      val ex = intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }

      val rendered = ex.capturedState.logs.render.fromInfo("root")
      assert(rendered.contains("before loop"), s"Expected 'before loop' in:\n$rendered")
      assert(rendered.contains("MIO timed out"), s"Expected timeout marker in:\n$rendered")
    }

    test("should preserve namedScopes in timeout state") {
      val prevBenchmark = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = true

        val mio = Log.namedScope("outer") {
          Log.namedScope("completed") {
            Log.info("A")
          } >> Log.namedScope("in-progress") {
            Log.info("B") >> infiniteLoop
          }
        }

        val ex = intercept[MIO.MioTimeoutException] {
          withTimeout(50) {
            mio.unsafe.runSync
          }
        }

        val logs = ex.capturedState.logs
        val scopes = logs.collect { case s: Log.Scope => s }
        val scopeNames = scopes.map(_.name)
        // All three scopes should be present (flat with parent IDs)
        assert(scopeNames.contains("outer"), s"Expected 'outer' scope, got: $scopeNames")
        assert(scopeNames.contains("completed"), s"Expected 'completed' scope, got: $scopeNames")
        assert(scopeNames.contains("in-progress"), s"Expected 'in-progress' scope, got: $scopeNames")
        // Outer scope should have timestamps (closed by timeout reconstruction)
        val outerScope = scopes.find(_.name == "outer").get
        assert(outerScope.start != Log.Timestamp.empty, "outer scope should have start timestamp")
        assert(outerScope.end != Log.Timestamp.empty, "outer scope should have end timestamp")
        // Completed and in-progress should be children of outer (via parentScopeId)
        val completedScope = scopes.find(_.name == "completed").get
        val inProgressScope = scopes.find(_.name == "in-progress").get
        assert(
          completedScope.parentScopeId == outerScope.scopeId,
          s"completed should be child of outer"
        )
        assert(
          inProgressScope.parentScopeId == outerScope.scopeId,
          s"in-progress should be child of outer"
        )
      } finally
        MIO.benchmarkScopes = prevBenchmark
    }

    test("should complete normally when computation finishes before timeout") {
      val mio = MIO.pure(42)
      val (_, result) = withTimeout(1000) {
        mio.unsafe.runSync
      }
      result === Right(42)
    }

    test("should not timeout when deadline is Long.MaxValue") {
      val prev = MIO.timeoutDeadlineNanos
      assert(prev == Long.MaxValue, "Default should be Long.MaxValue")
      val (_, result) = MIO.pure(42).unsafe.runSync
      result === Right(42)
    }

    test("should reset deadline to Long.MaxValue after timeout completes") {
      withTimeout(1000) {
        assert(MIO.timeoutDeadlineNanos != Long.MaxValue, "Deadline should be set during execution")
      }
      assert(
        MIO.timeoutDeadlineNanos == Long.MaxValue,
        s"Expected deadline to be reset to Long.MaxValue, got ${MIO.timeoutDeadlineNanos}"
      )
    }

    test("should reset deadline to Long.MaxValue after timeout fires") {
      ignore(intercept[MIO.MioTimeoutException] {
        withTimeout(1) {
          loopForever[Unit].unsafe.runSync
        }
      })
      assert(
        MIO.timeoutDeadlineNanos == Long.MaxValue,
        s"Expected deadline to be reset to Long.MaxValue after timeout, got ${MIO.timeoutDeadlineNanos}"
      )
    }

    test("should render flame graph from timeout state with preserved scopes") {
      val prevBenchmark = MIO.benchmarkScopes
      val prevStart = MIO.macroStartTimestamp
      try {
        MIO.benchmarkScopes = true
        MIO.macroStartTimestamp = Log.Timestamp.now

        val mio = Log.namedScope("root-scope") {
          Log.namedScope("fast-scope") {
            MIO.pure(())
          } >> Log.namedScope("slow-scope") {
            infiniteLoop
          }
        }

        val ex = intercept[MIO.MioTimeoutException] {
          withTimeout(50) {
            mio.unsafe.runSync
          }
        }

        val flameGraph = FlameGraph.renderSpeedscope("timeout-test", ex.capturedState.logs, MIO.macroStartTimestamp)
        assert(flameGraph.isDefined, "Expected flame graph from timeout state")
        val json = flameGraph.get
        assert(json.contains("\"name\": \"root-scope\""), s"Missing root-scope in flame graph:\n$json")
        assert(json.contains("\"name\": \"fast-scope\""), s"Missing fast-scope in flame graph:\n$json")
        assert(json.contains("\"name\": \"slow-scope\""), s"Missing slow-scope in flame graph:\n$json")
        // Verify events are present
        assert(json.contains("\"type\": \"O\""), "Missing open events")
        assert(json.contains("\"type\": \"C\""), "Missing close events")
      } finally {
        MIO.benchmarkScopes = prevBenchmark
        MIO.macroStartTimestamp = prevStart
      }
    }

    test("timeout should not be recoverable by handleErrorWith") {
      val mio = infiniteLoop.handleErrorWith { _ =>
        MIO.pure(42) // should never be reached
      }

      intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
    }

    test("timeout should not be recoverable by redeemWith") {
      val mio = loopForever[Int].redeemWith(_ => MIO.pure(42))(_ => MIO.pure(42))

      intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
    }

    test("timeout should not be recoverable by recover") {
      val mio = loopForever[Int].recover { case _ => 42 }

      intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
    }

    test("timeout should not be recoverable by orElse") {
      val mio = loopForever[Int].orElse(MIO.pure(42))

      intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
    }

    test("timeout should propagate through MIO.scoped / runSafe") {
      val mio = MIO.scoped { runSafe =>
        runSafe(loopForever[Int]) // timeout fires inside runSafe
      }

      intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
    }

    test("timeout should propagate through MIO.defer and not be caught by NonFatal") {
      // Verify that timeout inside a nested MIO.apply propagates through defer's NonFatal handler
      val mioWithTimeout = MIO.scoped { runSafe =>
        runSafe(Log.info("before loop"))
        runSafe(loopForever[String])
      }

      val ex = intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mioWithTimeout.unsafe.runSync
        }
      }
      val rendered = ex.capturedState.logs.render.fromInfo("root")
      assert(rendered.contains("before loop"), "Logs before timeout should be captured")
    }

    test("timeout with MLocal simulating ValDefsCache pattern: forward-declare then build") {
      // Simulate the ValDefsCache pattern:
      // 1. Forward-declare a cache entry (definition = None)
      // 2. Start building it (run more MIO to compute the body)
      // 3. Timeout fires during build
      // 4. Verify timeout propagates cleanly — NOT swallowed by error handling

      val cache = MLocal.unsafeSharedParallel(Map.empty[String, Option[String]])

      // Build first entry normally, then loop forever during second entry
      val buildFirst: MIO[Unit] = for {
        c1 <- cache.get
        _ <- cache.set(c1.updated("derivedShow", Some("built")))
      } yield ()

      val buildSecondForever: MIO[Unit] = Log.namedScope("building derivedEq") {
        Log.info("started building") >> loopForever[Unit]
      }

      val mio = for {
        // Step 1: Forward declare — like ValDefsCache.forwardDeclare
        _ <- cache.set(Map("derivedShow" -> None, "derivedEq" -> None))
        // Step 2: Build first entry — completes normally
        _ <- buildFirst
        // Step 3: Build second entry — infinite loop simulates slow derivation
        _ <- buildSecondForever
      } yield ()

      val ex = intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }

      // Verify the cache state: derivedShow is built, derivedEq is still None (incomplete)
      val cacheState = ex.capturedState.get(cache)
      assert(
        cacheState.get("derivedShow").contains(Some("built")),
        s"derivedShow should be built, got: ${cacheState.get("derivedShow")}"
      )
      assert(
        cacheState.get("derivedEq").contains(None),
        s"derivedEq should be incomplete (None), got: ${cacheState.get("derivedEq")}"
      )
    }

    test("timeout inside buildCachedWith-like pattern should not produce partial results") {
      // Simulate a more realistic pattern: forward-declare, then use runSafe to build
      val cache = MLocal.unsafeSharedParallel(Map.empty[String, Option[String]])

      val mio = MIO.scoped { runSafe =>
        // Forward declare
        runSafe(cache.set(Map("key" -> None)))

        // Start building — this will timeout
        val result: String =
          try
            runSafe(loopForever[String])
          catch {
            // A user might try to catch and continue — this MUST NOT work for timeout
            case scala.util.control.NonFatal(_) => "recovered"
          }

        // If we somehow got here (we shouldn't), finalize the cache
        runSafe(cache.set(Map("key" -> Some(result))))
      }

      // Timeout should propagate, not be caught by the NonFatal handler
      intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
    }

    test("timeout should work with DirectStyle runSafe and namedScope") {
      val prevBenchmark = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = true

        val mio = MIO.scoped { runSafe =>
          runSafe(Log.info("step 1"))
          runSafe(Log.namedScope("slow-scope") {
            Log.info("entered slow scope") >> loopForever[Unit]
          })
        }

        val ex = intercept[MIO.MioTimeoutException] {
          withTimeout(100) {
            mio.unsafe.runSync
          }
        }

        // Verify logs and scope reconstruction
        val rendered = ex.capturedState.logs.render.fromInfo("root")
        assert(rendered.contains("step 1"), s"Expected 'step 1' in:\n$rendered")
        assert(rendered.contains("slow-scope"), s"Expected 'slow-scope' in:\n$rendered")
        assert(rendered.contains("entered slow scope"), s"Expected 'entered slow scope' in:\n$rendered")
      } finally
        MIO.benchmarkScopes = prevBenchmark
    }

    test("timeout should work with multiple sequential runSafe calls") {
      val mio = MIO.scoped { runSafe =>
        // First call completes normally
        val a = runSafe(MIO.pure(1))
        // Second call times out
        val b = runSafe(loopForever[Int])
        a + b // should never be reached
      }

      val ex = intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
      assert(ex.capturedState != null, "State should be captured")
    }

    test("timeout should work with DirectStyle and MLocal state") {
      val local = MLocal.unsafeSharedParallel(0)

      val mio = MIO.scoped { runSafe =>
        runSafe(local.set(42))
        val v = runSafe(local.get)
        assert(v == 42)
        runSafe(loopForever[Unit])
      }

      val ex = intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }
      // MLocal value should be captured in the timeout state
      val localValue = ex.capturedState.get(local)
      assert(localValue == 42, s"Expected MLocal value 42, got $localValue")
    }

    test("timeout error message should be clear, not misleading 'not found' from incomplete cache") {
      val cache = MLocal.unsafeSharedParallel(Map.empty[String, Option[String]])

      val mio = cache.set(Map("typeclass" -> None)) >> loopForever[Unit]

      val ex = intercept[MIO.MioTimeoutException] {
        withTimeout(50) {
          mio.unsafe.runSync
        }
      }

      // The exception message should say "timed out", not something about missing definitions
      assert(
        ex.prettyPrintedMessage.contains("MIO timed out"),
        s"Expected 'MIO timed out' in message, got:\n${ex.prettyPrintedMessage}"
      )
      // The captured state should have the timeout marker log
      val rendered = ex.capturedState.logs.render.fromError("root")
      assert(rendered.contains("MIO timed out"), s"Expected timeout marker in logs:\n$rendered")
    }
  }
}
