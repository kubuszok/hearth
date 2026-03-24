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
        // Should have a single outer scope wrapping everything
        assert(logs.size == 1, s"Expected single outer scope, got ${logs.size} entries:\n${logs.mkString("\n")}")
        logs.head match {
          case Log.Scope("outer", entries, start, end) =>
            assert(start != Log.Timestamp.empty, "outer scope should have start timestamp")
            assert(end != Log.Timestamp.empty, "outer scope should have end timestamp")
            val hasCompleted = entries.exists {
              case Log.Scope("completed", _, _, _) => true
              case _                               => false
            }
            assert(hasCompleted, s"Expected 'completed' scope inside outer, got:\n${entries.mkString("\n")}")
            val hasInProgress = entries.exists {
              case Log.Scope("in-progress", _, _, _) => true
              case _                                 => false
            }
            assert(hasInProgress, s"Expected 'in-progress' scope inside outer, got:\n${entries.mkString("\n")}")
          case other =>
            fail(s"Expected outer scope, got: $other")
        }
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

    test("should save and restore previous deadline") {
      val outerDeadline = System.nanoTime() + 999999999999L
      MIO.timeoutDeadlineNanos = outerDeadline
      try {
        withTimeout(50) {
          // Inner deadline is set
          assert(MIO.timeoutDeadlineNanos != outerDeadline)
        }
        // Outer deadline is restored
        assert(
          MIO.timeoutDeadlineNanos == outerDeadline,
          s"Expected outer deadline to be restored, got ${MIO.timeoutDeadlineNanos}"
        )
      } finally
        MIO.timeoutDeadlineNanos = Long.MaxValue
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
  }
}
