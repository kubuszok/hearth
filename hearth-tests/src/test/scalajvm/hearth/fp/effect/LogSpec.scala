package hearth
package fp
package effect

final class LogSpec extends ScalaCheckSuite {

  group("Log") {

    test("should be lazy") {

      var used = false

      val state = Log
        .info {
          used = true
          "Hello"
        }
        .unsafe
        .runSync
        ._1

      used === false
      ignore(state.logs.render.fromInfo("root"))
      used === true
    }

    test("should allow filtering levels before rendering") {

      val mio = Log.info("Info-level log") >> Log.warn("Warn-level log") >> Log.error("Error-level log")

      val state = mio.unsafe.runSync._1

      state.logs.render.fromInfo("root") === """root:
                                               |├ [Info]  Info-level log
                                               |├ [Warn]  Warn-level log
                                               |└ [Error] Error-level log""".stripMargin
      state.logs.render.fromWarn("root") === """root:
                                               |├ [Warn]  Warn-level log
                                               |└ [Error] Error-level log""".stripMargin
      state.logs.render.fromError("root") === """root:
                                                |└ [Error] Error-level log""".stripMargin
    }

    test("should allow nested logs") {

      val mio = Log.info("Info-level log") >> Log.warn("Warn-level log") >> Log.error("Error-level log")

      val mio2 = Log.namedScope("example1") {
        mio
      } >> Log.namedScope("example2") {
        mio
      }

      val state = mio2.unsafe.runSync._1

      state.logs.render.fromInfo("root") === """root:
                                               |├ example1:
                                               |│ ├ [Info]  Info-level log
                                               |│ ├ [Warn]  Warn-level log
                                               |│ └ [Error] Error-level log
                                               |└ example2:
                                               |  ├ [Info]  Info-level log
                                               |  ├ [Warn]  Warn-level log
                                               |  └ [Error] Error-level log""".stripMargin
    }

    test("should display scope durations when benchmarkScopes is enabled") {

      val prev = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = true

        val mio = Log.namedScope("example1") {
          Log.info("Info-level log")
        } >> Log.namedScope("example2") {
          Log.warn("Warn-level log")
        }

        val state = mio.unsafe.runSync._1

        val rendered = state.logs.render.fromInfo("root")
        // Scopes should have duration annotations like "(123ns)"
        assert(rendered.contains("example1 ("), s"Expected duration annotation in example1, got:\n$rendered")
        assert(rendered.contains("ns):"), s"Expected 'ns):' in rendered output, got:\n$rendered")
        assert(rendered.contains("example2 ("), s"Expected duration annotation in example2, got:\n$rendered")
      } finally
        MIO.benchmarkScopes = prev
    }

    test("should not display scope durations when benchmarkScopes is disabled") {

      val prev = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = false

        val mio = Log.namedScope("example1") {
          Log.info("Info-level log")
        }

        val state = mio.unsafe.runSync._1

        val rendered = state.logs.render.fromInfo("root")
        assert(!rendered.contains("ns)"), s"Expected no duration annotation, got:\n$rendered")
        rendered === """root:
                       |└ example1:
                       |  └ [Info]  Info-level log""".stripMargin
      } finally
        MIO.benchmarkScopes = prev
    }

    test("should handle multiline messages and named scopes") {

      val mio =
        Log.info("""Hello
                   |World""".stripMargin) >>
          Log.warn("""Hello
                     |World""".stripMargin) >>
          Log.error("""Hello
                      |World""".stripMargin)

      val mio2 = Log.namedScope("Example\n1") {
        mio
      } >> Log.namedScope("Example\n2") {
        mio
      }

      val state = mio2.unsafe.runSync._1

      state.logs.render.fromInfo("root") === """root:
                                               |├ Example
                                               |│ 1:
                                               |│ ├ [Info]  Hello
                                               |│ │         World
                                               |│ ├ [Warn]  Hello
                                               |│ │         World
                                               |│ └ [Error] Hello
                                               |│           World
                                               |└ Example
                                               |  2:
                                               |  ├ [Info]  Hello
                                               |  │         World
                                               |  ├ [Warn]  Hello
                                               |  │         World
                                               |  └ [Error] Hello
                                               |            World""".stripMargin
    }
  }

  group("FlameGraph") {

    test("should render speedscope JSON for benchmarked scopes") {

      val prev = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = true

        val macroStart = Log.Timestamp.now

        val mio = Log.namedScope("outer") {
          Log.namedScope("inner1") {
            Log.info("msg1")
          } >> Log.namedScope("inner2") {
            Log.info("msg2")
          }
        }

        val state = mio.unsafe.runSync._1
        val result = FlameGraph.renderSpeedscope("test-macro", state.logs, macroStart)

        assert(result.isDefined, "Expected flame graph JSON to be generated")
        val json = result.get

        // Verify basic structure
        assert(json.contains("\"$schema\": \"https://www.speedscope.app/file-format-schema.json\""), "Missing schema")
        assert(json.contains("\"name\": \"test-macro\""), "Missing profile name")
        assert(json.contains("\"unit\": \"nanoseconds\""), "Missing unit")
        assert(json.contains("\"type\": \"evented\""), "Missing profile type")

        // Verify frames - outer, inner1, inner2
        assert(json.contains("\"name\": \"outer\""), "Missing outer frame")
        assert(json.contains("\"name\": \"inner1\""), "Missing inner1 frame")
        assert(json.contains("\"name\": \"inner2\""), "Missing inner2 frame")

        // Verify events (O/C pairs)
        assert(json.contains("\"type\": \"O\""), "Missing open events")
        assert(json.contains("\"type\": \"C\""), "Missing close events")
      } finally
        MIO.benchmarkScopes = prev
    }

    test("should return None when no benchmarked scopes exist") {

      val macroStart = Log.Timestamp.now

      val mio = Log.info("just a message")
      val state = mio.unsafe.runSync._1
      val result = FlameGraph.renderSpeedscope("test-macro", state.logs, macroStart)

      assert(result.isEmpty, "Expected None for logs without benchmarked scopes")
    }

    test("should return None when scopes have empty timestamps") {

      val prev = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = false

        val macroStart = Log.Timestamp.now

        val mio = Log.namedScope("example") {
          Log.info("msg")
        }

        val state = mio.unsafe.runSync._1
        val result = FlameGraph.renderSpeedscope("test-macro", state.logs, macroStart)

        assert(result.isEmpty, "Expected None for scopes without timestamps")
      } finally
        MIO.benchmarkScopes = prev
    }

    test("should produce events with open/close pairs for nested scopes") {

      val prev = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = true

        val macroStart = Log.Timestamp.now

        val mio = Log.namedScope("a") {
          Log.namedScope("b") {
            MIO.pure(())
          }
        }

        val state = mio.unsafe.runSync._1
        val result = FlameGraph.renderSpeedscope("test", state.logs, macroStart)

        assert(result.isDefined, "Expected flame graph JSON")
        val json = result.get

        // Verify that both frames exist and have open/close events
        val eventPattern = """"type": "(O|C)", "at": (\d+), "frame": (\d+)""".r
        val events = eventPattern.findAllMatchIn(json).map(m => (m.group(1), m.group(3).toInt)).toVector

        assert(events.nonEmpty, "Expected events")
        // Each frame should have matching open/close counts
        val frameEvents = events.groupBy(_._2)
        frameEvents.foreach { case (frame, evts) =>
          val opens = evts.count(_._1 == "O")
          val closes = evts.count(_._1 == "C")
          assert(opens == closes, s"Frame $frame has $opens opens and $closes closes")
        }
      } finally
        MIO.benchmarkScopes = prev
    }

    test("should escape special characters in scope names") {

      val prev = MIO.benchmarkScopes
      try {
        MIO.benchmarkScopes = true

        val macroStart = Log.Timestamp.now

        val mio = Log.namedScope("scope with \"quotes\" and \\backslash") {
          MIO.pure(())
        }

        val state = mio.unsafe.runSync._1
        val result = FlameGraph.renderSpeedscope("test", state.logs, macroStart)

        assert(result.isDefined)
        val json = result.get
        assert(json.contains("\\\"quotes\\\""), s"Expected escaped quotes in:\n$json")
        assert(json.contains("\\\\backslash"), s"Expected escaped backslash in:\n$json")
      } finally
        MIO.benchmarkScopes = prev
    }
  }
}
