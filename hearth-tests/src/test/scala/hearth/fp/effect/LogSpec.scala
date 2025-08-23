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
}
