package hearth

/** Base trait for all macro tests.
  *
  * Provides some utilities like:
  *   - `compileErrors("code").check(expectedLinesInExpectedOrder*)` - asserting that the error message contains all of
  *     the `expected` strings in order (lines do not have to be consequtive, some lines can be skipped from comparison)
  *   - `compileErrors("code").checkNot(absentLines*)` - asserting that the error message does not contain any of the
  *     `absent` strings
  *   - `compileErrors("code").arePresent()` - asserting that the error message is not empty
  */
trait MacroSuite extends Suite {

  implicit class CompileErrorsCheck(private val msg: String) {

    def check(msgs: String*): Unit = {
      val msgNoColors = msg.stripANSI
      var lastChar = 0
      for (msg <- msgs) {
        lastChar = msgNoColors.indexOf(msg, lastChar)
        Predef.assert(
          0 <= lastChar,
          s"""Error message did not contain expected snippet
             |Error message:
             |${this.msg}
             |Expected Snippet:
             |$msg""".stripMargin
        )
      }
    }

    def checkNot(msgs: String*): Unit = {
      val msgNoColors = msg.stripANSI
      for (msg <- msgs)
        Predef.assert(
          !msgNoColors.contains(msg),
          s"""Error message contain snippet that was expected to be not there
             |Error message:
             |${this.msg}
             |Not Expected Snippet:
             |$msg""".stripMargin
        )
    }

    def arePresent(): Unit = Predef.assert(msg.nonEmpty, "Expected compilation errors")
  }
}
