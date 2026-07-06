package hearth

/** Base trait for macro tests - extend it (instead of [[Suite]]) when a test needs to assert on compilation errors.
  *
  * Provides some utilities like:
  *   - `compileErrors("code").check(expectedLinesInExpectedOrder*)` - asserting that the error message contains all of
  *     the `expected` strings in order (lines do not have to be consecutive, some lines can be skipped from comparison)
  *   - `compileErrors("code").checkNot(absentLines*)` - asserting that the error message does not contain any of the
  *     `absent` strings
  *   - `compileErrors("code").arePresent()` - asserting that the error message is not empty
  *
  * @see
  *   docs/contributing/guidelines-for-tests.md
  *
  * @since 0.3.0
  */
trait MacroSuite extends Suite {

  implicit class CompileErrorsCheck(private val msg: String) {

    /** Asserts the (color-stripped) error message contains all `msgs` in order (gaps between them are allowed).
      * @since 0.3.0
      *
      * @param msgs
      *   the expected snippets, in the order they should appear
      */
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

    /** Asserts that none of `msgs` appear in the (color-stripped) error message.
      * @since 0.3.0
      *
      * @param msgs
      *   the snippets that must be absent
      */
    def checkNot(msgs: String*): Unit = {
      val msgNoColors = msg.stripANSI
      for (msg <- msgs)
        Predef.assert(
          !msgNoColors.contains(msg),
          s"""Error message contained a snippet that was not expected to be there
             |Error message:
             |${this.msg}
             |Not Expected Snippet:
             |$msg""".stripMargin
        )
    }

    /** Asserts that the error message is non-empty (i.e. compilation actually failed).
      * @since 0.3.0
      */
    def arePresent(): Unit = Predef.assert(msg.nonEmpty, "Expected compilation errors")
  }
}
