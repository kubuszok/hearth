package hearth

import hearth.data.*
import hearth.fp.ignore
import munit.{Location, TestOptions}

import scala.util.matching.Regex

/** Base trait for all Hearth tests.
  *
  * Provides some utilities like:
  *   - `group(name) { ... }` - grouping tests under a common name
  *   - `actual ==> expected` - asserting that `actual` is equal to `expected`, simply failing test if they don't
  *   - `actual <==> expected` - comparing `actual` and `expected` (when they are both [[String]] or both [[Data]]), and
  *     showing an error with a [[Diff]] if they aren't equal
  *   - `compileErrors("code").check(expectedLinesInExpectedOrder*)` - asserting that the error message contains all of
  *     the `expected` strings in order (lines do not have to be consequtive, some lines can be skipped from comparison)
  *   - `compileErrors("code").checkNot(absentLines*)` - asserting that the error message does not contain any of the
  *     `absent` strings
  *   - `compileErrors("code").arePresent()` - asserting that the error message is not empty
  */
trait Suite extends munit.BaseFunSuite { self =>

  private var prefix = ""

  private def appendName(prefix: String, name: String): String = if (prefix.isEmpty) name else s"$prefix / $name"

  def group(name: String)(body: => Any): Unit = {
    val oldPrefix = prefix
    prefix = appendName(prefix, name)
    ignore(body)
    prefix = oldPrefix
  }

  override def test(name: String)(body: => Any)(implicit loc: Location): Unit =
    super.test(appendName(prefix, name))(body)

  override def test(options: TestOptions)(body: => Any)(implicit loc: Location): Unit =
    if (options.name.startsWith(prefix)) super.test(options)(body)
    else super.test(options.withName(appendName(prefix, options.name)))(body)

  implicit class ArrowAssert(actual: Any) {
    def ==>[V](expected: V)(implicit loc: Location): Unit =
      (actual, expected) match {
        // Hack to make Arrays compare sanely; at some point we may want some
        // custom, extensible, typesafe equality check but for now this will do
        case (actual: Array[?], expected: Array[?]) =>
          Predef.assert(
            actual.toSeq == expected.toSeq,
            s"""${Console.RED}==> assertion failed${Console.RESET}:
               |  ${actual.toSeq} ${Console.RED}!=${Console.RESET} ${expected.toSeq}
               |${Console.RED}at $loc${Console.RESET}
               |""".stripMargin
          )
        case (actual, expected) =>
          Predef.assert(
            actual == expected,
            s"""${Console.RED}==> assertion failed${Console.RESET}:
               |  $actual ${Console.RED}!=${Console.RESET} $expected
               |${Console.RED}at $loc${Console.RESET}
               |""".stripMargin
          )
      }
  }

  implicit class DataAssert(actual: Data) {
    def <==>(expected: Data)(implicit loc: Location): Unit = {
      val diff = actual.diff(expected)
      Predef.assert(
        diff.isEmpty,
        s"""${Console.RED}<==> assertion failed (diff from expected)${Console.RESET}:
           |${diff.sorted.render}
           |${Console.RED}at $loc${Console.RESET}
           |""".stripMargin
      )
    }
  }

  implicit class ExpectedMsgAssert(actual: String) {
    def <==>(expected: String)(implicit loc: Location): Unit = {
      val diff = actual.split("\n").zipAll(expected.split("\n"), "", "").flatMap { case (l, r) =>
        if (l.stripANSI != r.stripANSI) List(l -> r)
        else List.empty
      }
      Predef.assert(
        diff.isEmpty,
        s"""${Console.RED}<==> assertion failed${Console.RESET}:
           |${diff.map { case (l, r) => s"  $l ${Console.RED}!=${Console.RESET} $r" }.mkString("\n")}
           |${Console.RED}at $loc${Console.RESET}
           |""".stripMargin
      )
    }
  }

  implicit class StringOps(private val str: String) {
    def stripANSI: String = Suite.AnsiControlCode.replaceAllIn(str, "")
  }

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
object Suite {

  val AnsiControlCode: Regex = "\u001b\\[([0-9]+)m".r
}
