package hearth

import munit.{Location, TestOptions}

import scala.util.matching.Regex

trait MacroSuite extends munit.BaseFunSuite { self =>

  private var prefix = ""

  private def appendName(prefix: String, name: String): String = if (prefix.isEmpty) name else s"$prefix / $name"

  def group(name: String)(body: => Any): Unit = {
    val oldPrefix = prefix
    prefix = appendName(prefix, name)
    val _ = body
    prefix = oldPrefix
  }

  override def test(name: String)(body: => Any)(implicit loc: Location): Unit =
    super.test(appendName(prefix, name))(body)

  override def test(options: TestOptions)(body: => Any)(implicit loc: Location): Unit =
    if (options.name.startsWith(prefix)) super.test(options)(body)
    else super.test(options.withName(appendName(prefix, options.name)))(body)

  implicit class ArrowAssert(lhs: Any) {
    def ==>[V](rhs: V): Unit =
      (lhs, rhs) match {
        // Hack to make Arrays compare sanely; at some point we may want some
        // custom, extensible, typesafe equality check but for now this will do
        case (lhs: Array[?], rhs: Array[?]) =>
          Predef.assert(lhs.toSeq == rhs.toSeq, s"==> assertion failed: ${lhs.toSeq} != ${rhs.toSeq}")
        case (lhs, rhs) =>
          Predef.assert(lhs == rhs, s"==> assertion failed: $lhs != $rhs")
      }
  }

  implicit class StringOps(private val str: String) {
    def stripANSI: String = MacroSuite.AnsiControlCode.replaceAllIn(str, "")
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
object MacroSuite {

  val AnsiControlCode: Regex = "\u001b\\[([0-9]+)m".r
}
