package hearth

import hearth.data.*
import hearth.fp.ignore
import munit.{Location, TestOptions}

/** Base trait for all Hearth tests - extend it (or [[MacroSuite]]) to write macro tests.
  *
  * Provides some utilities like:
  *   - `group(name) { ... }` - grouping tests under a common name
  *   - `test("name".ignoreOnScala2_13) { ... }` - ignoring test on Scala 2.13
  *   - `test("name".ignoreOnScala3) { ... }` - ignoring test on Scala 3
  *   - `test("name".ignoreOnJvm) { ... }` - ignoring test on JVM
  *   - `test("name".ignoreOnJs) { ... }` - ignoring test on JS
  *   - `test("name".ignoreOnNative) { ... }` - ignoring test on Native
  *   - `actual ==> expected` - asserting that `actual` is equal to `expected`, simply failing test if they don't
  *   - `actual <==> expected` - comparing `actual` and `expected` (when they are both [[String]] or both [[Data]]), and
  *     showing an error with a [[Diff]] if they aren't equal
  *
  * @see
  *   docs/contributing/guidelines-for-tests.md
  * @since 0.3.0
  */
trait Suite extends munit.BaseFunSuite { self =>

  private var prefix = ""

  private def appendName(prefix: String, name: String): String = if (prefix.isEmpty) name else s"$prefix / $name"

  /** Nests the `test`s registered inside `body` under a `<name> / ...` prefix.
    *
    * Groups may be nested; each level prepends its name to the prefix.
    *
    * @param name
    *   group label prepended to contained test names
    * @param body
    *   block that registers the grouped tests
    * @since 0.3.0
    */
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

  implicit class StringOps(private val str: String) {
    def stripANSI: String = removeAnsiColors(str)

    private def language = LanguageVersion.byHearth
    def ignoreOnScala2_13(implicit loc: Location): TestOptions =
      if (language.isScala2_13) str.ignore else TestOptions(str)
    def ignoreOnScala3(implicit loc: Location): TestOptions = if (language.isScala3) str.ignore else TestOptions(str)
    private def platform = Platform.byHearth
    def ignoreOnJvm(implicit loc: Location): TestOptions = if (platform.isJvm) str.ignore else TestOptions(str)
    def ignoreOnJs(implicit loc: Location): TestOptions = if (platform.isJs) str.ignore else TestOptions(str)
    def ignoreOnNative(implicit loc: Location): TestOptions = if (platform.isNative) str.ignore else TestOptions(str)
  }

  implicit class TestOptionsOps(private val options: TestOptions) {
    private def language = LanguageVersion.byHearth
    def ignoreOnScala2_13: TestOptions = if (language.isScala2_13) options.ignore else options
    def ignoreOnScala3: TestOptions = if (language.isScala3) options.ignore else options
    private def platform = Platform.byHearth
    def ignoreOnJvm: TestOptions = if (platform.isJvm) options.ignore else options
    def ignoreOnJs: TestOptions = if (platform.isJs) options.ignore else options
    def ignoreOnNative: TestOptions = if (platform.isNative) options.ignore else options
  }

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

    /** Asserts that `actual` structurally equals `expected`, failing with a rendered [[Diff]] on mismatch.
      *
      * The preferred way to assert macro results in Hearth: compare whole [[Data]] structures (e.g.
      * `result <==> Data.map("f" -> Data(1))`) so failures show a field-by-field diff instead of an opaque `!=`.
      *
      * @param expected
      *   the expected [[Data]]
      * @param loc
      *   source location for the failure message
      * @see
      *   docs/contributing/guidelines-for-tests.md
      * @since 0.3.0
      */
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

    /** Line-by-line comparison of two [[String]]s (ANSI-insensitive), reporting the differing lines.
      *
      * @param expected
      *   the expected string
      * @param loc
      *   source location for the failure message
      * @since 0.3.0
      */
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
}
