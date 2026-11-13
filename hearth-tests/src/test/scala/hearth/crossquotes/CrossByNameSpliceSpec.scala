package hearth
package crossquotes

/** Fixtures are in [[CrossByNameSpliceFixturesImpl]].
  *
  * Known gap (Scala 2): splicing the call of a case class' default getter (from `Parameter.defaultValue`) into a
  * by-name parameter, when the case class is compiled in the same run, compiles, but crashes the compiler's backend
  * ("an unexpected type representation reached the compiler backend ... <error>") while emitting the file expanding the
  * macro. It cannot be an expected failure (the whole test file stops compiling), so it lives in
  * `CrossByNameSpliceCrashSpec.scala.disabled`: rename it to `.scala` to reproduce it. The tests below are the variants
  * that work.
  */
final class CrossByNameSpliceSpec extends MacroSuite {

  group("Cross-Quotes macro/plugin: splicing into a by-name parameter") {

    test("a default getter's call spliced into a strict parameter works") {
      CrossByNameSpliceFixtures.testDefaultsSplicedIntoStrict[CaseClassWithDefaults] ==> List[Any](7, Some("n"))
    }

    test("an upcast literal spliced into a by-name parameter works") {
      CrossByNameSpliceFixtures.testUpcastLiteralSplicedIntoByName ==> List[Any](7)
    }
  }
}

final case class CaseClassWithDefaults(firstName: String, age: Int = 7, nick: Option[String] = Some("n"))
