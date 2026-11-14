package hearth
package crossquotes

/** Fixtures are in [[CrossByNameSpliceFixturesImpl]]. A case class compiled in the same run as the expansion is covered
  * by [[CrossByNameSpliceCrashSpec]].
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
