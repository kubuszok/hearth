package hearth
package crossquotes

/** Default values of a case class compiled in the same run as the expansion (here: the same file). In that run, on
  * Scala 2, its companion's `apply$default$N` getters can still have an erroneous type, and a call to one spliced into
  * a by-name parameter crashes the compiler's backend. See [[CrossByNameSpliceSpec]] for the other variants.
  */
final class CrossByNameSpliceCrashSpec extends MacroSuite {

  test("a default getter's call spliced into a by-name parameter, upcast to Any") {
    CrossByNameSpliceFixtures
      .testDefaultsSplicedIntoByName[CaseClassWithDefaultsInTheSameFile] ==> List[Any](7, Some("n"))
  }

  test("a default getter's call spliced into a by-name parameter, as it is") {
    CrossByNameSpliceFixtures
      .testDefaultsWithoutUpcastIntoByName[CaseClassWithDefaultsInTheSameFile] ==> List[Any](7, Some("n"))
  }
}

final case class CaseClassWithDefaultsInTheSameFile(firstName: String, age: Int = 7, nick: Option[String] = Some("n"))
