package hearth
package crossquotes

/** Rename to `.scala` to reproduce: on Scala 2 compiling this file crashes the compiler's backend with
  * {{{
  * an unexpected type representation reached the compiler backend while compiling CrossByNameSpliceCrashSpec.scala: <error>.
  * Error while emitting CrossByNameSpliceCrashSpec.scala
  * <error> (of class scala.reflect.internal.Types$ErrorType$)
  * }}}
  * Scala 3 passes. The case class has to be compiled in the same run as the expansion (here: the same file): with a
  * case class read from a class file (e.g. [[CaseClassWithDefaults]] after an incremental compilation) the same calls
  * compile, while in a clean build it crashes too. See [[CrossByNameSpliceSpec]] for the variants that work.
  */
final class CrossByNameSpliceCrashSpec extends MacroSuite {

  test("a default getter's call spliced into a by-name parameter, upcast to Any") {
    CrossByNameSpliceFixtures.testDefaultsSplicedIntoByName[CaseClassWithDefaultsInTheSameFile] ==> List[Any](7, Some("n"))
  }

  test("a default getter's call spliced into a by-name parameter, as it is") {
    CrossByNameSpliceFixtures.testDefaultsWithoutUpcastIntoByName[CaseClassWithDefaultsInTheSameFile] ==> List[Any](7, Some("n"))
  }
}

final case class CaseClassWithDefaultsInTheSameFile(firstName: String, age: Int = 7, nick: Option[String] = Some("n"))
