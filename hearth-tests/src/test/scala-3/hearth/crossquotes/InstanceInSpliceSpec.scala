package hearth
package crossquotes

/** Smoke test only: the `-Xcheck-macros` failures (a val derived inside a splice getting the wrong owner, a value
  * cached during one splice evaluation reused in the next) reproduce only across separate compilation units, so here we
  * only check that the generated code works.
  */
final class InstanceInSpliceSpec extends MacroSuite {

  group("Cross-Quotes (Scala 3): derive-in-splice owner + entry-ctx") {

    test("a createVal body built inside a nested-splice def body produces working code") {
      InstanceInSpliceFixtures.deriveWithVal.show(42) <==> "v:42"
    }

    test("two instances sharing a withMacroEntryCtx-pinned cache produce working code") {
      val (a, b) = InstanceInSpliceFixtures.deriveTwoWithCache
      a.show(1) <==> "1!"
      b.show(2) <==> "2!"
    }
  }
}
