package hearth
package crossquotes

/** Regression smoke test for hearth#317/#318 (Scala 3). See [[Issue317Fixtures]] for the mechanisms; the full
  * `-Xcheck-macros` failures reproduce only across separate compilation units (repros linked from the PR).
  */
final class Issue317Spec extends MacroSuite {

  group("Cross-Quotes (Scala 3): derive-in-splice owner + entry-ctx (hearth#317/#318)") {

    test("a createVal body built inside a nested-splice def body produces working code") {
      Issue317Fixtures.deriveWithVal.show(42) <==> "v:42"
    }

    test("two instances sharing a withMacroEntryCtx-pinned cache produce working code") {
      val (a, b) = Issue317Fixtures.deriveTwoWithCache
      a.show(1) <==> "1!"
      b.show(2) <==> "2!"
    }
  }
}
