package hearth

import hearth.crossquotes.Issue320ReproFixtures

/** Regression for hearth#320: on Scala 2, object/companion references inside an `Expr.quote` from one compilation scope
  * must survive being re-materialized at a foreign expansion site.
  *
  * This spec lives in package `hearth` while the quoted `CrossUnitWrapper`/`CrossUnitSupport` objects (and the fixture
  * that quotes them) live in package `hearth.crossquotes`, and only the fixture object is imported here — so the quoted
  * short names (`CrossUnitWrapper.wrap`, `CrossUnitSupport.describe`) are NOT in scope at this expansion site. Before
  * the fix the reified Scala 2 source used those short names and failed to compile here; the fix fully-qualifies them
  * as `_root_.hearth.crossquotes.…`. Scala 3 quotes are hygienic and already pass.
  */
final class Issue320Spec extends MacroSuite {

  group("Cross-Quotes (Scala 2 cross-unit hygiene, hearth#320)") {

    test("object/companion references in a quote resolve at a foreign-package expansion site") {
      Issue320ReproFixtures.testCrossUnitQuote("hi") <==> "wrapped(hi)"
    }
  }
}
