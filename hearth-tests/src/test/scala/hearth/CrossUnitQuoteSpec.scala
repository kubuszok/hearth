package hearth

import hearth.crossquotes.CrossUnitQuoteFixtures

/** On Scala 2, object/companion references inside an `Expr.quote` from one compilation scope must survive being
  * re-materialized at a foreign expansion site.
  *
  * This spec lives in package `hearth` while the quoted `CrossUnitWrapper`/`CrossUnitSupport` objects (and the fixture
  * that quotes them) live in package `hearth.crossquotes`, and only the fixture object is imported here — so the quoted
  * short names (`CrossUnitWrapper.wrap`, `CrossUnitSupport.describe`) are NOT in scope at this expansion site, and the
  * reified Scala 2 source must fully-qualify them. Scala 3 quotes are hygienic.
  */
final class CrossUnitQuoteSpec extends MacroSuite {

  group("Cross-Quotes (Scala 2 cross-unit hygiene)") {

    test("object/companion references in a quote resolve at a foreign-package expansion site") {
      CrossUnitQuoteFixtures.testCrossUnitQuote("hi") <==> "wrapped(hi)"
    }
  }
}
