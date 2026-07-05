package hearth
package crossquotes

import hearth.data.Data

/** Cross-platform (2.13 + 3) tests that `Type.CtorN.UpperBounded.of` / `Type.CtorN.Bounded.of` expand and match on BOTH
  * platforms (hearth#344 — the Scala 3 cross-quotes plugin previously only rewrote the 2-select `Type.CtorN.of`).
  */
final class BoundedCtorSpec extends MacroSuite {

  group("Cross-Quotes: Type.CtorN.UpperBounded.of / Bounded.of expand on both platforms (#344)") {

    test("Ctor1.UpperBounded.of matches F1[SubMarker]") {
      BoundedCtorFixtures.testUpperBounded1[BoundedCtors.F1[BoundedCtors.SubMarker]] <==> Data(true)
    }

    test("Ctor1.UpperBounded.of does not match an unrelated constructor") {
      BoundedCtorFixtures.testUpperBounded1[BoundedCtors.G1[BoundedCtors.SubMarker]] <==> Data(false)
    }

    test("Ctor2.UpperBounded.of matches F2") {
      BoundedCtorFixtures
        .testUpperBounded2[BoundedCtors.F2[BoundedCtors.SubMarker, BoundedCtors.SubMarker]] <==> Data(true)
    }

    test("Ctor1.Bounded.of matches G1") {
      BoundedCtorFixtures.testBounded1[BoundedCtors.G1[BoundedCtors.SubMarker]] <==> Data(true)
    }
  }
}
