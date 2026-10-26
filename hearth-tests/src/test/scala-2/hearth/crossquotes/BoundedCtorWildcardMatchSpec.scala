package hearth
package crossquotes

import hearth.data.Data

/** Scala 2 only: a WILDCARD type argument (`F1[_ <: Marker]`) reaches `baseType` as a packed `ExistentialType` rather
  * than a bare `TypeRef`, and the generated `Type.CtorN.UpperBounded/Bounded.unapply` must still MATCH it.
  */
final class BoundedCtorWildcardMatchSpec extends MacroSuite {

  group("Cross-Quotes (Scala 2): CtorN.UpperBounded/Bounded.unapply matches wildcard existential args") {

    test("Ctor1.UpperBounded matches a concrete application") {
      BoundedCtorWildcardMatchFixtures
        .testCtor1UpperBoundedMatch[BoundedCtorWildcardExamples.F1[BoundedCtorWildcardExamples.SubMarker]] <==> Data(
        true
      )
    }

    test("Ctor1.UpperBounded matches a wildcard application (F1[_ <: Marker])") {
      BoundedCtorWildcardMatchFixtures
        .testCtor1UpperBoundedMatch[BoundedCtorWildcardExamples.F1[? <: BoundedCtorWildcardExamples.Marker]] <==> Data(
        true
      )
    }

    test("Ctor2.UpperBounded matches a wildcard application (F2[_ <: Marker, _ <: Marker])") {
      BoundedCtorWildcardMatchFixtures
        .testCtor2UpperBoundedMatch[BoundedCtorWildcardExamples.F2[
          ? <: BoundedCtorWildcardExamples.Marker,
          ? <: BoundedCtorWildcardExamples.Marker
        ]] <==> Data(true)
    }

    test("Ctor1.Bounded matches a wildcard application (G1[_ >: SubMarker <: Marker])") {
      BoundedCtorWildcardMatchFixtures
        .testCtor1BoundedMatch[BoundedCtorWildcardExamples.G1[
          ? >: BoundedCtorWildcardExamples.SubMarker <: BoundedCtorWildcardExamples.Marker
        ]] <==> Data(true)
    }
  }
}
