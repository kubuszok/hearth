package hearth
package crossquotes

import hearth.data.Data

/** Regression for https://github.com/kubuszok/hearth/issues/307 (Scala 2 only), round 2: the generated
  * `Type.CtorN.UpperBounded/Bounded.unapply` must MATCH a WILDCARD type argument (`F1[_ <: Marker]`), which reaches
  * `baseType` as a packed `ExistentialType` rather than a bare `TypeRef`. See [[Issue307ReproFixturesImpl]] — the full
  * cross-compilation failure is validated by the standalone repro linked from the issue.
  */
final class Issue307MatchSpec extends MacroSuite {

  group("Cross-Quotes (Scala 2): CtorN.UpperBounded/Bounded.unapply matches wildcard existential args (#307)") {

    test("Ctor1.UpperBounded matches a concrete application") {
      Issue307ReproFixtures.testCtor1UpperBoundedMatch[Issue307.F1[Issue307.SubMarker]] <==> Data(true)
    }

    test("Ctor1.UpperBounded matches a wildcard application (F1[_ <: Marker])") {
      Issue307ReproFixtures.testCtor1UpperBoundedMatch[Issue307.F1[? <: Issue307.Marker]] <==> Data(true)
    }

    test("Ctor2.UpperBounded matches a wildcard application (F2[_ <: Marker, _ <: Marker])") {
      Issue307ReproFixtures
        .testCtor2UpperBoundedMatch[Issue307.F2[? <: Issue307.Marker, ? <: Issue307.Marker]] <==> Data(true)
    }

    test("Ctor1.Bounded matches a wildcard application (G1[_ >: SubMarker <: Marker])") {
      Issue307ReproFixtures
        .testCtor1BoundedMatch[Issue307.G1[? >: Issue307.SubMarker <: Issue307.Marker]] <==> Data(true)
    }
  }
}
