package hearth
package crossquotes

import hearth.data.Data

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossTypeSelfRefFixturesImpl]].
  *
  * Regression tests for hearth#285 — `implicit val ConfigT: Type[Configuration] = Type.of[Configuration]` must not
  * resolve the generated implicit search to the very value being defined.
  */
final class CrossTypeSelfRefSpec extends MacroSuite {

  group("Cross-Quotes macro/plugin: self-referential implicit Type vals (hearth#285)") {

    test("implicit val A: Type[A] = Type.of[A] should materialize directly, not resolve to itself") {
      CrossTypeSelfRefFixtures.testSelfReferentialImplicitType <==> Data.map(
        "classLevelVal" -> Data("hearth.examples.classes.ExampleClass"),
        "methodLocalVal" -> Data("hearth.examples.classes.ExampleTrait"),
        "secondLocalVal" -> Data("hearth.examples.classes.ExampleClassWithTypeParam[scala.Int]"),
        "implicitStillUsable" -> Data("scala.Option[hearth.examples.classes.ExampleTrait]")
      )
    }

    test("implicit val F: Type.Ctor1[F] = Type.Ctor1.of[F] should materialize directly, not resolve to itself") {
      CrossTypeSelfRefFixtures.testSelfReferentialImplicitCtor <==> Data.map(
        "appliedCtor" -> Data("scala.Option[scala.Int]")
      )
    }
  }
}
