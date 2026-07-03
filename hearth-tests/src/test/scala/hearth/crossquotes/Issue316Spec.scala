package hearth
package crossquotes

import hearth.data.Data

/** Regression for hearth#316: sibling implicit `Type[_]` definitions in one template must not deadlock forcing each
  * other during initialization.
  *
  * Macro implementation is in [[hearth.cq.CrossQuotesPlugin]] (Scala 3, where the deadlock occurred) and
  * [[hearth.cq.CrossQuotesMacros]] (Scala 2). Fixtures are in [[Issue316ReproFixturesImpl]].
  */
final class Issue316Spec extends MacroSuite {

  group("Cross-Quotes plugin: sibling implicit Type vals (hearth#316)") {

    test("sibling implicit lazy val Type definitions do not force each other into a deadlock") {
      Issue316ReproFixtures.testSiblingImplicitTypes <==> Data.map(
        "unit" -> Data("scala.Unit"),
        "null" -> Data("scala.Null"),
        "any" -> Data("scala.Any"),
        "usable" -> Data("scala.Option[scala.Unit]")
      )
    }
  }
}
