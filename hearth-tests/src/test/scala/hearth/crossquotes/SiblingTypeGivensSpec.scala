package hearth
package crossquotes

import hearth.data.Data

/** Sibling implicit `Type[_]` definitions in one template must not deadlock forcing each other during initialization.
  *
  * Macro implementation is in [[hearth.cq.CrossQuotesPlugin]] (Scala 3, where the deadlock occurred) and
  * [[hearth.cq.CrossQuotesMacros]] (Scala 2). Fixtures are in [[SiblingTypeGivensFixturesImpl]].
  */
final class SiblingTypeGivensSpec extends MacroSuite {

  group("Cross-Quotes plugin: sibling implicit Type vals") {

    test("sibling implicit lazy val Type definitions do not force each other into a deadlock") {
      SiblingTypeGivensFixtures.testSiblingImplicitTypes <==> Data.map(
        "unit" -> Data("scala.Unit"),
        "null" -> Data("scala.Null"),
        "any" -> Data("scala.Any"),
        "usable" -> Data("scala.Option[scala.Unit]")
      )
    }
  }
}
