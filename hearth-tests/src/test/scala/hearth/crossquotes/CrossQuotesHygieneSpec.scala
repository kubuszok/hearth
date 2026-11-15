package hearth
package crossquotes

import hearth.data.Data

/** Members named like top-level packages (`scala`, `java`, `hearth`, `hygienepkg`) shadow those packages inside this
  * object. Code produced by `Expr.quote` must still refer to the packages (i.e. be `_root_`-anchored).
  */
object CrossQuotesHygieneProbe {
  def scala: String = "shadows the scala package"
  def java: String = "shadows the java package"
  def hearth: String = "shadows the hearth package"
  def hygienepkg: String = "shadows the hygienepkg package"

  def run: Data = CrossQuotesHygieneFixtures.testQuoteHygiene[Int](42)
}

final class CrossQuotesHygieneSpec extends MacroSuite {

  group("Cross-Quotes hygiene") {

    test("Expr.quote should not be captured by members shadowing top-level packages at the expansion site") {
      CrossQuotesHygieneProbe.run <==> Data.map(
        "some" -> Data("Some(42)"),
        "list" -> Data("List(1, 2, 3)"),
        "none" -> Data("None"),
        "vector" -> Data("Vector(4)"),
        "uuid" -> Data("00000000-0000-0000-0000-000000000000"),
        "thing" -> Data("HygieneThing(5)")
      )
    }
  }
}
