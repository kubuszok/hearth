package hearth
package crossquotes

import hearth.data.Data

/** Fixtures are in [[CrossQuotesEdgeCasesScala3FixturesImpl]]. */
final class CrossQuotesEdgeCasesScala3Spec extends MacroSuite {

  group("Cross-Quotes edge cases (Scala 3)") {

    test("Type.of and Type.Ctor1.of should keep opaque types distinct from their representation") {
      CrossQuotesEdgeCasesScala3Fixtures.testOpaqueTypes <==> Data.map(
        "Type.of" -> Data.map(
          "plainPrint" -> Data("hearth.examples.crossquotesedge.opaques.Plain"),
          "is not the same as its representation" -> Data(false),
          "is not a subtype of its representation" -> Data(false)
        ),
        "Ctor1 of the representation" -> Data.map(
          "unapply on the opaque type" -> Data("<no match>")
        ),
        "Ctor1 of the opaque type" -> Data.map(
          "apply" -> Data("hearth.examples.crossquotesedge.opaques.Wrapped[java.lang.String]"),
          "unapply on the opaque type" -> Data("scala.Int"),
          "unapply on the representation" -> Data("<no match>")
        )
      )
    }

    test("quotes keep static types of spliced Exprs, typed vals, tuple patterns and their own type parameters") {
      CrossQuotesEdgeCasesScala3Fixtures.testQuoteSpliceTypes(4) <==> Data.map(
        "partial function literal" -> Data("4,6"),
        "named arguments out of declaration order" -> Data(99),
        "block ending with a reference to its typed val" -> Data("right: 5"),
        "tuple pattern in a val definition" -> Data("2/4"),
        "spliced literal keeps the static type of its Expr" -> Data("right: 5"),
        "type parameter shadowed by a method inside the quote" -> Data("described 4")
      )
    }
  }
}
