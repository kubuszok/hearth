package hearth
package data

/** Macro implementation is in [[DataFixturesImpl]] */
final class DataSpec extends MacroSuite {

  group("Data") {

    test("should be convertible to Expr[Data], which after unquoting is the same value as the original") {

      DataFixtures.example <==> DataFixturesImpl.example
    }

    test("should be convertible to Expr[Data] also for empty, nested, null and escaped values") {

      val actual = DataFixtures.exampleEdgeCases
      actual <==> DataFixturesImpl.exampleEdgeCases
      // `<==>` does not see a difference between a missing and a null list element, so compare the values too
      assert(actual == DataFixturesImpl.exampleEdgeCases)
    }

    // Checked with compileErrors, so that the whole spec still compiles when the generated code does not.
    test("should be convertible to Expr[Data] for an empty map, also nested in another map") {
      compileErrors("hearth.data.DataFixtures.exampleEmptyMap") ==> ""
      compileErrors("hearth.data.DataFixtures.exampleNestedEmptyMap") ==> ""
    }

    // test("should be able to diff with another Data") {

    //   DataFixtures.example <==> Data(
    //     Map(
    //       "null" -> Data(),
    //       "int" -> Data(1),
    //       "long" -> Data(1L),
    //       "float" -> Data(1.0f),
    //       "double" -> Data(1.0),
    //       "boolean" -> Data(true),
    //       "string" -> Data("string"),
    //       "list" -> Data(List(Data(1), Data(2), Data(3), Data(4))),
    //       "map" -> Data(Map("a" -> Data(1), "b" -> Data(2), "c" -> Data(3)))
    //     )
    //   )
    // }
  }
}
