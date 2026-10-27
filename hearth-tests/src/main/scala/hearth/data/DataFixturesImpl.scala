package hearth
package data

trait DataFixturesImpl { this: MacroCommons =>

  def example: Expr[Data] = Expr(DataFixturesImpl.example)

  def exampleEdgeCases: Expr[Data] = Expr(DataFixturesImpl.exampleEdgeCases)

  def exampleEmptyMap: Expr[Data] = Expr(Data.map())

  def exampleNestedEmptyMap: Expr[Data] = Expr(Data.map("nested" -> Data.map()))
}
object DataFixturesImpl {

  def example: Data = Data(
    Map(
      "null" -> Data(),
      "int" -> Data(1),
      "long" -> Data(1L),
      "float" -> Data(1.0f),
      "double" -> Data(1.0),
      "boolean" -> Data(true),
      "string" -> Data("string"),
      "list" -> data.Data.list(Data(1), Data(2), Data(3)),
      "map" -> data.Data.map("a" -> Data(1), "b" -> Data(2), "c" -> Data(3))
    )
  )

  def exampleEdgeCases: Data = Data.map(
    "emptyList" -> Data.list(),
    "listOfMaps" -> Data.list(Data.map("a" -> Data.list(Data(1))), Data.map("b" -> Data.map("c" -> Data(2)))),
    "nullInList" -> Data.list(Data(), Data(false)),
    "nullInMap" -> Data.map("missing" -> Data()),
    "specialString" -> Data("quote \" backslash \\ newline \n unicode \u0105 $ sign"),
    "emptyString" -> Data(""),
    "minInt" -> Data(Int.MinValue),
    "maxLong" -> Data(Long.MaxValue),
    "negativeDouble" -> Data(-0.5)
  )
}
