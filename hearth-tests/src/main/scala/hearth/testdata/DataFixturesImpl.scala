package hearth
package testdata

/** Fixtured for testing [[DataSpec]]. */
trait DataFixturesImpl { this: MacroCommons & DataSupports =>

  def example: Expr[Data] = Expr(DataFixturesImpl.example)
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
      "list" -> Data.list(Data(1), Data(2), Data(3)),
      "map" -> Data.map("a" -> Data(1), "b" -> Data(2), "c" -> Data(3))
    )
  )
}
