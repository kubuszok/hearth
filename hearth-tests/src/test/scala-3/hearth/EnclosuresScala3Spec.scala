package hearth

import hearth.data.Data

final class EnclosuresScala3Spec extends MacroSuite {

  object UsingNest {
    import EnclosuresFixtures.testEnclosingMethodParameters as params

    def withUsing(a: Int)(using b: Long): Data = { val _ = (a, b); params }
    def withAnonymousUsing(a: Int)(using Long): Data = { val _ = (a, summon[Long]); params }
  }

  private def param(name: String, tpe: String, isImplicit: Boolean = false): Data =
    Data.map(
      "name" -> Data(name),
      "type" -> Data(tpe),
      "isImplicit" -> Data(isImplicit),
      "isByName" -> Data(false)
    )

  group("trait hearth.Enclosures (Scala 3)") {

    group("Environment.enclosingMethodParameters, expected behavior") {

      test("lists a using clause as implicit parameters") {
        UsingNest.withUsing(1)(using 2L) <==> Data.list(
          Data.list(param("a", "scala.Int")),
          Data.list(param("b", "scala.Long", isImplicit = true))
        )
      }

      test("lists an anonymous using parameter with its synthetic name") {
        UsingNest.withAnonymousUsing(1)(using 2L) <==> Data.list(
          Data.list(param("a", "scala.Int")),
          Data.list(param("x$2", "scala.Long", isImplicit = true))
        )
      }
    }
  }
}
