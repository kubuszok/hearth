package hearth

import hearth.data.Data

/** Macro implementation is in [[MioIntegrationsFixturesImpl]] */
final class MioIntegrationsSpec extends MacroSuite {

  group("trait MacroCommons") {

    group("ValDefsCache should work with MIO extensions") {

      test("should forward declare and build cached value with MIO") {
        import MioIntegrationsFixtures.testValDefBuilderBuildCachedWithMIO

        testValDefBuilderBuildCachedWithMIO ==> Data.map(
          "def0" -> Data(0),
          "def1" -> Data(1 * 10),
          "def2" -> Data((1 + 2) * 10),
          "def3" -> Data((1 + 2 + 3) * 10),
          "def4" -> Data((1 + 2 + 3 + 4) * 10),
          "def5" -> Data((1 + 2 + 3 + 4 + 5) * 10),
          "def6" -> Data((1 + 2 + 3 + 4 + 5 + 6) * 10),
          "def7" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7) * 10),
          "def8" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8) * 10),
          "def9" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) * 10),
          "def10" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10) * 10),
          "def11" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11) * 10),
          "def12" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12) * 10),
          "def13" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13) * 10),
          "def14" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14) * 10),
          "def15" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15) * 10),
          "def16" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16) * 10),
          "def17" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17) * 10),
          "def18" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18) * 10),
          "def19" -> Data((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19) * 10),
          "def20" -> Data(
            (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20) * 10
          ),
          "def21" -> Data(
            (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20 + 21) * 10
          ),
          "def22" -> Data(
            (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20 + 21 + 22) * 10
          )
        )
      }
    }
  }
}
