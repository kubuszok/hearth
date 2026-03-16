package hearth

import hearth.data.Data

/** Macro implementation is in [[MioIntegrationsFixturesImpl]] */
final class MioIntegrationsSpec extends MacroSuite {

  group("trait MacroTypedCommons") {

    group("ValDefsCache should work with MIO extensions") {

      test("should forward declare and build cached value with MIO") {
        import MioIntegrationsFixtures.testValDefBuilderBuildCachedWithMIO

        testValDefBuilderBuildCachedWithMIO <==> Data.map(
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

    group("ValDefsCache merge with parMap2") {

      // Tests shared-parallel ValDefsCache semantics with parMap2: branch A builds, branch B checks
      // cache and reuses the entry. Result is 42 + 42 = 84 because both branches reference the same
      // cached Expr.quote(42).
      test("should merge caches from parMap2 branches that cache the same key") {
        import MioIntegrationsFixtures.testValDefsCacheParMap2MergeBug
        testValDefsCacheParMap2MergeBug <==> Data(84)
      }

      // Tests that calling buildCachedWith for an already-built key+signature produces an error.
      // Users should check the cache (via get* methods) before building.
      test("should fail when both branches blindly buildCachedWith for the same key") {
        import MioIntegrationsFixtures.testValDefsCacheParMap2DuplicateBuildError
        testValDefsCacheParMap2DuplicateBuildError <==> Data("duplicate-build-detected")
      }
    }

    group("Environment.loadMacroExtensions should work with MIO") {
      test("should load macro extensions") {
        import MioIntegrationsFixtures.testExtensionLoadingResultToMIO

        // We're using extensions intended for another test suite, so we expect all of them to fail to load.
        testExtensionLoadingResultToMIO <==> Data.map(
          "data" -> Data.map(
            // These should fail because the extensions are intended for another test suite, but when we allow failures, we should get the empty collection of loaded extensions.
            "successfulDontAllowFailures" -> Data.map(
              "errors" -> Data.list(
                Data("Instance of hearth.Example1MacroExtension cannot be applied to hearth.MioIntegrationsFixtures"),
                Data("Instance of hearth.Example2MacroExtension cannot be applied to hearth.MioIntegrationsFixtures")
              )
            ),
            "successfulAllowFailures" -> Data.map(
              "loaded" -> Data.list()
            ),
            // Same as above.
            "runningFailedDontAllowFailures" -> Data.map(
              "errors" -> Data.list(
                Data("Instance of hearth.Example3MacroExtension cannot be applied to hearth.MioIntegrationsFixtures")
              )
            ),
            "runningFailedAllowFailures" -> Data.map(
              "loaded" -> Data.list(Data())
            ),
            // These should always fail because the extension is not available.
            "loadingFailedDontAllowFailures" -> Data.map(
              "errors" -> Data.list(
                Data(
                  "hearth.TotallyFailedMacroExtension: Provider hearth.Example4MacroExtension could not be instantiated"
                )
              )
            ),
            "loadingFailedAllowFailures" -> Data.map(
              "errors" -> Data.list(
                Data(
                  "hearth.TotallyFailedMacroExtension: Provider hearth.Example4MacroExtension could not be instantiated"
                )
              )
            )
          ),
          "logs" ->
            Data("""Logs:
                   |├ [Error] hearth.SuccessfulMacroExtension - Failed to load 2 extensions:
                   |│           - hearth.Example1MacroExtension: Instance of hearth.Example1MacroExtension cannot be applied to hearth.MioIntegrationsFixtures
                   |│           - hearth.Example2MacroExtension: Instance of hearth.Example2MacroExtension cannot be applied to hearth.MioIntegrationsFixtures
                   |│         Successfully loaded 0 extensions:
                   |│         
                   |│         but failures were not allowed
                   |├ [Info]  hearth.SuccessfulMacroExtension - Successfully loaded 0 extensions:
                   |│         
                   |│         Failed to load 2 extensions:
                   |│           - hearth.Example1MacroExtension: Instance of hearth.Example1MacroExtension cannot be applied to hearth.MioIntegrationsFixtures
                   |│           - hearth.Example2MacroExtension: Instance of hearth.Example2MacroExtension cannot be applied to hearth.MioIntegrationsFixtures
                   |│         but failures were allowed
                   |├ [Error] hearth.PartiallyFailedMacroExtension - Failed to load 1 extensions:
                   |│           - hearth.Example3MacroExtension: Instance of hearth.Example3MacroExtension cannot be applied to hearth.MioIntegrationsFixtures
                   |│         Successfully loaded 0 extensions:
                   |│         
                   |│         but failures were not allowed
                   |├ [Info]  hearth.PartiallyFailedMacroExtension - Successfully loaded 0 extensions:
                   |│         
                   |│         Failed to load 1 extensions:
                   |│           - hearth.Example3MacroExtension: Instance of hearth.Example3MacroExtension cannot be applied to hearth.MioIntegrationsFixtures
                   |│         but failures were allowed
                   |├ [Error] hearth.TotallyFailedMacroExtension - Failed to load extensions:
                   |│         hearth.TotallyFailedMacroExtension: Provider hearth.Example4MacroExtension could not be instantiated
                   |└ [Error] hearth.TotallyFailedMacroExtension - Failed to load extensions:
                   |          hearth.TotallyFailedMacroExtension: Provider hearth.Example4MacroExtension could not be instantiated
                   |""".stripMargin)
        )
      }
    }
  }
}
