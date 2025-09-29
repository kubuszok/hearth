package hearth

import hearth.data.Data

final class EnvironmentSpec extends MacroSuite {

  group("trait hearth.Environment") {

    group("methods: Environment.currentPosition, expected behavior") {
      import EnvironmentFixtures.testPosition

      test("should return the current position") {
        testPosition <==> Data.map(
          "file" -> Data("hearth-tests/src/test/scala/hearth/EnvironmentSpec.scala"),
          "offset" -> Data(307),
          "line" -> Data(13),
          "column" -> Data(9),
          "fileName" -> Data("EnvironmentSpec.scala"),
          "prettyPrint" -> Data("EnvironmentSpec.scala:13:9")
        )
      }
    }

    group("methods: Environment.isExpandedAt, expected behavior") {
      import EnvironmentFixtures.testIsExpandedAt

      test("should return true if the current position matches the given position") {
        testIsExpandedAt("EnvironmentSpec.scala:28") ==> true
        testIsExpandedAt(
          "EnvironmentSpec.scala:29"
        ) ==> true
        testIsExpandedAt("EnvironmentSpec.scala:32:9") ==> true
      }

      test("should return false if the current position does not match the given position") {
        testIsExpandedAt("EnvironmentSpec.scala:35") ==> false
        testIsExpandedAt("EnvironmentSpec.scala:37:10") ==> false
      }

      test("should report error and abort if the position has invalid format") {
        compileErrors("EnvironmentFixtures.testIsExpandedAt(\"123\")").check("Invalid position: 123")
      }
    }

    group(
      "methods: Environment.{currentPosition, currentLanguageVersion, currentPlatform, isJvm, isJs, isNative, XMacroSettings, typedSettings}, expected behavior"
    ) {
      import EnvironmentFixtures.testEnvironment

      test("should return the current Scala version") {
        testEnvironment <==> Data.map(
          "currentPosition" -> Data("EnvironmentSpec.scala:51:9"),
          "currentLanguageVersion" -> Data(LanguageVersion.byHearth.toString),
          "isScala2_13" -> Data(LanguageVersion.byHearth.isScala2_13),
          "isScala3" -> Data(LanguageVersion.byHearth.isScala3),
          "currentPlatform" -> Data(Platform.byHearth.toString),
          "isJvm" -> Data(Platform.byHearth.isJvm),
          "isJs" -> Data(Platform.byHearth.isJs),
          "isNative" -> Data(Platform.byHearth.isNative),
          "XMacroSettings" -> Data.list(
            Data("hearth-tests.primitives.int=1024"),
            Data("hearth-tests.primitives.long=65536L"),
            Data("hearth-tests.primitives.float=3.14f"),
            Data("hearth-tests.primitives.double=2.71828"),
            Data("hearth-tests.primitives.boolean=true"),
            Data("hearth-tests.primitives.explicit-string=\"hello\""),
            Data("hearth-tests.primitives.implicit-string=hello")
          ),
          "typedSettings" -> Data.map(
            "hearth-tests" -> Data.map(
              "primitives" -> Data.map(
                "int" -> Data(1024),
                "long" -> Data(65536L),
                "float" -> Data(3.14f),
                "double" -> Data(2.71828),
                "boolean" -> Data(true),
                "explicit-string" -> Data("hello"),
                "implicit-string" -> Data("hello")
              )
            )
          )
        )
      }
    }

    group(
      "methods: Environment.reportErrorAndAbort, expected behavior"
    ) {

      test("should report error and abort") {
        // Apparently, there is no way to check if there were some info/warn messages.
        compileErrors("EnvironmentFixtures.testErrorAndAbort").check("Error and abort message")
      }
    }

    group("methods: Environment.loadMacroExtensions, expected behavior") {
      import EnvironmentFixtures.testLoadingExtensions

      test("should load macro extensions") {
        testLoadingExtensions <==> Data.list(Data("Example 1"), Data("Example 2"))
      }
    }
  }
}
