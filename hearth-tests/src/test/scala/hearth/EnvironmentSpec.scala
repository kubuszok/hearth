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

    group(
      "methods: Environment.{currentPosition, currentLanguageVersion, currentPlatform, isJvm, isJs, isNative, XMacroSettings, typedSettings}, expected behavior"
    ) {
      import EnvironmentFixtures.testEnvironment

      test("should return the current Scala version") {
        testEnvironment <==> Data.map(
          "currentPosition" -> Data("EnvironmentSpec.scala:30:9"),
          "currentLanguageVersion" -> Data(LanguageVersion.byHearth.toString),
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
  }
}
