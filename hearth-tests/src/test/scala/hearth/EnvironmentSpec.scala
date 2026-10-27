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

    group("methods: Environment.reportInfo(msg, position), expected behavior") {
      import EnvironmentFixtures.testReportInfoAtPosition

      test("should report info at the given position and continue the expansion") {
        // Apparently, there is no way to check if there were some info/warn messages,
        // so we only verify that reporting at a Method's position succeeds and the expansion continues.
        testReportInfoAtPosition[examples.methods.NoCompanionClass] <==> Data.map(
          "infoReported" -> Data(true)
        )
      }
    }

    group("methods: Environment.reportError(msg, position), expected behavior") {

      test("should report error at the given position") {
        compileErrors("EnvironmentFixtures.testReportErrorAtPosition").check("Error at position message")
      }
    }

    group("methods: Environment.reportErrorAndAbort(msg, position), expected behavior") {

      test("should report error at the given position and abort") {
        compileErrors("EnvironmentFixtures.testReportErrorAndAbortAtPosition").check(
          "Error and abort at position message"
        )
      }
    }

    group("methods: Environment.loadMacroExtensions, expected behavior") {
      import EnvironmentFixtures.testLoadingExtensions

      test("should load macro extensions") {
        testLoadingExtensions <==> Data.map(
          "successful" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            )
          ),
          "runningFailed" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list(),
              "errors" -> Data.list(Data("Failed to load Example 3"))
            ),
            "asOption" -> Data.map(
              "error" -> Data("Some extensions failed to load")
            ),
            "asEither" -> Data.map(
              "errors" -> Data.list(Data("Failed to load Example 3"))
            )
          ),
          "loadingFailed" -> Data.map(
            "detailed" -> Data.map(
              "error" -> Data(
                "hearth.TotallyFailedMacroExtension: Provider hearth.Example4MacroExtension could not be instantiated"
              )
            ),
            "asOption" -> Data.map(
              "error" -> Data("Some extensions failed to load")
            ),
            "asEither" -> Data.map(
              "errors" -> Data.list(
                Data(
                  "hearth.TotallyFailedMacroExtension: Provider hearth.Example4MacroExtension could not be instantiated"
                )
              )
            )
          ),
          "loadedExtensions" -> Data.list(
            Data("Example 1"),
            Data("Example 2")
          )
        )
      }
    }

    group("methods: Environment.loadMacroExtensionsExcluding, expected behavior") {
      import EnvironmentFixtures.testLoadingExtensionsExcluding

      test("should load macro extensions excluding the ones with the given names") {
        testLoadingExtensionsExcluding <==> Data.map(
          "successful" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"))
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"))
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"))
            )
          ),
          "runningFailed" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list()
            )
          ),
          "loadingFailed" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list()
            )
          ),
          "loadedExtensions" -> Data.list(
            Data("Example 1")
          )
        )
      }
    }

    group("methods: Environment.loadMacroExtensionsWhen, expected behavior") {
      import EnvironmentFixtures.testLoadingExtensionsWhen

      test("should load macro extensions filtering by the given condition") {
        testLoadingExtensionsWhen <==> Data.map(
          "successful" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example2MacroExtension"))
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example2MacroExtension"))
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example2MacroExtension"))
            )
          ),
          "runningFailed" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list()
            )
          ),
          "loadingFailed" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list()
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list()
            )
          ),
          "loadedExtensions" -> Data.list(
            Data("Example 2")
          )
        )
      }
    }

    group("methods: Environment.loadMacroExtensions called twice, expected behavior") {
      import EnvironmentFixtures.testLoadingExtensionsTwice

      test("should load each extension only once even when called twice") {
        testLoadingExtensionsTwice <==> Data.map(
          "firstLoad" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            )
          ),
          "secondLoad" -> Data.map(
            "detailed" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            ),
            "asOption" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            ),
            "asEither" -> Data.map(
              "loaded" -> Data.list(Data("hearth.Example1MacroExtension"), Data("hearth.Example2MacroExtension"))
            )
          ),
          // KEY: Each extension name appears only once — extend() was NOT called twice
          "loadedExtensions" -> Data.list(
            Data("Example 1"),
            Data("Example 2")
          )
        )
      }
    }

    group("methods: Environment.currentPosition.prettyPrintLong, expected behavior") {
      import EnvironmentFixtures.testPositionPrettyPrintLong

      test("should render the full file path followed by the line and column of the expansion") {
        testPositionPrettyPrintLong <==> Data.map(
          "path" -> Data("hearth-tests/src/test/scala/hearth/EnvironmentSpec.scala"),
          "endsWithLineAndColumn" -> Data(true),
          "endsWithPrettyPrint" -> Data(true)
        )
      }
    }

    group("methods: Environment.{currentScalaVersion, currentJDKVersion}, expected behavior") {
      import EnvironmentFixtures.testCompileTimeVersions

      test("should return the Scala and JDK versions used by the compiler expanding the macro") {
        testCompileTimeVersions <==> Data.map(
          "scalaLanguageVersion" -> Data(LanguageVersion.byHearth.toString),
          "scalaMajor" -> Data(if (LanguageVersion.byHearth.isScala3) 3 else 2),
          "scalaMinorIs13WhenScala2" -> Data(true),
          "jdkMajor" -> Data(1),
          "jdkMinorAtLeast8" -> Data(true),
          "jdkSameAsRuntimeJDKVersion" -> Data(true)
        )
      }
    }

    group("methods: Environment.typedSettings accessors, expected behavior") {
      import EnvironmentFixtures.testTypedSettingsAccessors

      test("should expose each parsed -Xmacro-settings value through the accessor of its own type") {
        testTypedSettingsAccessors <==> Data.map(
          "intAsInt" -> Data(true),
          "longAsLong" -> Data(true),
          "floatAsFloat" -> Data(true),
          "doubleAsDouble" -> Data(true),
          "booleanAsBoolean" -> Data(true),
          "explicitStringAsString" -> Data(true),
          "implicitStringAsString" -> Data(true),
          "intRendered" -> Data("1024")
        )
      }
    }

    group("methods: Environment.mioBenchmarkFlameGraphDir, expected behavior") {
      import EnvironmentFixtures.testMioBenchmarkFlameGraphDir

      test("should return the directory passed via -Xmacro-settings:hearth.mioBenchmarkFlameGraphDir") {
        testMioBenchmarkFlameGraphDir <==> Data(true)
      }
    }

    group("methods: Environment.reportError called more than once, expected behavior") {

      test("should only report the first error message") {
        compileErrors("EnvironmentFixtures.testReportErrorTwice")
          .check("First error message")
        compileErrors("EnvironmentFixtures.testReportErrorTwice")
          .checkNot("Second error message")
      }

      test("should only report the first error message even if the next one aborts the expansion") {
        compileErrors("EnvironmentFixtures.testReportErrorThenAbort")
          .check("Error reported before abort")
        compileErrors("EnvironmentFixtures.testReportErrorThenAbort")
          .checkNot("Abort reported after error")
      }
    }

    group("methods: Environment.handleMioTerminationException, expected behavior") {

      test("should report the termination as a compilation error by default") {
        compileErrors("EnvironmentFixtures.testHandleMioTerminationReportsError")
          .check("Simulated MIO termination")
      }

      test("should return the value of the thunk when nothing was terminated") {
        EnvironmentFixtures.testHandleMioTerminationPassesValue ==> 42
      }
    }

    group("methods: Environment.withMioTimeout, expected behavior") {
      import EnvironmentFixtures.testWithMioTimeout

      test("should return Right on success, Left on timeout, reject re-entering and always reset the deadline") {
        testWithMioTimeout <==> Data.map(
          "success" -> Data(42),
          "timedOut" -> Data(true),
          "deadlineResetAfterTimeout" -> Data(true),
          "reentry" -> Data("nested call rejected"),
          "deadlineResetAfterReentry" -> Data(true)
        )
      }
    }

    group("methods: Environment.withMioLoggingDisabled, expected behavior") {
      import EnvironmentFixtures.testWithMioLoggingDisabled

      test("should record no logs inside the block, also when nested, and restore logging afterwards") {
        testWithMioLoggingDisabled <==> Data.map(
          "entriesInNestedDisabledBlock" -> Data(0),
          "entriesAfterNestedBlockEnded" -> Data(0),
          "entriesAfterDisabledBlockEnded" -> Data(1)
        )
      }
    }
  }
}
