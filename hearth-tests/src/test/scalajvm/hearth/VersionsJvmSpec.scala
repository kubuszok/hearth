package hearth

/** JVM-only tests for the reflection / classpath based resolution in `hearth/versions.scala`.
  *
  * These depend on `Class#getProtectionDomain` / runtime APIs that only make sense on the JVM, so they are kept out of
  * the cross-platform [[VersionsSpec]].
  */
final class VersionsJvmSpec extends MacroSuite {

  group("object JDKVersion") {

    test("runtimeJDKVersion should resolve the running JDK with major == 1 and a sane minor") {
      val v = JDKVersion.runtimeJDKVersion
      v.major ==> 1
      // Every JDK Hearth runs on is at least 8.
      assert(v.minor >= 8, s"Expected JDK feature version >= 8, got ${v.minor}")
    }

    test("runtimeJDKVersion should be stable across calls") {
      JDKVersion.runtimeJDKVersion ==> JDKVersion.runtimeJDKVersion
    }
  }

  group("object ScalaVersion") {

    test("byJVMRuntime should resolve a supported Scala version") {
      val v = ScalaVersion.byJVMRuntime
      // The runtime Scala version must be one Hearth supports.
      assert(v.isScala2_13 || v.isScala3, s"Unexpected runtime Scala version: $v")
      // ... and it must match the language version Hearth was compiled against.
      v.toLanguageVersion ==> LanguageVersion.byHearth
    }

    test("byJVMRuntime should be stable across calls") {
      ScalaVersion.byJVMRuntime ==> ScalaVersion.byJVMRuntime
    }

    test("byScalaLibrary should throw when the jar name does not match a Scala library") {
      // A user-defined object lives in a test jar / class directory whose name does not match
      // the scala-library / scala3-library naming patterns, so resolution must fail loudly.
      val ex = intercept[RuntimeException](ScalaVersion.byScalaLibrary(NotAScalaLibraryMarker))
      assert(
        ex.getMessage.contains("Cannot resolve Scala version from library:"),
        s"Unexpected message: ${ex.getMessage}"
      )
    }

    test("resolveByScala2Library should return None for a non-Scala-2-library object") {
      ScalaVersion.resolveByScala2Library(NotAScalaLibraryMarker) ==> None
    }

    test("resolveByScala3Library should return None for a non-Scala-3-library object") {
      ScalaVersion.resolveByScala3Library(NotAScalaLibraryMarker) ==> None
    }

    test("resolveBy*Library should yield a sane version when they do resolve, else None") {
      // Pass stdlib objects; depending on whether they live in scala-library / scala3-library jars
      // (vs. a class directory) resolution may succeed or be None — neither must throw, and any
      // resolved value must be a supported version.
      List(
        ScalaVersion.resolveByScala2Library(Nil),
        ScalaVersion.resolveByScala3Library(Predef)
      ).foreach { resolved =>
        resolved.foreach { v =>
          assert(v.isScala2_13 || v.isScala3, s"Unexpected resolved version: $v")
        }
      }
      // byJVMRuntime is the supported entrypoint and must always succeed.
      assert(ScalaVersion.byJVMRuntime.isScala2_13 || ScalaVersion.byJVMRuntime.isScala3)
    }
  }

  group("object HearthVersion") {

    test("byHearthLibrary should either resolve a Jvm HearthVersion or be None (running from class dir)") {
      // When tests run from a packaged jar this resolves; when run from a class directory it is None.
      // Either way it must never throw and, if present, must report the JVM platform.
      HearthVersion.byHearthLibrary.foreach { v =>
        v.platform ==> Platform.Jvm
        v.languageVersion ==> LanguageVersion.byHearth
        assert(v.version.nonEmpty)
      }
    }
  }
}

/** Marker object used to feed `byScalaLibrary` an object that lives outside any Scala library jar. */
private object NotAScalaLibraryMarker
