package hearth

/** Pure-logic tests for the runtime version models in `hearth/versions.scala`.
  *
  * These cover only platform-independent value behavior (case-class fields, predicates, `toString`, `Ordering`) so they
  * run on JVM, JS and Native alike. Reflection-based resolution (`byJVMRuntime`, `byScalaLibrary`, `byHearthLibrary`,
  * `runtimeJDKVersion`, ...) is exercised separately in the JVM-only spec.
  */
final class VersionsSpec extends MacroSuite {

  group("final case class HearthVersion") {

    test("toString should render version, language version and platform") {
      HearthVersion("0.1.0", LanguageVersion.Scala2_13, Platform.Jvm).toString ==>
        "Hearth 0.1.0 (Scala 2.13, Jvm)"
      HearthVersion("1.2.3", LanguageVersion.Scala3, Platform.Js).toString ==>
        "Hearth 1.2.3 (Scala 3, Js)"
    }

    test("should preserve all of its fields") {
      val v = HearthVersion("0.4.0", LanguageVersion.Scala3, Platform.Native)
      v.version ==> "0.4.0"
      v.languageVersion ==> LanguageVersion.Scala3
      v.platform ==> Platform.Native
    }
  }

  group("final case class JDKVersion") {

    test("toString should render the major.minor version") {
      JDKVersion(1, 8).toString ==> "JDK 1.8"
      JDKVersion(1, 17).toString ==> "JDK 1.17"
    }

    test("ordering should order by (major, minor)") {
      val sorted = List(
        JDKVersion(1, 21),
        JDKVersion(1, 8),
        JDKVersion(2, 0),
        JDKVersion(1, 17)
      ).sorted
      sorted ==> List(
        JDKVersion(1, 8),
        JDKVersion(1, 17),
        JDKVersion(1, 21),
        JDKVersion(2, 0)
      )
    }

    test("ordering should compare adjacent versions") {
      val ord = implicitly[Ordering[JDKVersion]]
      ord.lt(JDKVersion(1, 8), JDKVersion(1, 17)) ==> true
      ord.lt(JDKVersion(1, 17), JDKVersion(1, 8)) ==> false
      ord.equiv(JDKVersion(1, 17), JDKVersion(1, 17)) ==> true
      ord.compare(JDKVersion(1, 8), JDKVersion(2, 0)) < 0 ==> true
    }
  }

  group("final case class ScalaVersion") {

    test("isScala2_13 / isScala3 should classify by major.minor") {
      val s213 = ScalaVersion(2, 13, 16)
      s213.isScala2_13 ==> true
      s213.isScala3 ==> false

      val s3 = ScalaVersion(3, 3, 8)
      s3.isScala2_13 ==> false
      s3.isScala3 ==> true
    }

    test("toLanguageVersion should map supported versions") {
      ScalaVersion(2, 13, 16).toLanguageVersion ==> LanguageVersion.Scala2_13
      ScalaVersion(2, 13, 0).toLanguageVersion ==> LanguageVersion.Scala2_13
      ScalaVersion(3, 3, 8).toLanguageVersion ==> LanguageVersion.Scala3
      ScalaVersion(3, 0, 0).toLanguageVersion ==> LanguageVersion.Scala3
    }

    test("toLanguageVersion should throw for unsupported versions") {
      val _ = intercept[RuntimeException](ScalaVersion(2, 12, 18).toLanguageVersion)
      val _ = intercept[RuntimeException](ScalaVersion(1, 0, 0).toLanguageVersion)
      val ex = intercept[RuntimeException](ScalaVersion(2, 11, 0).toLanguageVersion)
      assert(ex.getMessage.contains("Unsupported Scala version: 2.11.0"))
    }

    test("isScala2_13 / isScala3 should throw for unsupported versions (via toLanguageVersion)") {
      val _ = intercept[RuntimeException](ScalaVersion(2, 12, 0).isScala2_13)
      val _ = intercept[RuntimeException](ScalaVersion(2, 12, 0).isScala3)
    }

    test("toString should render Scala major.minor.patch") {
      ScalaVersion(2, 13, 16).toString ==> "Scala 2.13.16"
      ScalaVersion(3, 3, 8).toString ==> "Scala 3.3.8"
    }

    test("ordering should order by (major, minor, patch)") {
      val sorted = List(
        ScalaVersion(3, 3, 8),
        ScalaVersion(2, 13, 16),
        ScalaVersion(3, 3, 1),
        ScalaVersion(2, 13, 18),
        ScalaVersion(3, 8, 4)
      ).sorted
      sorted ==> List(
        ScalaVersion(2, 13, 16),
        ScalaVersion(2, 13, 18),
        ScalaVersion(3, 3, 1),
        ScalaVersion(3, 3, 8),
        ScalaVersion(3, 8, 4)
      )
    }

    test("ordering should compare adjacent versions") {
      val ord = implicitly[Ordering[ScalaVersion]]
      ord.lt(ScalaVersion(2, 13, 16), ScalaVersion(2, 13, 18)) ==> true
      ord.lt(ScalaVersion(2, 13, 18), ScalaVersion(3, 3, 8)) ==> true
      ord.equiv(ScalaVersion(3, 3, 8), ScalaVersion(3, 3, 8)) ==> true
    }
  }

  group("sealed trait LanguageVersion") {

    test("isScala2_13 / isScala3 predicates") {
      LanguageVersion.Scala2_13.isScala2_13 ==> true
      LanguageVersion.Scala2_13.isScala3 ==> false
      LanguageVersion.Scala3.isScala2_13 ==> false
      LanguageVersion.Scala3.isScala3 ==> true
    }

    test("toString should render the human-readable name") {
      LanguageVersion.Scala2_13.toString ==> "Scala 2.13"
      LanguageVersion.Scala3.toString ==> "Scala 3"
    }

    test("ordering should place Scala 2.13 before Scala 3") {
      List[LanguageVersion](LanguageVersion.Scala3, LanguageVersion.Scala2_13).sorted ==>
        List(LanguageVersion.Scala2_13, LanguageVersion.Scala3)
      val ord = implicitly[Ordering[LanguageVersion]]
      ord.lt(LanguageVersion.Scala2_13, LanguageVersion.Scala3) ==> true
      ord.equiv(LanguageVersion.Scala3, LanguageVersion.Scala3) ==> true
    }
  }

  group("sealed trait Platform") {

    test("isJvm / isJs / isNative predicates are mutually exclusive") {
      Platform.Jvm.isJvm ==> true
      Platform.Jvm.isJs ==> false
      Platform.Jvm.isNative ==> false

      Platform.Js.isJvm ==> false
      Platform.Js.isJs ==> true
      Platform.Js.isNative ==> false

      Platform.Native.isJvm ==> false
      Platform.Native.isJs ==> false
      Platform.Native.isNative ==> true
    }
  }
}
