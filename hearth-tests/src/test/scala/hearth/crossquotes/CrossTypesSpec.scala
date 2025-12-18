package hearth
package crossquotes

import hearth.data.Data
import hearth.crossquotes.CrossTypesFixtures

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossTypesFixturesImpl]].
  */
final class CrossTypesSpec extends MacroSuite {

  group("Cross-Quotes macro/plugin") {

    group("for Type.of") {

      test("should resolve types in the context of the macro compilation, and consider implicit Types") {

        // These should be ignored, Type.of in macros should be sanitized without our additional effort.
        @scala.annotation.nowarn
        @scala.annotation.unused
        trait ExampleTrait
        @scala.annotation.nowarn
        @scala.annotation.unused
        trait ExampleTraitWithTypeParam[A]

        CrossTypesFixtures.testTypeOf[Int] <==> Data.map(
          "resolvingType" -> Data.map(
            "ofImportedType" -> Data("hearth.examples.classes.ExampleTrait"),
            "ofImportedTypeWithTypeParam" -> Data("hearth.examples.classes.ExampleTraitWithTypeParam[scala.Int]"),
            "ofRelativePathType" -> Data("hearth.examples.classes.ExampleClass"),
            "ofRelativePathWithTypeParamType" -> Data("hearth.examples.classes.ExampleClassWithTypeParam[scala.Int]"),
            "ofFqcnType" -> Data("hearth.examples.classes.ExampleClass"),
            "ofFqcnWithTypeParamType" -> Data("hearth.examples.classes.ExampleClassWithTypeParam[scala.Int]")
          ),
          "resolvingImplicitType" -> Data.map(
            "viaTypeBound" -> Data("scala.Option[scala.Int]"),
            "viaImplicitParam" -> Data("scala.Option[scala.Int]"),
            "viaImport" -> Data("scala.Option[scala.Int]"),
            "viaDeclaredVal" -> Data("scala.Option[scala.Int]"),
            "viaDeclaredDef" -> Data("scala.Option[scala.Int]") // ,
            // "viaInheritance" -> Data("scala.Option[scala.Int]") Probably impossible to implement on Scala 3, since we cannot use Types to find which implicits would be needed.
          )
        )
      }
    }

    group("for Type.Ctor[n].of") {
      import CrossTypesFixtures.*

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor1"
      ) {
        // format: off
        testTypeCtor1[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor1[examples.kinds.Arity1[Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity1[java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity1[java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor1[examples.kinds.Alias.Renamed1[Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity1[java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity1[java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor1[examples.kinds.Alias.FixedFront1[Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[scala.Unit, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor1[examples.kinds.Alias.FixedBack1[Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor2"
      ) {
        // format: off
        testTypeCtor2[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor2[examples.kinds.Arity2[Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[scala.Int, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor2[examples.kinds.Alias.Renamed2[Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[scala.Int, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor2[examples.kinds.Alias.FixedFront2[Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[scala.Unit, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor2[examples.kinds.Alias.FixedBack2[Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor3"
      ) {
        // format: off
        testTypeCtor3[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor3[examples.kinds.Arity3[Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor3[examples.kinds.Alias.Renamed3[Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor3[examples.kinds.Alias.FixedFront3[Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[scala.Unit, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor3[examples.kinds.Alias.FixedBack3[Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor4"
      ) {
        // format: off
        testTypeCtor4[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor4[examples.kinds.Arity4[Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor4[examples.kinds.Alias.Renamed4[Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor4[examples.kinds.Alias.FixedFront4[Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor4[examples.kinds.Alias.FixedBack4[Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor5"
      ) {
        // format: off
        testTypeCtor5[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor5[examples.kinds.Arity5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor5[examples.kinds.Alias.Renamed5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor5[examples.kinds.Alias.FixedFront5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor5[examples.kinds.Alias.FixedBack5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor6"
      ) {
        // format: off
        testTypeCtor6[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor6[examples.kinds.Arity6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor6[examples.kinds.Alias.Renamed6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor6[examples.kinds.Alias.FixedFront6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor6[examples.kinds.Alias.FixedBack6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor7"
      ) {
        // format: off
        testTypeCtor7[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor7[examples.kinds.Arity7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor7[examples.kinds.Alias.Renamed7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor7[examples.kinds.Alias.FixedFront7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor7[examples.kinds.Alias.FixedBack7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor8"
      ) {
        // format: off
        testTypeCtor8[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor8[examples.kinds.Arity8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor8[examples.kinds.Alias.Renamed8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor8[examples.kinds.Alias.FixedFront8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor8[examples.kinds.Alias.FixedBack8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor9"
      ) {
        // format: off
        testTypeCtor9[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor9[examples.kinds.Arity9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor9[examples.kinds.Alias.Renamed9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor9[examples.kinds.Alias.FixedFront9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor9[examples.kinds.Alias.FixedBack9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor10"
      ) {
        // format: off
        testTypeCtor10[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor10[examples.kinds.Arity10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor10[examples.kinds.Alias.Renamed10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor10[examples.kinds.Alias.FixedFront10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
            "as fixed front" -> Data.map(
              "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
              "reapplied" -> Data("hearth.examples.kinds.Arity11[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
            )
          )
        // format: on

        // format: off
        testTypeCtor10[examples.kinds.Alias.FixedBack10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
            "as fixed back" -> Data.map(
              "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
              "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
            )
          )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor11"
      ) {
        // format: off
        testTypeCtor11[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor11[examples.kinds.Arity11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor11[examples.kinds.Alias.Renamed11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor11[examples.kinds.Alias.FixedFront11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor11[examples.kinds.Alias.FixedBack11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor12"
      ) {
        // format: off
        testTypeCtor12[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor12[examples.kinds.Arity12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor12[examples.kinds.Alias.Renamed12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor12[examples.kinds.Alias.FixedFront12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor12[examples.kinds.Alias.FixedBack12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor13"
      ) {
        // format: off
        testTypeCtor13[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor13[examples.kinds.Arity13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor13[examples.kinds.Alias.Renamed13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor13[examples.kinds.Alias.FixedFront13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor13[examples.kinds.Alias.FixedBack13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor14"
      ) {
        // format: off
        testTypeCtor14[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor14[examples.kinds.Arity14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor14[examples.kinds.Alias.Renamed14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor14[examples.kinds.Alias.FixedFront14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor14[examples.kinds.Alias.FixedBack14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor15"
      ) {
        // format: off
        testTypeCtor15[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor15[examples.kinds.Arity15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor15[examples.kinds.Alias.Renamed15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor15[examples.kinds.Alias.FixedFront15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor15[examples.kinds.Alias.FixedBack15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor16"
      ) {
        // format: off
        testTypeCtor16[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor16[examples.kinds.Arity16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor16[examples.kinds.Alias.Renamed16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor16[examples.kinds.Alias.FixedFront16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor16[examples.kinds.Alias.FixedBack16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor17"
      ) {
        // format: off
        testTypeCtor17[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor17[examples.kinds.Arity17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor17[examples.kinds.Alias.Renamed17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor17[examples.kinds.Alias.FixedFront17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor17[examples.kinds.Alias.FixedBack17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor18"
      ) {
        // format: off
        testTypeCtor18[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor18[examples.kinds.Arity18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor18[examples.kinds.Alias.Renamed18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor18[examples.kinds.Alias.FixedFront18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor18[examples.kinds.Alias.FixedBack18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor19"
      ) {
        // format: off
        testTypeCtor19[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor19[examples.kinds.Arity19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor19[examples.kinds.Alias.Renamed19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor19[examples.kinds.Alias.FixedFront19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor19[examples.kinds.Alias.FixedBack19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor20"
      ) {
        // format: off
        testTypeCtor20[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor20[examples.kinds.Arity20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setT" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor20[examples.kinds.Alias.Renamed20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setT" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor20[examples.kinds.Alias.FixedFront20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor20[examples.kinds.Alias.FixedBack20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor21"
      ) {
        // format: off
        testTypeCtor21[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor21[examples.kinds.Arity21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setT" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setU" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor21[examples.kinds.Alias.Renamed21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setT" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setU" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor21[examples.kinds.Alias.FixedFront21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor21[examples.kinds.Alias.FixedBack21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor22"
      ) {
        // format: off
        testTypeCtor22[String] <==> Data("Not one of the expected types")
        // format: on

        // format: off
        testTypeCtor22[examples.kinds.Arity22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setT" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setU" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setV" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor22[examples.kinds.Alias.Renamed22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setA" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setB" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setC" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setD" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setE" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setF" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setG" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setH" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setI" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setJ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setK" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setL" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setM" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setN" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setO" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setP" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setQ" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setR" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setS" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as setT" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String, java.lang.String]")
          ),
          "as setU" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int, java.lang.String]")
          ),
          "as setV" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Int]")
          )
        )
        // format: on

        // format: off
        testTypeCtor22[examples.kinds.Alias.FixedFront22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity23[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
        // format: on

        // format: off
        testTypeCtor22[examples.kinds.Alias.FixedBack22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity23[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]")
          )
        )
        // format: on
      }
    }
  }
}
