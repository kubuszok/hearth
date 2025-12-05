package hearth
package typed

import hearth.data.Data

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossQuotesFixturesImpl]] and [[CrossTypesFixturesImpl]].
  */
final class CrossQuotesSpec extends MacroSuite {

  group("Cross-Quotes macro/plugin") {

    group("for Type.of") {

      test("should resolve types in the context of the macro compilation, and consider provides implicit Types") {

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
            "viaImport" -> Data("scala.Option[scala.Int]"),
            "viaDeclaredVal" -> Data("scala.Option[scala.Int]"),
            "viaDeclaredDef" -> Data("scala.Option[scala.Int]"),
            "viaInheritance" -> Data("scala.Option[scala.Int]")
          )
        )
      }
    }

    group("for Type.Ctor[n].of") {
      import CrossTypesFixtures.*

      // TODO: test kind projections

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor1"
      ) {
        testTypeCtor1[String] <==> Data("Not one of the expected types")

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

        testTypeCtor1[examples.kinds.Alias.FixedFront1[Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[scala.Unit, java.lang.String]")
          )
        )

        testTypeCtor1[examples.kinds.Alias.FixedBack1[Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, scala.Unit]")
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor2"
      ) {
        testTypeCtor2[String] <==> Data("Not one of the expected types")

        testTypeCtor2[examples.kinds.Arity2[Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          )
        )

        testTypeCtor2[examples.kinds.Alias.Renamed2[Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity2[java.lang.String, java.lang.String]")
          )
        )

        testTypeCtor2[examples.kinds.Alias.FixedFront2[Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[scala.Unit, java.lang.String, java.lang.String]")
          )
        )

        testTypeCtor2[examples.kinds.Alias.FixedBack2[Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, scala.Unit]")
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor3"
      ) {
        testTypeCtor3[String] <==> Data("Not one of the expected types")

        testTypeCtor3[examples.kinds.Arity3[Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          )
        )

        testTypeCtor3[examples.kinds.Alias.Renamed3[Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data("hearth.examples.kinds.Arity3[java.lang.String, java.lang.String, java.lang.String]")
          )
        )

        testTypeCtor3[examples.kinds.Alias.FixedFront3[Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity4[scala.Unit, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor3[examples.kinds.Alias.FixedBack3[Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor4"
      ) {
        testTypeCtor4[String] <==> Data("Not one of the expected types")

        testTypeCtor4[examples.kinds.Arity4[Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor4[examples.kinds.Alias.Renamed4[Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor4[examples.kinds.Alias.FixedFront4[Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity5[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor4[examples.kinds.Alias.FixedBack4[Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor5"
      ) {
        testTypeCtor5[String] <==> Data("Not one of the expected types")

        testTypeCtor5[examples.kinds.Arity5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data
              .list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data
              .list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor5[examples.kinds.Alias.Renamed5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data
              .list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data
              .list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor5[examples.kinds.Alias.FixedFront5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data
              .list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity6[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor5[examples.kinds.Alias.FixedBack5[Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data
              .list(Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int"), Data("scala.Int")),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor6"
      ) {
        testTypeCtor6[String] <==> Data("Not one of the expected types")

        testTypeCtor6[examples.kinds.Arity6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor6[examples.kinds.Alias.Renamed6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor6[examples.kinds.Alias.FixedFront6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity7[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor6[examples.kinds.Alias.FixedBack6[Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor7"
      ) {
        testTypeCtor7[String] <==> Data("Not one of the expected types")

        testTypeCtor7[examples.kinds.Arity7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor7[examples.kinds.Alias.Renamed7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor7[examples.kinds.Alias.FixedFront7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity8[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor7[examples.kinds.Alias.FixedBack7[Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor8"
      ) {
        testTypeCtor8[String] <==> Data("Not one of the expected types")

        testTypeCtor8[examples.kinds.Arity8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor8[examples.kinds.Alias.Renamed8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor8[examples.kinds.Alias.FixedFront8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity9[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor8[examples.kinds.Alias.FixedBack8[Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor9"
      ) {
        testTypeCtor9[String] <==> Data("Not one of the expected types")

        testTypeCtor9[examples.kinds.Arity9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor9[examples.kinds.Alias.Renamed9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor9[examples.kinds.Alias.FixedFront9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity10[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor9[examples.kinds.Alias.FixedBack9[Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor10"
      ) {
        testTypeCtor10[String] <==> Data("Not one of the expected types")

        testTypeCtor10[examples.kinds.Arity10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor10[examples.kinds.Alias.Renamed10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor10[examples.kinds.Alias.FixedFront10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data
          .map(
            "as fixed front" -> Data.map(
              "unapplied" -> Data.list(
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int")
              ),
              "reapplied" -> Data(
                "hearth.examples.kinds.Arity11[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
              )
            )
          )

        testTypeCtor10[examples.kinds.Alias.FixedBack10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data
          .map(
            "as fixed back" -> Data.map(
              "unapplied" -> Data.list(
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int")
              ),
              "reapplied" -> Data(
                "hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
              )
            )
          )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor11"
      ) {
        testTypeCtor11[String] <==> Data("Not one of the expected types")

        testTypeCtor11[examples.kinds.Arity11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor11[examples.kinds.Alias.Renamed11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data
          .map(
            "as class" -> Data.map(
              "unapplied" -> Data.list(
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int")
              ),
              "reapplied" -> Data(
                "hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
              )
            ),
            "as renamed" -> Data.map(
              "unapplied" -> Data.list(
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int")
              ),
              "reapplied" -> Data(
                "hearth.examples.kinds.Arity11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
              )
            )
          )

        testTypeCtor11[
          examples.kinds.Alias.FixedFront11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity12[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor11[
          examples.kinds.Alias.FixedBack11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor12"
      ) {
        testTypeCtor12[String] <==> Data("Not one of the expected types")

        testTypeCtor12[examples.kinds.Arity12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]] <==> Data
          .map(
            "as class" -> Data.map(
              "unapplied" -> Data.list(
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int")
              ),
              "reapplied" -> Data(
                "hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
              )
            ),
            "as renamed" -> Data.map(
              "unapplied" -> Data.list(
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int"),
                Data("scala.Int")
              ),
              "reapplied" -> Data(
                "hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
              )
            )
          )

        testTypeCtor12[
          examples.kinds.Alias.Renamed12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor12[
          examples.kinds.Alias.FixedFront12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity13[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor12[
          examples.kinds.Alias.FixedBack12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor13"
      ) {
        testTypeCtor13[String] <==> Data("Not one of the expected types")

        testTypeCtor13[
          examples.kinds.Arity13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor13[
          examples.kinds.Alias.Renamed13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor13[
          examples.kinds.Alias.FixedFront13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity14[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor13[
          examples.kinds.Alias.FixedBack13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor14"
      ) {
        testTypeCtor14[String] <==> Data("Not one of the expected types")

        testTypeCtor14[
          examples.kinds.Arity14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor14[
          examples.kinds.Alias.Renamed14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor14[
          examples.kinds.Alias.FixedFront14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity15[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor14[
          examples.kinds.Alias.FixedBack14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor15"
      ) {
        testTypeCtor15[String] <==> Data("Not one of the expected types")

        testTypeCtor15[
          examples.kinds.Arity15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor15[
          examples.kinds.Alias.Renamed15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor15[
          examples.kinds.Alias.FixedFront15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity16[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor15[
          examples.kinds.Alias.FixedBack15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor16"
      ) {
        testTypeCtor16[String] <==> Data("Not one of the expected types")

        testTypeCtor16[
          examples.kinds.Arity16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor16[
          examples.kinds.Alias.Renamed16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor16[examples.kinds.Alias.FixedFront16[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity17[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor16[examples.kinds.Alias.FixedBack16[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor17"
      ) {
        testTypeCtor17[String] <==> Data("Not one of the expected types")

        testTypeCtor17[
          examples.kinds.Arity17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
        ] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor17[examples.kinds.Alias.Renamed17[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor17[examples.kinds.Alias.FixedFront17[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity18[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor17[examples.kinds.Alias.FixedBack17[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor18"
      ) {
        testTypeCtor18[String] <==> Data("Not one of the expected types")

        testTypeCtor18[examples.kinds.Arity18[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor18[examples.kinds.Alias.Renamed18[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor18[examples.kinds.Alias.FixedFront18[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity19[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor18[examples.kinds.Alias.FixedBack18[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor19"
      ) {
        testTypeCtor19[String] <==> Data("Not one of the expected types")

        testTypeCtor19[examples.kinds.Arity19[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor19[examples.kinds.Alias.Renamed19[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor19[examples.kinds.Alias.FixedFront19[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity20[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor19[examples.kinds.Alias.FixedBack19[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor20"
      ) {
        testTypeCtor20[String] <==> Data("Not one of the expected types")

        testTypeCtor20[examples.kinds.Arity20[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor20[examples.kinds.Alias.Renamed20[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor20[examples.kinds.Alias.FixedFront20[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity21[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor20[examples.kinds.Alias.FixedBack20[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor21"
      ) {
        testTypeCtor21[String] <==> Data("Not one of the expected types")

        testTypeCtor21[examples.kinds.Arity21[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor21[examples.kinds.Alias.Renamed21[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor21[examples.kinds.Alias.FixedFront21[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity22[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor21[examples.kinds.Alias.FixedBack21[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }

      test(
        "should resolve types in the context of the macro compilation, and properly handle type aliases and kind projections for Type.Ctor22"
      ) {
        testTypeCtor22[String] <==> Data("Not one of the expected types")

        testTypeCtor22[examples.kinds.Arity22[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor22[examples.kinds.Alias.Renamed22[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as class" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          ),
          "as renamed" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor22[examples.kinds.Alias.FixedFront22[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed front" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity23[scala.Unit, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )

        testTypeCtor22[examples.kinds.Alias.FixedBack22[
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
        ]] <==> Data.map(
          "as fixed back" -> Data.map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int"),
              Data("scala.Int")
            ),
            "reapplied" -> Data(
              "hearth.examples.kinds.Arity23[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, scala.Unit]"
            )
          )
        )
      }
    }

    group("for Expr.quote+Expr.splice") {

      test("should work for simple expressions") {
        CrossQuotesFixtures.simpleExpr <==> "3"
      }

      test("should work for generic expressions") {
        CrossQuotesFixtures.genericExpr(4) <==> "4"
      }

      test("should work for unsanitized expressions") {
        CrossQuotesFixtures.unsanitizedExpr <==> "ListMap(1 -> 2)"
      }

      test("should work for nested expressions") {
        CrossQuotesFixtures.nestedExpr <==> "42"
      }
    }
  }
}
