package hearth
package crossquotes

import hearth.data.Data

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossQuotesEdgeCasesFixturesImpl]].
  */
final class CrossQuotesEdgeCasesSpec extends MacroSuite {

  group("Cross-Quotes edge cases") {

    group("for Type.of") {

      test("should handle special, singleton, applied, compound, path-dependent and aliased types and implicit Types") {
        CrossQuotesEdgeCasesFixtures.testTypeOfEdgeCases[Int] <==> Data.map(
          "special types" -> Data.map(
            "Nothing" -> Data("scala.Nothing"),
            "Null" -> Data("scala.Null"),
            "Any" -> Data("scala.Any"),
            "AnyRef" -> Data("java.lang.Object"),
            "AnyVal" -> Data("scala.AnyVal"),
            "Unit" -> Data("scala.Unit")
          ),
          "singleton types" -> Data.map(
            "string literal" -> Data("\"foo\""),
            "int literal" -> Data("42"),
            "object" -> Data("hearth.examples.crossquotesedge.SingletonObject.type"),
            "object is not widened" -> Data(false)
          ),
          "applied to the type parameter" -> Data.map(
            "array" -> Data("scala.Array[scala.Int]"),
            "function" -> Data("scala.Function1[scala.Int, java.lang.String]"),
            "tuple" -> Data("scala.Tuple2[scala.Int, java.lang.String]"),
            "nested" -> Data(
              "scala.collection.immutable.Map[java.lang.String, scala.collection.immutable.List[scala.Option[scala.Int]]]"
            )
          ),
          "compound type" -> Data.map(
            "is subtype of the first component" -> Data(true),
            "is subtype of the second component" -> Data(true),
            "component is not a subtype of the compound" -> Data(false)
          ),
          "path-dependent types" -> Data.map(
            "selected through a stable path" -> Data("hearth.examples.crossquotesedge.PathDependent.first.Inner"),
            "different prefixes are different types" -> Data(false),
            "type projection" -> Data("hearth.examples.crossquotesedge.Outer#Inner"),
            "path-dependent type is a subtype of the projection" -> Data(true)
          ),
          "type aliases" -> Data.map(
            "alias is the same as the aliased type" -> Data(true),
            "applied alias is the same as the aliased type" -> Data(true)
          ),
          "implicit Types in scope" -> Data.map(
            "nested method with its own context bound" -> Data("scala.Option[java.lang.String]"),
            "two type parameters" -> Data("scala.util.Either[scala.Int, java.lang.String]"),
            "import from an upper-bounded existential" -> Data("scala.collection.immutable.List[scala.Int]"),
            "abstract type member with an implicit Type in the same class" -> Data(
              "scala.collection.immutable.List[scala.Int]"
            ),
            "imported abstract type member with an implicit Type" -> Data("scala.Option[scala.Int]")
          )
        )
      }
    }

    group("for Type.CtorN.of") {

      test("should unapply subtypes, aliases and special types, apply any type and respect path-dependent prefixes") {
        CrossQuotesEdgeCasesFixtures.testCtorEdgeCases <==> Data.map(
          "unapply on subtypes" -> Data.map(
            "Option on Some[Int]" -> Data("scala.Int"),
            "Option on None.type" -> Data("scala.Nothing"),
            "Seq on List[Int]" -> Data("scala.Int"),
            "Iterable on Map[String, Int]" -> Data("scala.Tuple2[java.lang.String, scala.Int]"),
            "Comparable on String" -> Data("java.lang.String"),
            "Either on Right[String, Int]" -> Data("java.lang.String, scala.Int"),
            "Function1 on a class extending a function" -> Data("scala.Int, scala.Boolean"),
            "Arity2 on a subtype fixing an argument" -> Data("java.lang.String, scala.Int"),
            "Arity2 on a subtype swapping arguments" -> Data("java.lang.String, scala.Int"),
            "subtype constructor on its supertype" -> Data("<no match>")
          ),
          "unapply on aliases" -> Data.map(
            "Option on an alias without type parameters" -> Data("scala.Int"),
            "Either on an applied alias" -> Data("java.lang.String, scala.Int"),
            "Function1 on an alias of a function" -> Data("scala.Int, java.lang.String")
          ),
          "unapply on special types" -> Data.map(
            "Option on Nothing" -> Data("<no match>"),
            "Option on Null" -> Data("<no match>"),
            "Option on Any" -> Data("<no match>")
          ),
          "apply" -> Data.map(
            "Option of Nothing" -> Data("scala.Option[scala.Nothing]"),
            "Option of an applied type" -> Data("scala.Option[scala.collection.immutable.List[scala.Int]]"),
            "Option of a literal type, unapplied back" -> Data("\"x\"")
          ),
          "path-dependent constructor" -> Data.map(
            "apply" -> Data("hearth.examples.crossquotesedge.PathDependent.first.InnerGeneric[scala.Int]"),
            "unapply with the same prefix" -> Data("scala.Int"),
            "unapply with a different prefix" -> Data("<no match>")
          )
        )
      }

      test("UpperBounded.of and Bounded.of should apply and unapply only types within the bounds") {
        CrossQuotesEdgeCasesFixtures.testBoundedCtorClaims <==> Data.map(
          "UpperBounded" -> Data.map(
            "apply" -> Data("scala.Option[java.lang.String]"),
            "unapply within bounds" -> Data("java.lang.String"),
            "unapply outside of bounds" -> Data("<no match>")
          ),
          "Bounded" -> Data.map(
            "apply" -> Data("scala.Option[java.lang.String]"),
            "unapply within bounds" -> Data("java.lang.String"),
            "unapply outside of bounds" -> Data("<no match>")
          ),
          "Bounded with a constructor bounded both ways" -> Data.map(
            "apply" -> Data("hearth.examples.crossquotesedge.Kennel[hearth.examples.crossquotesedge.Dog]"),
            "unapply at the lower bound" -> Data("hearth.examples.crossquotesedge.Puppy"),
            "unapply between the bounds" -> Data("hearth.examples.crossquotesedge.Dog"),
            "unapply at the upper bound" -> Data("hearth.examples.crossquotesedge.Animal")
          )
        )
      }

      test("setA/setB/setC should give the same result in any order and match only the fixed arguments") {
        CrossQuotesEdgeCasesFixtures.testPartialApplicationClaims <==> Data.map(
          "applying all parameters" -> Data.map(
            "from the back" -> Data("hearth.examples.kinds.Arity3[scala.Int, java.lang.String, scala.Double]"),
            "from the front" -> Data("hearth.examples.kinds.Arity3[scala.Int, java.lang.String, scala.Double]")
          ),
          "constructor with the first parameter left" -> Data.map(
            "apply" -> Data("hearth.examples.kinds.Arity3[scala.Boolean, java.lang.String, scala.Double]"),
            "unapply matching fixed parameters" -> Data("scala.Int"),
            "unapply not matching fixed parameters" -> Data("<no match>")
          ),
          "Either with the first parameter fixed" -> Data.map(
            "Either[String, Int]" -> Data(true),
            "Either[String, Double]" -> Data(true),
            "Either[Int, String]" -> Data(false),
            "Option[String]" -> Data(false),
            "Right[String, Int]" -> Data("scala.Int")
          ),
          "Either with the last parameter fixed" -> Data.map(
            "Either[String, Int]" -> Data("java.lang.String"),
            "Either[String, String]" -> Data("<no match>")
          )
        )
      }
    }

    group("for Expr.quote+Expr.splice") {

      test("should support ordinary Scala constructs around splices") {
        CrossQuotesEdgeCasesFixtures.testQuoteEdgeCases <==> Data.map(
          "match with splices in cases" -> Data.map(
            "right" -> Data("right: 5"),
            "left" -> Data("left: five")
          ),
          "splices in string interpolation" -> Data("name=hearth, n=42"),
          "splices as val, var, lazy val and def bodies" -> Data(20),
          "splices as lambda bodies" -> Data("2x2"),
          "while, try-catch and if-else" -> Data("big:10:recovered"),
          "literals and back-ticked names" -> Data(
            "9223372036854775807|-2147483648|c|1.5|2.25|quote\" dollar$ backslash\\|true|bt|$"
          ),
          "default arguments" -> Data(94),
          "implicits defined inside the quote" -> Data("3,2,1"),
          "higher-order functions and for-comprehensions" -> Data("4,6|12,13,21,23|2/4"),
          "by-name argument and function application" -> Data(25),
          "varargs splice" -> Data(6),
          "Unit splice as a statement" -> Data("after unit")
        )
      }

      test("should substitute the type parameter in generic quotes") {
        CrossQuotesEdgeCasesFixtures.testQuoteGenerics(4) <==> Data.map(
          "recursive list building" -> Data("4,4,4"),
          "asInstanceOf to the type parameter" -> Data("4"),
          "type parameter in nested applied types" -> Data("List(Some(4), Some(4))"),
          "explicitly typed quote" -> Data("Some(4)"),
          "implicit summoned inside the quote" -> Data("0")
        )
      }
    }
  }
}
