package hearth
package typed

import hearth.data.Data
import scala.util.Try
import scala.concurrent.Future
import scala.collection.immutable.Queue

/** Macro implementation is in [[hearth.cq.CrossQuotesMacros]] (Scala 2) and [[hearth.cq.CrossQuotesPlugin]] (Scala 3).
  *
  * Fixtures are in [[CrossQuotesFixturesImpl]] and [[CrossTypesFixturesImpl]].
  */
final class CrossQuotesSpec extends MacroSuite {

  group("CrossQuotes macro/plugin") {

    group("for Type.of") {

      test("should work for simple types") {
        CrossQuotesFixtures.simpleType <==> "scala.Int"
      }

      test("should work for generic types") {
        CrossQuotesFixtures.genericType[Int] <==> "scala.collection.immutable.List[scala.Int]"
      }

      test("should work for unsanitized types") {
        CrossQuotesFixtures.unsanitizedType <==> "scala.collection.immutable.ListMap[scala.Int, java.lang.String]"
      }
    }

    group("methods: Type.Ctor[n].of") {
      import CrossTypesFixtures.*

      test("for Type.Ctor1.of") {
        testTypeCtor1[String] <==> Data("Not an option")
        testTypeCtor1[Option[Int]] <==> Data(
          Map(
            "unapplied" -> Data.list(Data("scala.Int")),
            "reapplied" -> Data("scala.Option[java.lang.String]")
          )
        )
        testTypeCtor1[Option[String]] <==> Data(
          Map(
            "unapplied" -> Data.list(Data("java.lang.String")),
            "reapplied" -> Data("scala.Option[java.lang.String]")
          )
        )
        testTypeCtor1[Option[Option[String]]] <==> Data(
          Map(
            "unapplied" -> Data.list(Data("scala.Option[java.lang.String]")),
            "reapplied" -> Data("scala.Option[java.lang.String]")
          )
        )
        testTypeCtor1[Option[Option[Option[String]]]] <==> Data(
          Map(
            "unapplied" -> Data.list(Data("scala.Option[scala.Option[java.lang.String]]")),
            "reapplied" -> Data("scala.Option[java.lang.String]")
          )
        )
      }

      test("for Type.Ctor2.of") {
        testTypeCtor2[String] <==> Data("Not an either")
        testTypeCtor2[Option[Int]] <==> Data("Not an either")
        testTypeCtor2[Either[Int, String]] <==> Data(
          Map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("java.lang.String")),
            "reapplied" -> Data("scala.util.Either[java.lang.String, java.lang.String]")
          )
        )
      }

      test("for Type.Ctor3.of") {
        testTypeCtor3[String] <==> Data("Not a tuple3")
        testTypeCtor3[Option[Int]] <==> Data("Not a tuple3")
        testTypeCtor3[(Int, String, Double)] <==> Data(
          Map(
            "unapplied" -> Data.list(Data("scala.Int"), Data("java.lang.String"), Data("scala.Double")),
            "reapplied" -> Data("scala.Tuple3[java.lang.String, java.lang.String, java.lang.String]")
          )
        )
      }

      test("for Type.Ctor4.of") {
        testTypeCtor4[String] <==> Data("Not a tuple4")
        testTypeCtor4[Option[Int]] <==> Data("Not a tuple4")
        testTypeCtor4[(Int, String, Double, Boolean)] <==> Data(
          Map(
            "unapplied" -> Data
              .list(Data("scala.Int"), Data("java.lang.String"), Data("scala.Double"), Data("scala.Boolean")),
            "reapplied" -> Data("scala.Tuple4[java.lang.String, java.lang.String, java.lang.String, java.lang.String]")
          )
        )
      }

      test("for Type.Ctor5.of") {
        testTypeCtor5[String] <==> Data("Not a tuple5")
        testTypeCtor5[Option[Int]] <==> Data("Not a tuple5")
        testTypeCtor5[(Int, String, Double, Boolean, Char)] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char")
            ),
            "reapplied" -> Data(
              "scala.Tuple5[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor6.of") {
        testTypeCtor6[String] <==> Data("Not a tuple6")
        testTypeCtor6[Option[Int]] <==> Data("Not a tuple6")
        testTypeCtor6[(Int, String, Double, Boolean, Char, Float)] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float")
            ),
            "reapplied" -> Data(
              "scala.Tuple6[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor7.of") {
        testTypeCtor7[String] <==> Data("Not a tuple7")
        testTypeCtor7[Option[Int]] <==> Data("Not a tuple7")
        testTypeCtor7[(Int, String, Double, Boolean, Char, Float, Long)] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long")
            ),
            "reapplied" -> Data(
              "scala.Tuple7[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor8.of") {
        testTypeCtor8[String] <==> Data("Not a tuple8")
        testTypeCtor8[Option[Int]] <==> Data("Not a tuple8")
        testTypeCtor8[(Int, String, Double, Boolean, Char, Float, Long, Short)] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short")
            ),
            "reapplied" -> Data(
              "scala.Tuple8[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor9.of") {
        testTypeCtor9[String] <==> Data("Not a tuple9")
        testTypeCtor9[Option[Int]] <==> Data("Not a tuple9")
        testTypeCtor9[(Int, String, Double, Boolean, Char, Float, Long, Short, Byte)] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte")
            ),
            "reapplied" -> Data(
              "scala.Tuple9[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor10.of") {
        testTypeCtor10[String] <==> Data("Not a tuple10")
        testTypeCtor10[Option[Int]] <==> Data("Not a tuple10")
        testTypeCtor10[(Int, String, Double, Boolean, Char, Float, Long, Short, Byte, Unit)] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit")
            ),
            "reapplied" -> Data(
              "scala.Tuple10[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor11.of") {
        testTypeCtor11[String] <==> Data("Not a tuple11")
        testTypeCtor11[Option[Int]] <==> Data("Not a tuple11")
        testTypeCtor11[(Int, String, Double, Boolean, Char, Float, Long, Short, Byte, Unit, List[Int])] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]")
            ),
            "reapplied" -> Data(
              "scala.Tuple11[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor12.of") {
        testTypeCtor12[String] <==> Data("Not a tuple12")
        testTypeCtor12[Option[Int]] <==> Data("Not a tuple12")
        testTypeCtor12[
          (Int, String, Double, Boolean, Char, Float, Long, Short, Byte, Unit, List[Int], Set[String])
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]")
            ),
            "reapplied" -> Data(
              "scala.Tuple12[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor13.of") {
        testTypeCtor13[String] <==> Data("Not a tuple13")
        testTypeCtor13[Option[Int]] <==> Data("Not a tuple13")
        testTypeCtor13[
          (Int, String, Double, Boolean, Char, Float, Long, Short, Byte, Unit, List[Int], Set[String], Map[Int, String])
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]")
            ),
            "reapplied" -> Data(
              "scala.Tuple13[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor14.of") {
        testTypeCtor14[String] <==> Data("Not a tuple14")
        testTypeCtor14[Option[Int]] <==> Data("Not a tuple14")
        testTypeCtor14[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]")
            ),
            "reapplied" -> Data(
              "scala.Tuple14[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor15.of") {
        testTypeCtor15[String] <==> Data("Not a tuple15")
        testTypeCtor15[Option[Int]] <==> Data("Not a tuple15")
        testTypeCtor15[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]")
            ),
            "reapplied" -> Data(
              "scala.Tuple15[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor16.of") {
        testTypeCtor16[String] <==> Data("Not a tuple16")
        testTypeCtor16[Option[Int]] <==> Data("Not a tuple16")
        testTypeCtor16[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean],
              Option[Char]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]"),
              Data("scala.Option[scala.Char]")
            ),
            "reapplied" -> Data(
              "scala.Tuple16[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor17.of") {
        testTypeCtor17[String] <==> Data("Not a tuple17")
        testTypeCtor17[Option[Int]] <==> Data("Not a tuple17")
        testTypeCtor17[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean],
              Option[Char],
              Either[Int, String]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]"),
              Data("scala.Option[scala.Char]"),
              Data("scala.util.Either[scala.Int, java.lang.String]")
            ),
            "reapplied" -> Data(
              "scala.Tuple17[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor18.of") {
        testTypeCtor18[String] <==> Data("Not a tuple18")
        testTypeCtor18[Option[Int]] <==> Data("Not a tuple18")
        testTypeCtor18[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean],
              Option[Char],
              Either[Int, String],
              Try[Double]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]"),
              Data("scala.Option[scala.Char]"),
              Data("scala.util.Either[scala.Int, java.lang.String]"),
              Data("scala.util.Try[scala.Double]")
            ),
            "reapplied" -> Data(
              "scala.Tuple18[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor19.of") {
        testTypeCtor19[String] <==> Data("Not a tuple19")
        testTypeCtor19[Option[Int]] <==> Data("Not a tuple19")
        testTypeCtor19[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean],
              Option[Char],
              Either[Int, String],
              Try[Double],
              Future[String]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]"),
              Data("scala.Option[scala.Char]"),
              Data("scala.util.Either[scala.Int, java.lang.String]"),
              Data("scala.util.Try[scala.Double]"),
              Data("scala.concurrent.Future[java.lang.String]")
            ),
            "reapplied" -> Data(
              "scala.Tuple19[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor20.of") {
        testTypeCtor20[String] <==> Data("Not a tuple20")
        testTypeCtor20[Option[Int]] <==> Data("Not a tuple20")
        testTypeCtor20[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean],
              Option[Char],
              Either[Int, String],
              Try[Double],
              Future[String],
              Stream[Int]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]"),
              Data("scala.Option[scala.Char]"),
              Data("scala.util.Either[scala.Int, java.lang.String]"),
              Data("scala.util.Try[scala.Double]"),
              Data("scala.concurrent.Future[java.lang.String]"),
              Data("scala.collection.immutable.Stream[scala.Int]")
            ),
            "reapplied" -> Data(
              "scala.Tuple20[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor21.of") {
        testTypeCtor21[String] <==> Data("Not a tuple21")
        testTypeCtor21[Option[Int]] <==> Data("Not a tuple21")
        testTypeCtor21[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean],
              Option[Char],
              Either[Int, String],
              Try[Double],
              Future[String],
              Stream[Int],
              Queue[String]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]"),
              Data("scala.Option[scala.Char]"),
              Data("scala.util.Either[scala.Int, java.lang.String]"),
              Data("scala.util.Try[scala.Double]"),
              Data("scala.concurrent.Future[java.lang.String]"),
              Data("scala.collection.immutable.Stream[scala.Int]"),
              Data("scala.collection.immutable.Queue[java.lang.String]")
            ),
            "reapplied" -> Data(
              "scala.Tuple21[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
            )
          )
        )
      }

      test("for Type.Ctor22.of") {
        testTypeCtor22[String] <==> Data("Not a tuple22")
        testTypeCtor22[Option[Int]] <==> Data("Not a tuple22")
        testTypeCtor22[
          (
              Int,
              String,
              Double,
              Boolean,
              Char,
              Float,
              Long,
              Short,
              Byte,
              Unit,
              List[Int],
              Set[String],
              Map[Int, String],
              Vector[Double],
              Array[Boolean],
              Option[Char],
              Either[Int, String],
              Try[Double],
              Future[String],
              Stream[Int],
              Queue[String],
              Seq[Double]
          )
        ] <==> Data(
          Map(
            "unapplied" -> Data.list(
              Data("scala.Int"),
              Data("java.lang.String"),
              Data("scala.Double"),
              Data("scala.Boolean"),
              Data("scala.Char"),
              Data("scala.Float"),
              Data("scala.Long"),
              Data("scala.Short"),
              Data("scala.Byte"),
              Data("scala.Unit"),
              Data("scala.collection.immutable.List[scala.Int]"),
              Data("scala.collection.immutable.Set[java.lang.String]"),
              Data("scala.collection.immutable.Map[scala.Int, java.lang.String]"),
              Data("scala.collection.immutable.Vector[scala.Double]"),
              Data("scala.Array[scala.Boolean]"),
              Data("scala.Option[scala.Char]"),
              Data("scala.util.Either[scala.Int, java.lang.String]"),
              Data("scala.util.Try[scala.Double]"),
              Data("scala.concurrent.Future[java.lang.String]"),
              Data("scala.collection.immutable.Stream[scala.Int]"),
              Data("scala.collection.immutable.Queue[java.lang.String]"),
              Data("scala.collection.immutable.Seq[scala.Double]")
            ),
            "reapplied" -> Data(
              "scala.Tuple22[java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String]"
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
