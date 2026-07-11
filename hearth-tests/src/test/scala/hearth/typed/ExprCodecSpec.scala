package hearth
package typed

import hearth.data.Data
import hearth.examples.expr_codecs.*

final class ExprCodecSpec extends MacroSuite {

  private val isScala2 = LanguageVersion.byHearth.isScala2_13

  private def caseClassReLifted(s2: String, s3: String): Data =
    if (isScala2) Data(s2) else Data(s3)

  private def singletonReLifted(s2fqn: String, s3short: String): Data =
    if (isScala2) Data(s2fqn) else Data(s3short)

  group("ExprCodec.derived round-trip") {
    import ExprCodecFixtures.testExprCodecRoundTrip

    group("case classes") {

      test("simple case class") {
        testExprCodecRoundTrip(ServerConfig("localhost", 8080)) <==> Data.map(
          "decoded" -> Data("ServerConfig(localhost,8080)"),
          "reLifted" -> caseClassReLifted(
            s2 =
              "new hearth.examples.expr_codecs.ServerConfig((\"localhost\": scala.Predef.String), (8080: scala.Int))",
            s3 = "new hearth.examples.expr_codecs.ServerConfig(\"localhost\", 8080)"
          )
        )
      }

      test("nested case class") {
        testExprCodecRoundTrip(AppConfig(ServerConfig("db.local", 5432), true)) <==> Data.map(
          "decoded" -> Data("AppConfig(ServerConfig(db.local,5432),true)"),
          "reLifted" -> caseClassReLifted(
            s2 =
              "new hearth.examples.expr_codecs.AppConfig(new hearth.examples.expr_codecs.ServerConfig((\"db.local\": scala.Predef.String), (5432: scala.Int)), (true: scala.Boolean))",
            s3 =
              "new hearth.examples.expr_codecs.AppConfig(new hearth.examples.expr_codecs.ServerConfig(\"db.local\", 5432), true)"
          )
        )
      }

      test("vararg case class") {
        // fromExpr semi-evaluates the vararg apply call, toExpr re-lifts the (normalized) Seq[Int] field through
        // the built-in SeqExprCodec and re-splices it as `seq: _*` via the vararg-aware construct path.
        testExprCodecRoundTrip(VarargNumbers(1, 2, 3)) <==> Data.map(
          "decoded" -> Data("VarargNumbers(List(1, 2, 3))"),
          "reLifted" -> caseClassReLifted(
            s2 = "new hearth.examples.expr_codecs.VarargNumbers((scala.collection.immutable.Seq(1, 2, 3): _*))",
            s3 = "new hearth.examples.expr_codecs.VarargNumbers(scala.Seq.apply[scala.Int](1, 2, 3): _*)"
          )
        )
      }
    }

    group("sealed traits") {

      test("case class child") {
        testExprCodecRoundTrip[Shape](Shape.Circle(3.14)) <==> Data.map(
          "decoded" -> Data("Circle(3.14)"),
          "reLifted" -> caseClassReLifted(
            s2 =
              "(new hearth.examples.expr_codecs.Shape.Circle((3.14: scala.Double)): hearth.examples.expr_codecs.Shape)",
            s3 = "new hearth.examples.expr_codecs.Shape.Circle(3.14)"
          )
        )
      }

      test("case class child with multiple fields") {
        testExprCodecRoundTrip[Shape](Shape.Rectangle(10.0, 20.0)) <==> Data.map(
          "decoded" -> Data("Rectangle(10.0,20.0)"),
          "reLifted" -> caseClassReLifted(
            s2 =
              "(new hearth.examples.expr_codecs.Shape.Rectangle((10.0: scala.Double), (20.0: scala.Double)): hearth.examples.expr_codecs.Shape)",
            s3 = "new hearth.examples.expr_codecs.Shape.Rectangle(10.0, 20.0)"
          )
        )
      }

      test("case object child") {
        testExprCodecRoundTrip[Shape](Shape.Origin) <==> Data.map(
          "decoded" -> Data("Origin"),
          "reLifted" -> singletonReLifted(
            s2fqn = "(hearth.examples.expr_codecs.Shape.Origin: hearth.examples.expr_codecs.Shape)",
            s3short = "expr_codecs.Shape.Origin"
          )
        )
      }

      // Regression: `semiQuoteEnum` must dispatch on the value's RUNTIME CLASS, not `getClass.getSimpleName`. `List`
      // decomposes as the sealed `:: | Nil`, and `::`'s JVM-encoded simple name is `$colon$colon` (not `::`), so
      // simple-name dispatch failed to re-lift a non-empty list ("No child for $colon$colon").
      test("List re-lifts through its sealed `:: | Nil` decomposition (encoded child name)") {
        import ExprCodecFixtures.testSemiQuoteReLift
        testSemiQuoteReLift[List[Int]](List(1, 2, 3)) <==> Data("reLifted: List(1, 2, 3)")
      }
    }

    group("case class with Data field") {

      test("round-trip case class containing Data") {
        testExprCodecRoundTrip(DataHolder("test", Data("hello"))) <==> Data.map(
          "decoded" -> Data("DataHolder(test,hello)"),
          "reLifted" -> caseClassReLifted(
            s2 =
              "new hearth.examples.expr_codecs.DataHolder((\"test\": scala.Predef.String), hearth.data.Data.apply(\"hello\"))",
            s3 = "new hearth.examples.expr_codecs.DataHolder(\"test\", hearth.data.Data$package.Data.apply(\"hello\"))"
          )
        )
      }
    }

    group("singletons") {

      test("standalone case object") {
        testExprCodecRoundTrip(Sentinel) <==> Data.map(
          "decoded" -> Data("Sentinel"),
          "reLifted" -> singletonReLifted(
            s2fqn = "hearth.examples.expr_codecs.Sentinel",
            s3short = "expr_codecs.Sentinel"
          )
        )
      }
    }
  }

  group("ExprCodec.derived negative paths") {

    test("toExpr on a type with an unquotable field aborts with a clean message naming the type") {
      // HolderOfNotQuotable has a `payload: NotQuotable` field; NotQuotable is neither case class, singleton, sealed,
      // nor has a built-in/summonable codec. The derived toExpr cannot quote it, so it must abort with a clean,
      // positioned diagnostic (not an opaque uncaught AssertionError).
      compileErrors("ExprCodecFixtures.testDeriveToExprFailure").check(
        "ExprCodec.derived[hearth.examples.expr_codecs.HolderOfNotQuotable].toExpr failed",
        "Cannot semi-quote value of type hearth.examples.expr_codecs.NotQuotable"
      )
    }

    test("fromExpr on an unsupported type still succeeds via generic semiEval (asymmetric with toExpr)") {
      // toExpr aborts for NotQuotable (no quote case), but fromExpr decodes the `new NotQuotable(1)` constructor call
      // through Expr.semiEval, so it round-trips the value rather than failing.
      ExprCodecFixtures.testDeriveFromExprFailure(new NotQuotable(1)) <==> Data.map(
        "decodedValue" -> Data("1")
      )
    }
  }

  group("ExprCodec override-map =:= cache") {

    test("one override keyed by a type is reused for every field of that type") {
      // TwoHosts has two String fields; a single QuoteOverride keyed by String must apply to BOTH (=:= lookup,
      // not identity), proving dedup-by-=:= does not drop a repeated occurrence.
      ExprCodecFixtures.testOverrideMapReuseForRepeatedFieldType <==> Data.map(
        "bothFieldsOverridden" -> caseClassReLifted(
          s2 =
            "new hearth.examples.expr_codecs.TwoHosts((\"ALPHA\": scala.Predef.String), (\"BETA\": scala.Predef.String))",
          s3 = "new hearth.examples.expr_codecs.TwoHosts(\"ALPHA\", \"BETA\")"
        )
      )
    }
  }

  group("built-in parameterized codecs") {

    test("Expr.typeOf reports the concrete instantiated type for exprs produced by built-in codecs") {
      // Regression: on Scala 2 ExprCodec.make captured the codec's own free type parameter A in the WeakTypeTag,
      // so Expr.typeOf reported `A` instead of e.g. Seq[Int], breaking downstream upcast calls.
      ExprCodecFixtures.testBuiltInCodecExprTypes <==> Data.map(
        "Seq[Int]" -> Data("ok"),
        "List[Int]" -> Data("ok"),
        "Option[Int]" -> Data("ok"),
        "Map[String, Int]" -> Data("ok")
      )
    }
  }

  group("Expr.semiQuote direct usage") {

    test("quotes primitives and String via built-in codecs") {
      ExprCodecFixtures.testSemiQuotePrimitives <==> Data.map(
        "int" -> Data("42"),
        "string" -> Data("\"quoted\""),
        "boolean" -> Data("true"),
        "double" -> Data("3.14")
      )
    }

    test("quotes case classes (recursively)") {
      ExprCodecFixtures.testSemiQuoteCaseClass <==> Data.map(
        "simple" -> caseClassReLifted(
          s2 = "new hearth.examples.expr_codecs.ServerConfig((\"localhost\": scala.Predef.String), (8080: scala.Int))",
          s3 = "new hearth.examples.expr_codecs.ServerConfig(\"localhost\", 8080)"
        ),
        "nested" -> caseClassReLifted(
          s2 =
            "new hearth.examples.expr_codecs.AppConfig(new hearth.examples.expr_codecs.ServerConfig((\"db.local\": scala.Predef.String), (5432: scala.Int)), (true: scala.Boolean))",
          s3 =
            "new hearth.examples.expr_codecs.AppConfig(new hearth.examples.expr_codecs.ServerConfig(\"db.local\", 5432), true)"
        )
      )
    }

    test("quotes children of a sealed hierarchy (upcast to the parent)") {
      ExprCodecFixtures.testSemiQuoteSealedChild <==> Data.map(
        "caseClassChild" -> caseClassReLifted(
          s2 =
            "(new hearth.examples.expr_codecs.Shape.Circle((3.14: scala.Double)): hearth.examples.expr_codecs.Shape)",
          s3 = "new hearth.examples.expr_codecs.Shape.Circle(3.14)"
        ),
        "caseObjectChild" -> singletonReLifted(
          s2fqn = "(hearth.examples.expr_codecs.Shape.Origin: hearth.examples.expr_codecs.Shape)",
          s3short = "expr_codecs.Shape.Origin"
        )
      )
    }

    test("quotes a standalone case object") {
      ExprCodecFixtures.testSemiQuoteSingleton <==> Data.map(
        "caseObject" -> singletonReLifted(
          s2fqn = "hearth.examples.expr_codecs.Sentinel",
          s3short = "expr_codecs.Sentinel"
        )
      )
    }

    test("QuoteOverride customizes quoting for a chosen type (also inside case classes)") {
      ExprCodecFixtures.testSemiQuoteWithOverride <==> Data.map(
        "direct" -> Data("\"HELLO\""),
        "nested" -> caseClassReLifted(
          s2 = "new hearth.examples.expr_codecs.ServerConfig((\"LOCALHOST\": scala.Predef.String), (8080: scala.Int))",
          s3 = "new hearth.examples.expr_codecs.ServerConfig(\"LOCALHOST\", 8080)"
        )
      )
    }

    test("returns Left with a message for a type that cannot be quoted") {
      ExprCodecFixtures.testSemiQuoteFailure <==> Data.map(
        "notQuotable" -> Data("<failed: Cannot semi-quote value of type hearth.examples.expr_codecs.NotQuotable>")
      )
    }
  }
}
