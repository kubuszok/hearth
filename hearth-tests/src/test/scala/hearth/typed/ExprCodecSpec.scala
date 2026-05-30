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
            s2 = "new hearth.examples.expr_codecs.ServerConfig((\"localhost\": scala.Predef.String), (8080: scala.Int))",
            s3 = "new hearth.examples.expr_codecs.ServerConfig(\"localhost\", 8080)"
          )
        )
      }

      test("nested case class") {
        testExprCodecRoundTrip(AppConfig(ServerConfig("db.local", 5432), true)) <==> Data.map(
          "decoded" -> Data("AppConfig(ServerConfig(db.local,5432),true)"),
          "reLifted" -> caseClassReLifted(
            s2 = "new hearth.examples.expr_codecs.AppConfig(new hearth.examples.expr_codecs.ServerConfig((\"db.local\": scala.Predef.String), (5432: scala.Int)), (true: scala.Boolean))",
            s3 = "new hearth.examples.expr_codecs.AppConfig(new hearth.examples.expr_codecs.ServerConfig(\"db.local\", 5432), true)"
          )
        )
      }
    }

    group("sealed traits") {

      test("case class child") {
        testExprCodecRoundTrip[Shape](Shape.Circle(3.14)) <==> Data.map(
          "decoded" -> Data("Circle(3.14)"),
          "reLifted" -> caseClassReLifted(
            s2 = "(new hearth.examples.expr_codecs.Shape.Circle((3.14: scala.Double)): hearth.examples.expr_codecs.Shape)",
            s3 = "new hearth.examples.expr_codecs.Shape.Circle(3.14)"
          )
        )
      }

      test("case class child with multiple fields") {
        testExprCodecRoundTrip[Shape](Shape.Rectangle(10.0, 20.0)) <==> Data.map(
          "decoded" -> Data("Rectangle(10.0,20.0)"),
          "reLifted" -> caseClassReLifted(
            s2 = "(new hearth.examples.expr_codecs.Shape.Rectangle((10.0: scala.Double), (20.0: scala.Double)): hearth.examples.expr_codecs.Shape)",
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
}
