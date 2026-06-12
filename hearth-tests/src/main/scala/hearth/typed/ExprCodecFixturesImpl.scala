package hearth
package typed

import hearth.data.Data

trait ExprCodecFixturesImpl { this: MacroCommons =>

  // -- Expr.semiQuote direct tests --

  // Cross-quotes Type.of cannot be assigned directly to a local implicit val (the expansion would self-reference
  // the implicit being defined on Scala 2), so we follow the same pattern as AnonymousInstanceFixturesImpl.
  private val intTypeEC: Type[Int] = Type.of[Int]
  private val stringTypeEC: Type[String] = Type.of[String]
  private val booleanTypeEC: Type[Boolean] = Type.of[Boolean]
  private val doubleTypeEC: Type[Double] = Type.of[Double]
  private val anyTypeEC: Type[Any] = Type.of[Any]
  private val serverConfigTypeEC: Type[hearth.examples.expr_codecs.ServerConfig] =
    Type.of[hearth.examples.expr_codecs.ServerConfig]
  private val appConfigTypeEC: Type[hearth.examples.expr_codecs.AppConfig] =
    Type.of[hearth.examples.expr_codecs.AppConfig]
  private val shapeTypeEC: Type[hearth.examples.expr_codecs.Shape] =
    Type.of[hearth.examples.expr_codecs.Shape]
  private val sentinelTypeEC: Type[hearth.examples.expr_codecs.Sentinel.type] =
    Type.of[hearth.examples.expr_codecs.Sentinel.type]
  private val notQuotableTypeEC: Type[hearth.examples.expr_codecs.NotQuotable] =
    Type.of[hearth.examples.expr_codecs.NotQuotable]

  private def renderSemiQuote[A: Type](result: Either[String, Expr[A]]): Data =
    result.fold(error => Data(s"<failed: ${removeAnsiColors(error)}>"), expr => Data(expr.plainPrint))

  def testSemiQuotePrimitives: Expr[Data] = {
    implicit val intType: Type[Int] = intTypeEC
    implicit val stringType: Type[String] = stringTypeEC
    implicit val booleanType: Type[Boolean] = booleanTypeEC
    implicit val doubleType: Type[Double] = doubleTypeEC
    Expr(
      Data.map(
        "int" -> renderSemiQuote(Expr.semiQuote(42)),
        "string" -> renderSemiQuote(Expr.semiQuote("quoted")),
        "boolean" -> renderSemiQuote(Expr.semiQuote(true)),
        "double" -> renderSemiQuote(Expr.semiQuote(3.14))
      )
    )
  }

  def testSemiQuoteCaseClass: Expr[Data] = {
    implicit val serverConfigType: Type[hearth.examples.expr_codecs.ServerConfig] = serverConfigTypeEC
    implicit val appConfigType: Type[hearth.examples.expr_codecs.AppConfig] = appConfigTypeEC
    Expr(
      Data.map(
        "simple" -> renderSemiQuote(
          Expr.semiQuote(hearth.examples.expr_codecs.ServerConfig("localhost", 8080))
        ),
        "nested" -> renderSemiQuote(
          Expr.semiQuote(
            hearth.examples.expr_codecs.AppConfig(hearth.examples.expr_codecs.ServerConfig("db.local", 5432), true)
          )
        )
      )
    )
  }

  def testSemiQuoteSealedChild: Expr[Data] = {
    implicit val shapeType: Type[hearth.examples.expr_codecs.Shape] = shapeTypeEC
    Expr(
      Data.map(
        "caseClassChild" -> renderSemiQuote(
          Expr.semiQuote[hearth.examples.expr_codecs.Shape](hearth.examples.expr_codecs.Shape.Circle(3.14))
        ),
        "caseObjectChild" -> renderSemiQuote(
          Expr.semiQuote[hearth.examples.expr_codecs.Shape](hearth.examples.expr_codecs.Shape.Origin)
        )
      )
    )
  }

  def testSemiQuoteSingleton: Expr[Data] = {
    implicit val sentinelType: Type[hearth.examples.expr_codecs.Sentinel.type] = sentinelTypeEC
    Expr(
      Data.map(
        "caseObject" -> renderSemiQuote(Expr.semiQuote(hearth.examples.expr_codecs.Sentinel))
      )
    )
  }

  def testSemiQuoteWithOverride: Expr[Data] = {
    implicit val stringType: Type[String] = stringTypeEC
    implicit val serverConfigType: Type[hearth.examples.expr_codecs.ServerConfig] = serverConfigTypeEC
    val noOverride: Existential[QuoteOverride] = {
      implicit val anyType: Type[Any] = anyTypeEC
      Existential[QuoteOverride, Any](QuoteOverride.none[Any])
    }
    val overrides: UntypedType => Existential[QuoteOverride] = { tpe =>
      if (tpe =:= UntypedType.fromTyped[String])
        Existential[QuoteOverride, String](QuoteOverride[String](value => Right(Expr(value.toUpperCase))))
      else noOverride
    }
    Expr(
      Data.map(
        "direct" -> renderSemiQuote(Expr.semiQuote("hello", overrides)),
        "nested" -> renderSemiQuote(
          Expr.semiQuote(hearth.examples.expr_codecs.ServerConfig("localhost", 8080), overrides)
        )
      )
    )
  }

  def testSemiQuoteFailure: Expr[Data] = {
    implicit val notQuotableType: Type[hearth.examples.expr_codecs.NotQuotable] = notQuotableTypeEC
    Expr(
      Data.map(
        "notQuotable" -> renderSemiQuote(Expr.semiQuote(new hearth.examples.expr_codecs.NotQuotable(1)))
      )
    )
  }

  // -- ExprCodec.derived round-trip --

  def testExprCodecRoundTrip[A: Type](expr: Expr[A]): Expr[Data] = {
    val codec: ExprCodec[A] = ExprCodec.derived[A]
    codec.fromExpr(expr) match {
      case Some(value) =>
        val reLifted: Expr[A] = codec.toExpr(value)
        Expr(
          Data.map(
            "decoded" -> Data(value.toString),
            "reLifted" -> Data(reLifted.plainPrint)
          )
        )
      case None =>
        Expr(
          Data.map(
            "decoded" -> Data("<failed>"),
            "exprPrint" -> Data(expr.plainPrint)
          )
        )
    }
  }
}
