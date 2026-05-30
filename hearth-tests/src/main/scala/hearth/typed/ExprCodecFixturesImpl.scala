package hearth
package typed

import hearth.data.Data

trait ExprCodecFixturesImpl { this: MacroCommons =>

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
