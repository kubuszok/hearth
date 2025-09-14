package hearth
package typed

import hearth.data.Data

/** Fixtured for testing [[ExprsSpec]]. */
trait ExprsFixturesImpl { this: MacroTypedCommons =>

  def testExprPrinters[A: Type](expr: Expr[A]): Expr[Data] = Expr(
    Data.map(
      "Expr.plainPrint" -> Data(Expr.plainPrint(expr)),
      "Expr.prettyPrint" -> Data(removeAnsiColors((Expr.prettyPrint(expr)))),
      "Expr.plainAST" -> Data(Expr.plainAST(expr)),
      "Expr.prettyAST" -> Data(removeAnsiColors((Expr.prettyAST(expr))))
    )
  )

  def testExprSummoning[A: Type]: Expr[Data] = Expr(
    Data.map(
      "Expr.summonImplicit" -> Data(Expr.summonImplicit[A].fold("<failed to summon>")(_.plainPrint))
    )
  )

  def testExprUpcasting[A: Type, B: Type](expr: Expr[A]): Expr[Data] = Expr(
    Data.map(
      "Expr.upcast" -> Data(scala.util.Try(Expr.upcast[A, B](expr)).toOption.fold("<failed to upcast>")(_.plainPrint))
    )
  )

  def testSuppressUnused[A: Type](expr: Expr[A]): Expr[Unit] =
    Expr.suppressUnused(expr)
}
