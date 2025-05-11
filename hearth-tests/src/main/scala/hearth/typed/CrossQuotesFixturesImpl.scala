package hearth
package typed

trait CrossQuotesFixturesImpl { this: MacroTypedCommons =>

  def simpleExpr: Expr[String] = {
    val e1 = Expr.quote(1)
    val e2 = Expr.quote(2)

    Expr.quote {
      val a = Expr.splice(e1) + Expr.splice(e2)
      a.toString
    }
  }

  // TODO: nested exprs

  // TODO: type args

  // TODO: sanitization tests
}
