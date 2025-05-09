package hearth
package typed

trait CompilerPluginTest { this: MacroTypedCommons =>

  def input: Expr[String] = {
    val e1 = Expr.quote(1)
    val e2 = Expr.quote(2)

    Expr.quote {
      val a = Expr.splice(e1) + Expr.splice(e2)
      a.toString
    }
  }

  // scalafmt: { runner.dialect = scala3 }
  // def output: Expr[String] = {
  //   val e1 = {
  //     implicit val quotes: scala.quoted.Quotes = CrossQuotes.ctx
  //     CrossQuotes.castK[scala.quoted.Expr, Expr]('{ 1 })
  //   }
  //   val e2 = {
  //     implicit val quotes: scala.quoted.Quotes = CrossQuotes.ctx
  //     CrossQuotes.castK[scala.quoted.Expr, Expr]('{ 2 })
  //   }

  //   {
  //     implicit val quotes: scala.quoted.Quotes = CrossQuotes.ctx
  //     CrossQuotes.castK[scala.quoted.Expr, Expr]('{
  //       val a = ${ CrossQuotes.castK[Expr, scala.quoted.Expr](e1) } + ${ CrossQuotes.castK[Expr, scala.quoted.Expr](e2) }
  //       a.toString
  //     })
  //   }
  // }
}
