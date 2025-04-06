package hearth
package typed

trait TypesFixturesImpl { this: MacroTypedCommons =>

  def testNamesPrinters[A: Type]: Expr[String] =
    Expr(
      s"""Short:  ${Type.shortName[A]}
         |FCQN:   ${Type.fcqn[A]}
         |Plain:  ${Type.plainPrint[A]}
         |Pretty: ${Type.prettyPrint[A]}""".stripMargin
    )
}
