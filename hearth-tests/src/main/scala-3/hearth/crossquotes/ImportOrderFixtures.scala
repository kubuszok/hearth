package hearth
package crossquotes

import scala.quoted.*

final private class ImportOrderFixtures(q: Quotes) extends MacroCommonsScala3(using q), ImportOrderFixturesImpl

object ImportOrderFixtures {

  inline def importBeforeTypeOf(inline expr: Any): String = ${ importBeforeTypeOfImpl('expr) }
  private def importBeforeTypeOfImpl(expr: Expr[Any])(using q: Quotes): Expr[String] =
    new ImportOrderFixtures(q).importBeforeTypeOf(expr)

  inline def importAfterTypeOf(inline expr: Any): String = ${ importAfterTypeOfImpl('expr) }
  private def importAfterTypeOfImpl(expr: Expr[Any])(using q: Quotes): Expr[String] =
    new ImportOrderFixtures(q).importAfterTypeOf(expr)

  inline def classBodyTypeOfBeforeImport: String = ${ classBodyTypeOfBeforeImportImpl }
  private def classBodyTypeOfBeforeImportImpl(using q: Quotes): Expr[String] =
    new ImportOrderFixtures(q).classBodyTypeOfBeforeImport

  inline def classBodyTypeOfAfterImport: String = ${ classBodyTypeOfAfterImportImpl }
  private def classBodyTypeOfAfterImportImpl(using q: Quotes): Expr[String] =
    new ImportOrderFixtures(q).classBodyTypeOfAfterImport
}
