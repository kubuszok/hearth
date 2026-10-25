package hearth
package crossquotes

import scala.quoted.*

final private class ImportOrderReproFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      ImportOrderReproFixturesImpl

object ImportOrderReproFixtures {

  inline def importBeforeTypeOf(inline expr: Any): String = ${ importBeforeTypeOfImpl('expr) }
  private def importBeforeTypeOfImpl(expr: Expr[Any])(using q: Quotes): Expr[String] =
    new ImportOrderReproFixtures(q).importBeforeTypeOf(expr)

  inline def importAfterTypeOf(inline expr: Any): String = ${ importAfterTypeOfImpl('expr) }
  private def importAfterTypeOfImpl(expr: Expr[Any])(using q: Quotes): Expr[String] =
    new ImportOrderReproFixtures(q).importAfterTypeOf(expr)

  inline def classBodyTypeOfBeforeImport: String = ${ classBodyTypeOfBeforeImportImpl }
  private def classBodyTypeOfBeforeImportImpl(using q: Quotes): Expr[String] =
    new ImportOrderReproFixtures(q).classBodyTypeOfBeforeImport

  inline def classBodyTypeOfAfterImport: String = ${ classBodyTypeOfAfterImportImpl }
  private def classBodyTypeOfAfterImportImpl(using q: Quotes): Expr[String] =
    new ImportOrderReproFixtures(q).classBodyTypeOfAfterImport
}
