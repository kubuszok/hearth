package hearth
package crossquotes

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ImportOrderFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ImportOrderFixturesImpl {

  def importBeforeTypeOfImpl(expr: c.Expr[Any]): c.Expr[String] = importBeforeTypeOf(expr)
  def importAfterTypeOfImpl(expr: c.Expr[Any]): c.Expr[String] = importAfterTypeOf(expr)
  def classBodyTypeOfBeforeImportImpl: c.Expr[String] = classBodyTypeOfBeforeImport
  def classBodyTypeOfAfterImportImpl: c.Expr[String] = classBodyTypeOfAfterImport
}

object ImportOrderFixtures {

  def importBeforeTypeOf(expr: Any): String = macro ImportOrderFixtures.importBeforeTypeOfImpl
  def importAfterTypeOf(expr: Any): String = macro ImportOrderFixtures.importAfterTypeOfImpl
  def classBodyTypeOfBeforeImport: String = macro ImportOrderFixtures.classBodyTypeOfBeforeImportImpl
  def classBodyTypeOfAfterImport: String = macro ImportOrderFixtures.classBodyTypeOfAfterImportImpl
}
