package hearth
package crossquotes

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ImportOrderReproFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ImportOrderReproFixturesImpl {

  def importBeforeTypeOfImpl(expr: c.Expr[Any]): c.Expr[String] = importBeforeTypeOf(expr)
  def importAfterTypeOfImpl(expr: c.Expr[Any]): c.Expr[String] = importAfterTypeOf(expr)
  def classBodyTypeOfBeforeImportImpl: c.Expr[String] = classBodyTypeOfBeforeImport
  def classBodyTypeOfAfterImportImpl: c.Expr[String] = classBodyTypeOfAfterImport
}

object ImportOrderReproFixtures {

  def importBeforeTypeOf(expr: Any): String = macro ImportOrderReproFixtures.importBeforeTypeOfImpl
  def importAfterTypeOf(expr: Any): String = macro ImportOrderReproFixtures.importAfterTypeOfImpl
  def classBodyTypeOfBeforeImport: String = macro ImportOrderReproFixtures.classBodyTypeOfBeforeImportImpl
  def classBodyTypeOfAfterImport: String = macro ImportOrderReproFixtures.classBodyTypeOfAfterImportImpl
}
