package hearth
package data

import hearth.data.{Data, DataFixturesImpl}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class DataFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with DataFixturesImpl {

  def exampleImpl: c.Expr[Data] = example
  def exampleEdgeCasesImpl: c.Expr[Data] = exampleEdgeCases
  def exampleEmptyMapImpl: c.Expr[Data] = exampleEmptyMap
  def exampleNestedEmptyMapImpl: c.Expr[Data] = exampleNestedEmptyMap
}
object DataFixtures {

  def example: Data = macro DataFixtures.exampleImpl
  def exampleEdgeCases: Data = macro DataFixtures.exampleEdgeCasesImpl
  def exampleEmptyMap: Data = macro DataFixtures.exampleEmptyMapImpl
  def exampleNestedEmptyMap: Data = macro DataFixtures.exampleNestedEmptyMapImpl
}
