package hearth
package data

import scala.quoted.*

final private class DataFixtures(q: Quotes) extends MacroCommonsScala3(using q), DataFixturesImpl

object DataFixtures {

  inline def example: Data = ${ exampleImpl }
  private def exampleImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).example

  inline def exampleEdgeCases: Data = ${ exampleEdgeCasesImpl }
  private def exampleEdgeCasesImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).exampleEdgeCases

  inline def exampleEmptyMap: Data = ${ exampleEmptyMapImpl }
  private def exampleEmptyMapImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).exampleEmptyMap

  inline def exampleNestedEmptyMap: Data = ${ exampleNestedEmptyMapImpl }
  private def exampleNestedEmptyMapImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).exampleNestedEmptyMap
}
