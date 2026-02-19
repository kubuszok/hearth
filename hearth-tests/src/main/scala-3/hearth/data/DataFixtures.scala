package hearth
package data

import scala.quoted.*

final private class DataFixtures(q: Quotes) extends MacroCommonsScala3(using q), DataFixturesImpl

object DataFixtures {

  // [hearth#176]

  inline def example: Data = ${ exampleImpl }
  private def exampleImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).example
}
