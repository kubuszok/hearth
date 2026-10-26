package hearth
package data

import scala.quoted.*

final private class DataFixtures(q: Quotes) extends MacroCommonsScala3(using q), DataFixturesImpl

object DataFixtures {

  inline def example: Data = ${ exampleImpl }
  private def exampleImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).example
}
