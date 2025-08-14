package hearth
package data

import scala.quoted.*

final private class DataFixtures(q: Quotes) extends MacroCommonsScala3(using q), DataFixturesImpl

object DataFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def example: Data = ${ exampleImpl }
  private def exampleImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).example
}
