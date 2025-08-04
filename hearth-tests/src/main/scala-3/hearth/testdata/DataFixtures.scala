package hearth
package testdata

import scala.quoted.*

final class DataFixtures(q: Quotes) extends MacroCommonsScala3(using q) with DataSupports with DataFixturesImpl

object DataFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def example: Data = ${ exampleImpl }
  def exampleImpl(using q: Quotes): Expr[Data] = new DataFixtures(q).example
}
