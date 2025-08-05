package hearth
package testdata

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class DataFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with DataSupports
    with DataFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def exampleImpl: c.Expr[Data] = example
}
object DataFixtures {

  def example: Data = macro DataFixtures.exampleImpl
}
