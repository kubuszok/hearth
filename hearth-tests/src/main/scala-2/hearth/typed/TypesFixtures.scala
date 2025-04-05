package hearth
package typed

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final class TypesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with TypesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testNamesPrintersImpl[A: c.WeakTypeTag]: c.Expr[String] = testNamesPrinters[A]
}

object TypesFixtures {

  def testNamesPrinters[A]: String = macro TypesFixtures.testNamesPrintersImpl[A]
}
