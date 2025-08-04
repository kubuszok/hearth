package hearth
package typed

import hearth.testdata.{Data, DataSupports}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final class TypesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with DataSupports with TypesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testNamesPrintersImpl[A: c.WeakTypeTag]: c.Expr[Data] = testNamesPrinters[A]

  def testFlagsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testFlags[A]
}

object TypesFixtures {

  def testNamesPrinters[A]: Data = macro TypesFixtures.testNamesPrintersImpl[A]

  def testFlags[A]: Data = macro TypesFixtures.testFlagsImpl[A]
}
