package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class TypesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with TypesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testNamesPrintersImpl[A: c.WeakTypeTag]: c.Expr[Data] = testNamesPrinters[A]

  def testFlagsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testFlags[A]

  def testTypeCtor1Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor1[A]

  def testTypeCtor2Impl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeCtor2[A]
}

object TypesFixtures {

  def testNamesPrinters[A]: Data = macro TypesFixtures.testNamesPrintersImpl[A]

  def testFlags[A]: Data = macro TypesFixtures.testFlagsImpl[A]

  def testTypeCtor1[A]: Data = macro TypesFixtures.testTypeCtor1Impl[A]

  def testTypeCtor2[A]: Data = macro TypesFixtures.testTypeCtor2Impl[A]
}
