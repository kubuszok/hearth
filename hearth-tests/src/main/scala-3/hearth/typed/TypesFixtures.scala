package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class TypesFixtures(q: Quotes) extends MacroCommonsScala3(using q), TypesFixturesImpl

object TypesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testNamesPrinters[A]: Data = ${ testNamesPrintersImpl[A] }
  private def testNamesPrintersImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testNamesPrinters[A]

  inline def testFlags[A]: Data = ${ testFlagsImpl[A] }
  private def testFlagsImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testFlags[A]

  inline def testTypeCtor1[A]: Data = ${ testTypeCtor1Impl[A] }
  private def testTypeCtor1Impl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testTypeCtor1[A]

  inline def testTypeCtor2[A]: Data = ${ testTypeCtor2Impl[A] }
  private def testTypeCtor2Impl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testTypeCtor2[A]
}
