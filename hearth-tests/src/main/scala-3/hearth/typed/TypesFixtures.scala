package hearth
package typed

import hearth.testdata.{Data, DataSupports}

import scala.quoted.*

final private class TypesFixtures(q: Quotes) extends MacroCommonsScala3(using q), DataSupports, TypesFixturesImpl

object TypesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testNamesPrinters[A]: Data = ${ testNamesPrintersImpl[A] }
  private def testNamesPrintersImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testNamesPrinters[A]

  inline def testFlags[A]: Data = ${ testFlagsImpl[A] }
  private def testFlagsImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testFlags[A]

  inline def testTypeCtor[A]: Data = ${ testTypeCtorImpl[A] }
  private def testTypeCtorImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testTypeCtor[A]
}
