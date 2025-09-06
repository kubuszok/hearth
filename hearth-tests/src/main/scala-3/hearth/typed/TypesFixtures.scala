package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class TypesFixtures(q: Quotes) extends MacroCommonsScala3(using q), TypesFixturesImpl

object TypesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testNamesPrinters[A]: Data = ${ testNamesPrintersImpl[A] }
  private def testNamesPrintersImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testNamesPrinters[A]

  inline def testClassOfType[A]: Data = ${ testClassOfTypeImpl[A] }
  private def testClassOfTypeImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testClassOfType[A]

  inline def testPosition[A]: Data = ${ testPositionImpl[A] }
  private def testPositionImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testPosition[A]

  inline def testChildren[A]: Data = ${ testChildrenImpl[A] }
  private def testChildrenImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testChildren[A]

  inline def testAnnotations[A]: Data = ${ testAnnotationsImpl[A] }
  private def testAnnotationsImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testAnnotations[A]

  inline def testFlags[A]: Data = ${ testFlagsImpl[A] }
  private def testFlagsImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testFlags[A]

  inline def testComparisons[A, B]: Data = ${ testComparisonsImpl[A, B] }
  private def testComparisonsImpl[A: Type, B: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testComparisons[A, B]

  inline def testBidirectionalCodecs: Data = ${ testBidirectionalCodecsImpl }
  private def testBidirectionalCodecsImpl(using q: Quotes): Expr[Data] = new TypesFixtures(q).testBidirectionalCodecs

  inline def testOneWayCodecs: Data = ${ testOneWayCodecsImpl }
  private def testOneWayCodecsImpl(using q: Quotes): Expr[Data] = new TypesFixtures(q).testOneWayCodecs
}
