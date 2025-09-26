package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class TypesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with TypesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testNamesPrintersImpl[A: c.WeakTypeTag]: c.Expr[Data] = testNamesPrinters[A]

  def testClassOfTypeImpl[A: c.WeakTypeTag]: c.Expr[Data] = testClassOfType[A]

  def testPositionImpl[A: c.WeakTypeTag]: c.Expr[Data] = testPosition[A]

  def testChildrenImpl[A: c.WeakTypeTag]: c.Expr[Data] = testChildren[A]

  def testAnnotationsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testAnnotations[A]

  def testFlagsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testFlags[A]

  def testChildrenFlagsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testChildrenFlags[A]

  def testComparisonsImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Expr[Data] = testComparisons[A, B]

  def testBidirectionalCodecsImpl: c.Expr[Data] = testBidirectionalCodecs

  def testOneWayCodecsImpl: c.Expr[Data] = testOneWayCodecs
}

object TypesFixtures {

  def testNamesPrinters[A]: Data = macro TypesFixtures.testNamesPrintersImpl[A]

  def testClassOfType[A]: Data = macro TypesFixtures.testClassOfTypeImpl[A]

  def testPosition[A]: Data = macro TypesFixtures.testPositionImpl[A]

  def testChildren[A]: Data = macro TypesFixtures.testChildrenImpl[A]

  def testAnnotations[A]: Data = macro TypesFixtures.testAnnotationsImpl[A]

  def testFlags[A]: Data = macro TypesFixtures.testFlagsImpl[A]

  def testChildrenFlags[A]: Data = macro TypesFixtures.testChildrenFlagsImpl[A]

  def testComparisons[A, B]: Data = macro TypesFixtures.testComparisonsImpl[A, B]

  def testBidirectionalCodecs: Data = macro TypesFixtures.testBidirectionalCodecsImpl

  def testOneWayCodecs: Data = macro TypesFixtures.testOneWayCodecsImpl
}
