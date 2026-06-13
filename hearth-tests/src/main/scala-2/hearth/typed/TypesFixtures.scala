package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class TypesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with TypesFixturesImpl {

  // [hearth#176]

  def testNamesPrintersImpl[A: c.WeakTypeTag]: c.Expr[Data] = testNamesPrinters[A]

  def testClassOfTypeImpl[A: c.WeakTypeTag]: c.Expr[Data] = testClassOfType[A]

  def testPositionImpl[A: c.WeakTypeTag]: c.Expr[Data] = testPosition[A]

  def testChildrenImpl[A: c.WeakTypeTag]: c.Expr[Data] = testChildren[A]

  def testAnnotationsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testAnnotations[A]

  def testAnnotationTypeComparisonImpl[A: c.WeakTypeTag]: c.Expr[Data] = testAnnotationTypeComparison[A]

  def testFlagsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testFlags[A]

  def testChildrenNamesImpl[A: c.WeakTypeTag]: c.Expr[Data] = testChildrenNames[A]

  def testChildrenFlagsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testChildrenFlags[A]

  def testComparisonsImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Expr[Data] = testComparisons[A, B]

  def testTypeHierarchyImpl[A: c.WeakTypeTag, Parent: c.WeakTypeTag]: c.Expr[Data] = testTypeHierarchy[A, Parent]

  def testBidirectionalCodecsImpl: c.Expr[Data] = testBidirectionalCodecs

  def testNilAsCollectionCodecsImpl: c.Expr[Data] = testNilAsCollectionCodecs

  def testOneWayCodecsImpl: c.Expr[Data] = testOneWayCodecs

  def testUnionMembersImpl[A: c.WeakTypeTag]: c.Expr[Data] = testUnionMembers[A]

  def testOpaqueUnderlyingTypeImpl[A: c.WeakTypeTag]: c.Expr[Data] = testOpaqueUnderlyingType[A]

  def testTypeArgumentsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testTypeArguments[A]

  def testDecompose1Impl[A: c.WeakTypeTag]: c.Expr[Data] = testDecompose1[A]

  def testDecompose2Impl[A: c.WeakTypeTag]: c.Expr[Data] = testDecompose2[A]

  def testMiniFunctorDerivationImpl[A: c.WeakTypeTag]: c.Expr[Data] = testMiniFunctorDerivation[A]

  def testDecomposeSummonNameImpl[A: c.WeakTypeTag]: c.Expr[String] = testDecomposeSummonName[A]

  def testCtorK1FromDiscoveredPartsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testCtorK1FromDiscoveredParts[A]
}

object TypesFixtures {

  def testNamesPrinters[A]: Data = macro TypesFixtures.testNamesPrintersImpl[A]

  def testClassOfType[A]: Data = macro TypesFixtures.testClassOfTypeImpl[A]

  def testPosition[A]: Data = macro TypesFixtures.testPositionImpl[A]

  def testChildren[A]: Data = macro TypesFixtures.testChildrenImpl[A]

  def testAnnotations[A]: Data = macro TypesFixtures.testAnnotationsImpl[A]

  def testAnnotationTypeComparison[A]: Data = macro TypesFixtures.testAnnotationTypeComparisonImpl[A]

  def testFlags[A]: Data = macro TypesFixtures.testFlagsImpl[A]

  def testChildrenNames[A]: Data = macro TypesFixtures.testChildrenNamesImpl[A]

  def testChildrenFlags[A]: Data = macro TypesFixtures.testChildrenFlagsImpl[A]

  def testComparisons[A, B]: Data = macro TypesFixtures.testComparisonsImpl[A, B]

  def testTypeHierarchy[A, Parent]: Data = macro TypesFixtures.testTypeHierarchyImpl[A, Parent]

  def testBidirectionalCodecs: Data = macro TypesFixtures.testBidirectionalCodecsImpl

  def testNilAsCollectionCodecs: Data = macro TypesFixtures.testNilAsCollectionCodecsImpl

  def testOneWayCodecs: Data = macro TypesFixtures.testOneWayCodecsImpl

  def testUnionMembers[A]: Data = macro TypesFixtures.testUnionMembersImpl[A]

  def testOpaqueUnderlyingType[A]: Data = macro TypesFixtures.testOpaqueUnderlyingTypeImpl[A]

  def testTypeArguments[A]: Data = macro TypesFixtures.testTypeArgumentsImpl[A]

  def testDecompose1[A]: Data = macro TypesFixtures.testDecompose1Impl[A]

  def testDecompose2[A]: Data = macro TypesFixtures.testDecompose2Impl[A]

  def testMiniFunctorDerivation[A]: Data = macro TypesFixtures.testMiniFunctorDerivationImpl[A]

  def testDecomposeSummonName[A]: String = macro TypesFixtures.testDecomposeSummonNameImpl[A]

  def testCtorK1FromDiscoveredParts[A]: Data = macro TypesFixtures.testCtorK1FromDiscoveredPartsImpl[A]
}
