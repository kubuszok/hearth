package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class TypesFixtures(q: Quotes) extends MacroCommonsScala3(using q), TypesFixturesImpl {

  @scala.annotation.nowarn
  def testTupleXXLCodec: Expr[Data] = {
    def oneWay[A: TypeCodec](value: A): Data = {
      val encoded = Type(value)
      Data.map(
        "encoded" -> Data(encoded.plainPrint)
      )
    }
    // Use scala.quoted.Type.of directly to avoid the cross-quotes plugin expansion
    // which would try to summon implicit Type["a"]/Type["b"] and loop.
    given quotes: scala.quoted.Quotes = CrossQuotes.ctx
    implicit val a: Type["a"] = scala.quoted.Type.of["a"].asInstanceOf[Type["a"]]
    implicit val b: Type["b"] = scala.quoted.Type.of["b"].asInstanceOf[Type["b"]]
    Expr(
      Data.map(
        "23-element tuple" -> oneWay[
          (
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a",
              "b",
              "a"
          )
        ](
          (
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a",
            "b": "b",
            "a": "a"
          )
        )
      )
    )
  }
}

object TypesFixtures {

  // [hearth#176]

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

  inline def testAnnotationTypeComparison[A]: Data = ${ testAnnotationTypeComparisonImpl[A] }
  private def testAnnotationTypeComparisonImpl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testAnnotationTypeComparison[A]

  inline def testFlags[A]: Data = ${ testFlagsImpl[A] }
  private def testFlagsImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testFlags[A]

  inline def testChildrenNames[A]: Data = ${ testChildrenNamesImpl[A] }
  private def testChildrenNamesImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testChildrenNames[A]

  inline def testChildrenFlags[A]: Data = ${ testChildrenFlagsImpl[A] }
  private def testChildrenFlagsImpl[A: Type](using q: Quotes): Expr[Data] = new TypesFixtures(q).testChildrenFlags[A]

  inline def testComparisons[A, B]: Data = ${ testComparisonsImpl[A, B] }
  private def testComparisonsImpl[A: Type, B: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testComparisons[A, B]

  inline def testTypeHierarchy[A, Parent]: Data = ${ testTypeHierarchyImpl[A, Parent] }
  private def testTypeHierarchyImpl[A: Type, Parent: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testTypeHierarchy[A, Parent]

  inline def testBidirectionalCodecs: Data = ${ testBidirectionalCodecsImpl }
  private def testBidirectionalCodecsImpl(using q: Quotes): Expr[Data] = new TypesFixtures(q).testBidirectionalCodecs

  inline def testNilAsCollectionCodecs: Data = ${ testNilAsCollectionCodecsImpl }
  private def testNilAsCollectionCodecsImpl(using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testNilAsCollectionCodecs

  inline def testOneWayCodecs: Data = ${ testOneWayCodecsImpl }
  private def testOneWayCodecsImpl(using q: Quotes): Expr[Data] = new TypesFixtures(q).testOneWayCodecs

  inline def testUnionMembers[A]: Data = ${ testUnionMembersImpl[A] }
  private def testUnionMembersImpl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testUnionMembers[A]

  inline def testOpaqueUnderlyingType[A]: Data = ${ testOpaqueUnderlyingTypeImpl[A] }
  private def testOpaqueUnderlyingTypeImpl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testOpaqueUnderlyingType[A]

  inline def testTupleXXLCodec: Data = ${ testTupleXXLCodecImpl }
  private def testTupleXXLCodecImpl(using q: Quotes): Expr[Data] = new TypesFixtures(q).testTupleXXLCodec

  inline def testTypeArguments[A]: Data = ${ testTypeArgumentsImpl[A] }
  private def testTypeArgumentsImpl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testTypeArguments[A]

  inline def testDecompose1[A]: Data = ${ testDecompose1Impl[A] }
  private def testDecompose1Impl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testDecompose1[A]

  inline def testDecompose2[A]: Data = ${ testDecompose2Impl[A] }
  private def testDecompose2Impl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testDecompose2[A]

  inline def testMiniFunctorDerivation[A]: Data = ${ testMiniFunctorDerivationImpl[A] }
  private def testMiniFunctorDerivationImpl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testMiniFunctorDerivation[A]

  inline def testDecomposeSummonName[A]: String = ${ testDecomposeSummonNameImpl[A] }
  private def testDecomposeSummonNameImpl[A: Type](using q: Quotes): Expr[String] =
    new TypesFixtures(q).testDecomposeSummonName[A]

  inline def testCtorK1FromDiscoveredParts[A]: Data = ${ testCtorK1FromDiscoveredPartsImpl[A] }
  private def testCtorK1FromDiscoveredPartsImpl[A: Type](using q: Quotes): Expr[Data] =
    new TypesFixtures(q).testCtorK1FromDiscoveredParts[A]
}
