package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class AnonymousInstanceFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      AnonymousInstanceFixturesImpl

object AnonymousInstanceFixtures {

  inline def testAnonymousInstanceParse[A]: Data = ${ testAnonymousInstanceParseImpl[A] }
  private def testAnonymousInstanceParseImpl[A: Type](using q: Quotes): Expr[Data] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceParse[A]

  inline def testAnonymousInstanceParseWithMixins[A, M1]: Data = ${
    testAnonymousInstanceParseWithMixinsImpl[A, M1]
  }
  private def testAnonymousInstanceParseWithMixinsImpl[A: Type, M1: Type](using q: Quotes): Expr[Data] = {
    val mc = new AnonymousInstanceFixtures(q)
    mc.testAnonymousInstanceParseWithMixins[A](mc.UntypedType.fromTyped[M1].as_??)
  }

  inline def testAnonymousInstanceConstruct[A]: String = ${ testAnonymousInstanceConstructImpl[A] }
  private def testAnonymousInstanceConstructImpl[A: Type](using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstruct[A]

  inline def testAnonymousInstanceConstructWithMixins[A, M1]: String = ${
    testAnonymousInstanceConstructWithMixinsImpl[A, M1]
  }
  private def testAnonymousInstanceConstructWithMixinsImpl[A: Type, M1: Type](using q: Quotes): Expr[String] = {
    val mc = new AnonymousInstanceFixtures(q)
    mc.testAnonymousInstanceConstructWithMixins[A](mc.UntypedType.fromTyped[M1].as_??)
  }

  inline def testAnonymousInstanceConstructWithCtorIndex[A](inline ctorIndex: Int): String = ${
    testAnonymousInstanceConstructWithCtorIndexImpl[A]('ctorIndex)
  }
  private def testAnonymousInstanceConstructWithCtorIndexImpl[A: Type](
      ctorIndex: Expr[Int]
  )(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructWithCtorIndex[A](ctorIndex)

  inline def testAnonymousInstanceParseInaccessible: Data = ${ testAnonymousInstanceParseInaccessibleImpl }
  private def testAnonymousInstanceParseInaccessibleImpl(using q: Quotes): Expr[Data] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceParseInaccessible

  inline def testAnonymousInstanceConstructNoOverrides[A]: String = ${
    testAnonymousInstanceConstructNoOverridesImpl[A]
  }
  private def testAnonymousInstanceConstructNoOverridesImpl[A: Type](using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructNoOverrides[A]

  inline def testAnonymousInstanceConstructNoOverridesWithMixins[A, M1]: String = ${
    testAnonymousInstanceConstructNoOverridesWithMixinsImpl[A, M1]
  }
  private def testAnonymousInstanceConstructNoOverridesWithMixinsImpl[A: Type, M1: Type](using
      q: Quotes
  ): Expr[String] = {
    val mc = new AnonymousInstanceFixtures(q)
    mc.testAnonymousInstanceConstructNoOverridesWithMixins[A](mc.UntypedType.fromTyped[M1].as_??)
  }

  inline def testAnonymousInstanceConstructOverridingFinal[A](inline finalMethodName: String): String = ${
    testAnonymousInstanceConstructOverridingFinalImpl[A]('finalMethodName)
  }
  private def testAnonymousInstanceConstructOverridingFinalImpl[A: Type](
      finalMethodName: Expr[String]
  )(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructOverridingFinal[A](finalMethodName)

  inline def testAnonymousInstanceCapturedOverride(inline captured: String): String = ${
    testAnonymousInstanceCapturedOverrideImpl('captured)
  }
  private def testAnonymousInstanceCapturedOverrideImpl(captured: Expr[String])(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceCapturedOverride(captured)

  inline def testAnonymousInstanceOverrideUsingParams(inline captured: String): String = ${
    testAnonymousInstanceOverrideUsingParamsImpl('captured)
  }
  private def testAnonymousInstanceOverrideUsingParamsImpl(captured: Expr[String])(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceOverrideUsingParams(captured)

  inline def testAnonymousInstanceOverrideReferencingQuoteParam: String => String = ${
    testAnonymousInstanceOverrideReferencingQuoteParamImpl
  }
  private def testAnonymousInstanceOverrideReferencingQuoteParamImpl(using q: Quotes): Expr[String => String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceOverrideReferencingQuoteParam

  inline def testAnonymousInstanceValDefBodyInSplice: String = ${ testAnonymousInstanceValDefBodyInSpliceImpl }
  private def testAnonymousInstanceValDefBodyInSpliceImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceValDefBodyInSplice

  inline def testAnonymousInstanceTwoValDefInstancesInSplice: String = ${
    testAnonymousInstanceTwoValDefInstancesInSpliceImpl
  }
  private def testAnonymousInstanceTwoValDefInstancesInSpliceImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceTwoValDefInstancesInSplice

  inline def testAnonymousInstanceConstructOverloads: String = ${ testAnonymousInstanceConstructOverloadsImpl }
  private def testAnonymousInstanceConstructOverloadsImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructOverloads

  inline def testAnonymousInstanceConstructGeneric: String = ${ testAnonymousInstanceConstructGenericImpl }
  private def testAnonymousInstanceConstructGenericImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructGeneric

  inline def testAnonymousInstanceConstructGenericConcreteReturn: String = ${
    testAnonymousInstanceConstructGenericConcreteReturnImpl
  }
  private def testAnonymousInstanceConstructGenericConcreteReturnImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructGenericConcreteReturn

  inline def testAnonymousInstanceConstructGenericFactory: String = ${
    testAnonymousInstanceConstructGenericFactoryImpl
  }
  private def testAnonymousInstanceConstructGenericFactoryImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructGenericFactory

  inline def testAnonymousInstanceConstructSymbolic: String = ${ testAnonymousInstanceConstructSymbolicImpl }
  private def testAnonymousInstanceConstructSymbolicImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructSymbolic

  inline def testAnonymousInstanceConstructImplicitParam: String = ${ testAnonymousInstanceConstructImplicitParamImpl }
  private def testAnonymousInstanceConstructImplicitParamImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructImplicitParam

  inline def testAnonymousInstanceConstructAbstractVal: String = ${ testAnonymousInstanceConstructAbstractValImpl }
  private def testAnonymousInstanceConstructAbstractValImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructAbstractVal

  inline def testAnonymousInstanceConstructThisType: String = ${ testAnonymousInstanceConstructThisTypeImpl }
  private def testAnonymousInstanceConstructThisTypeImpl(using q: Quotes): Expr[String] =
    new AnonymousInstanceFixtures(q).testAnonymousInstanceConstructThisType
}
