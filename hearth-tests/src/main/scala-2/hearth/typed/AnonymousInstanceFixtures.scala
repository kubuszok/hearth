package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class AnonymousInstanceFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with AnonymousInstanceFixturesImpl {

  def testAnonymousInstanceParseImpl[A: c.WeakTypeTag]: c.Expr[Data] = testAnonymousInstanceParse[A]

  def testAnonymousInstanceParseWithMixinsImpl[A: c.WeakTypeTag, M1: c.WeakTypeTag]: c.Expr[Data] =
    testAnonymousInstanceParseWithMixins[A](Type.of[M1].as_??)

  def testAnonymousInstanceConstructImpl[A: c.WeakTypeTag]: c.Expr[String] = testAnonymousInstanceConstruct[A]

  def testAnonymousInstanceConstructWithMixinsImpl[A: c.WeakTypeTag, M1: c.WeakTypeTag]: c.Expr[String] =
    testAnonymousInstanceConstructWithMixins[A](Type.of[M1].as_??)

  def testAnonymousInstanceConstructWithCtorIndexImpl[A: c.WeakTypeTag](
      ctorIndex: c.Expr[Int]
  ): c.Expr[String] =
    testAnonymousInstanceConstructWithCtorIndex[A](ctorIndex)

  def testAnonymousInstanceParseInaccessibleImpl: c.Expr[Data] = testAnonymousInstanceParseInaccessible

  def testAnonymousInstanceConstructNoOverridesImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testAnonymousInstanceConstructNoOverrides[A]

  def testAnonymousInstanceConstructNoOverridesWithMixinsImpl[A: c.WeakTypeTag, M1: c.WeakTypeTag]: c.Expr[String] =
    testAnonymousInstanceConstructNoOverridesWithMixins[A](Type.of[M1].as_??)

  def testAnonymousInstanceConstructOverridingFinalImpl[A: c.WeakTypeTag](
      finalMethodName: c.Expr[String]
  ): c.Expr[String] =
    testAnonymousInstanceConstructOverridingFinal[A](finalMethodName)

  def testAnonymousInstanceCapturedOverrideImpl(captured: c.Expr[String]): c.Expr[String] =
    testAnonymousInstanceCapturedOverride(captured)

  def testAnonymousInstanceOverrideUsingParamsImpl(captured: c.Expr[String]): c.Expr[String] =
    testAnonymousInstanceOverrideUsingParams(captured)

  def testAnonymousInstanceOverrideReferencingQuoteParamImpl: c.Expr[String => String] =
    testAnonymousInstanceOverrideReferencingQuoteParam

  def testAnonymousInstanceConstructOverloadsImpl: c.Expr[String] = testAnonymousInstanceConstructOverloads

  def testAnonymousInstanceConstructGenericImpl: c.Expr[String] = testAnonymousInstanceConstructGeneric
}

object AnonymousInstanceFixtures {

  def testAnonymousInstanceParse[A]: Data = macro AnonymousInstanceFixtures.testAnonymousInstanceParseImpl[A]

  def testAnonymousInstanceParseWithMixins[A, M1]: Data =
    macro AnonymousInstanceFixtures.testAnonymousInstanceParseWithMixinsImpl[A, M1]

  def testAnonymousInstanceConstruct[A]: String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructImpl[A]

  def testAnonymousInstanceConstructWithMixins[A, M1]: String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructWithMixinsImpl[A, M1]

  def testAnonymousInstanceConstructWithCtorIndex[A](ctorIndex: Int): String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructWithCtorIndexImpl[A]

  def testAnonymousInstanceParseInaccessible: Data =
    macro AnonymousInstanceFixtures.testAnonymousInstanceParseInaccessibleImpl

  def testAnonymousInstanceConstructNoOverrides[A]: String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructNoOverridesImpl[A]

  def testAnonymousInstanceConstructNoOverridesWithMixins[A, M1]: String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructNoOverridesWithMixinsImpl[A, M1]

  def testAnonymousInstanceConstructOverridingFinal[A](finalMethodName: String): String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructOverridingFinalImpl[A]

  def testAnonymousInstanceCapturedOverride(captured: String): String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceCapturedOverrideImpl

  def testAnonymousInstanceOverrideUsingParams(captured: String): String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceOverrideUsingParamsImpl

  def testAnonymousInstanceOverrideReferencingQuoteParam: String => String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceOverrideReferencingQuoteParamImpl

  def testAnonymousInstanceConstructOverloads: String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructOverloadsImpl

  def testAnonymousInstanceConstructGeneric: String =
    macro AnonymousInstanceFixtures.testAnonymousInstanceConstructGenericImpl
}
