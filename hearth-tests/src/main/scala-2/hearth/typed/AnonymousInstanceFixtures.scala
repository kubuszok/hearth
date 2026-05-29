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
}
