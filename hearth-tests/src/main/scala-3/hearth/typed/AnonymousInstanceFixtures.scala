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
}
