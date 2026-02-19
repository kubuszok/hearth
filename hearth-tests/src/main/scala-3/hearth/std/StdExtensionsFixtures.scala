package hearth
package std

import hearth.data.Data

import scala.quoted.*

final private class StdExtensionsFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      StdExtensions,
      StdExtensionsFixturesImpl

object StdExtensionsFixtures {

  // [hearth#176]

  inline def testIsCollection[A](inline value: A): Data = ${ testIsCollectionImpl[A]('value) }
  private def testIsCollectionImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsCollection[A](value)

  inline def testIsOption[A](inline value: A): Data = ${ testIsOptionImpl[A]('value) }
  private def testIsOptionImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsOption[A](value)

  inline def testIsEither[A](inline value: A): Data = ${ testIsEitherImpl[A]('value) }
  private def testIsEitherImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsEither[A](value)

  inline def testIsValueType[A](inline value: A): Data = ${ testIsValueTypeImpl[A]('value) }
  private def testIsValueTypeImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsValueType[A](value)

  inline def testCtorLikes[A]: Data = ${ testCtorLikesImpl[A] }
  private def testCtorLikesImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testCtorLikes[A]

  inline def testParse[A]: Data = ${ testParseImpl[A] }
  private def testParseImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testParse[A]

  inline def testLastUnapplyFailure[A]: Data = ${ testLastUnapplyFailureImpl[A] }
  private def testLastUnapplyFailureImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testLastUnapplyFailure[A]
}
