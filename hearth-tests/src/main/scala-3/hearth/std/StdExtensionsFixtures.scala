package hearth
package std

import hearth.data.Data

import scala.quoted.*

final private class StdExtensionsFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      StdExtensions,
      StdExtensionsFixturesImpl

object StdExtensionsFixtures {

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

  inline def testIsValueTypeRewrapType[A](inline value: A): Data = ${ testIsValueTypeRewrapTypeImpl[A]('value) }
  private def testIsValueTypeRewrapTypeImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsValueTypeRewrapType[A](value)

  inline def testIsCollectionForeach[A](inline value: A): Data = ${ testIsCollectionForeachImpl[A]('value) }
  private def testIsCollectionForeachImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsCollectionForeach[A](value)

  inline def testCtorLikes[A]: Data = ${ testCtorLikesImpl[A] }
  private def testCtorLikesImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testCtorLikes[A]

  inline def testParse[A]: Data = ${ testParseImpl[A] }
  private def testParseImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testParse[A]

  inline def testLastUnapplyFailure[A]: Data = ${ testLastUnapplyFailureImpl[A] }
  private def testLastUnapplyFailureImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testLastUnapplyFailure[A]

  inline def testCollectionTypes[A]: Data = ${ testCollectionTypesImpl[A] }
  private def testCollectionTypesImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testCollectionTypes[A]

  inline def testBuildAndIterate[A]: Data = ${ testBuildAndIterateImpl[A] }
  private def testBuildAndIterateImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testBuildAndIterate[A]

  inline def testSizeHint[A](inline value: A): Data = ${ testSizeHintImpl[A]('value) }
  private def testSizeHintImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testSizeHint[A](value)

  inline def testIsOptionOrElse[A](inline value: A, inline default: A): Data =
    ${ testIsOptionOrElseImpl[A]('value, 'default) }
  private def testIsOptionOrElseImpl[A: Type](value: Expr[A], default: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsOptionOrElse[A](value, default)

  inline def testIsEitherOrElse[A](inline value: A, inline default: A): Data =
    ${ testIsEitherOrElseImpl[A]('value, 'default) }
  private def testIsEitherOrElseImpl[A: Type](value: Expr[A], default: Expr[A])(using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsEitherOrElse[A](value, default)

  inline def testIsEitherTypes[A]: Data = ${ testIsEitherTypesImpl[A] }
  private def testIsEitherTypesImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsEitherTypes[A]

  inline def testIsOptionTypes[A]: Data = ${ testIsOptionTypesImpl[A] }
  private def testIsOptionTypesImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsOptionTypes[A]

  inline def testValueTypeInfo[A]: Data = ${ testValueTypeInfoImpl[A] }
  private def testValueTypeInfoImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testValueTypeInfo[A]

  inline def testProvenance[A]: Data = ${ testProvenanceImpl[A] }
  private def testProvenanceImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testProvenance[A]

  inline def testIsMapProvenance[A]: Data = ${ testIsMapProvenanceImpl[A] }
  private def testIsMapProvenanceImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testIsMapProvenance[A]

  inline def testSkipReasons[A]: Data = ${ testSkipReasonsImpl[A] }
  private def testSkipReasonsImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testSkipReasons[A]

  inline def testUnapplyFailures[A]: Data = ${ testUnapplyFailuresImpl[A] }
  private def testUnapplyFailuresImpl[A: Type](using q: Quotes): Expr[Data] =
    new StdExtensionsFixtures(q).testUnapplyFailures[A]
}
