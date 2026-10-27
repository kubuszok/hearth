package hearth
package std

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class StdExtensionsFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with StdExtensions
    with StdExtensionsFixturesImpl {

  def testIsCollectionImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsCollection[A](value)

  def testIsOptionImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsOption[A](value)

  def testIsEitherImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsEither[A](value)

  def testIsValueTypeImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsValueType[A](value)

  def testIsCollectionForeachImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] =
    testIsCollectionForeach[A](value)

  def testCtorLikesImpl[A: c.WeakTypeTag]: c.Expr[Data] = testCtorLikes[A]

  def testParseImpl[A: c.WeakTypeTag]: c.Expr[Data] = testParse[A]

  def testLastUnapplyFailureImpl[A: c.WeakTypeTag]: c.Expr[Data] = testLastUnapplyFailure[A]

  def testIsValueTypeRewrapTypeImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] =
    testIsValueTypeRewrapType[A](value)

  def testCollectionTypesImpl[A: c.WeakTypeTag]: c.Expr[Data] = testCollectionTypes[A]

  def testBuildAndIterateImpl[A: c.WeakTypeTag]: c.Expr[Data] = testBuildAndIterate[A]

  def testSizeHintImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testSizeHint[A](value)

  def testIsOptionOrElseImpl[A: c.WeakTypeTag](value: c.Expr[A], default: c.Expr[A]): c.Expr[Data] =
    testIsOptionOrElse[A](value, default)

  def testIsEitherOrElseImpl[A: c.WeakTypeTag](value: c.Expr[A], default: c.Expr[A]): c.Expr[Data] =
    testIsEitherOrElse[A](value, default)

  def testIsEitherTypesImpl[A: c.WeakTypeTag]: c.Expr[Data] = testIsEitherTypes[A]

  def testIsOptionTypesImpl[A: c.WeakTypeTag]: c.Expr[Data] = testIsOptionTypes[A]

  def testValueTypeInfoImpl[A: c.WeakTypeTag]: c.Expr[Data] = testValueTypeInfo[A]

  def testProvenanceImpl[A: c.WeakTypeTag]: c.Expr[Data] = testProvenance[A]

  def testIsMapProvenanceImpl[A: c.WeakTypeTag]: c.Expr[Data] = testIsMapProvenance[A]

  def testSkipReasonsImpl[A: c.WeakTypeTag]: c.Expr[Data] = testSkipReasons[A]

  def testUnapplyFailuresImpl[A: c.WeakTypeTag]: c.Expr[Data] = testUnapplyFailures[A]
}

object StdExtensionsFixtures {

  def testIsCollection[A](value: A): Data = macro StdExtensionsFixtures.testIsCollectionImpl[A]

  def testIsOption[A](value: A): Data = macro StdExtensionsFixtures.testIsOptionImpl[A]

  def testIsEither[A](value: A): Data = macro StdExtensionsFixtures.testIsEitherImpl[A]

  def testIsValueType[A](value: A): Data = macro StdExtensionsFixtures.testIsValueTypeImpl[A]

  def testIsCollectionForeach[A](value: A): Data = macro StdExtensionsFixtures.testIsCollectionForeachImpl[A]

  def testCtorLikes[A]: Data = macro StdExtensionsFixtures.testCtorLikesImpl[A]

  def testParse[A]: Data = macro StdExtensionsFixtures.testParseImpl[A]

  def testLastUnapplyFailure[A]: Data = macro StdExtensionsFixtures.testLastUnapplyFailureImpl[A]

  def testIsValueTypeRewrapType[A](value: A): Data = macro StdExtensionsFixtures.testIsValueTypeRewrapTypeImpl[A]

  def testCollectionTypes[A]: Data = macro StdExtensionsFixtures.testCollectionTypesImpl[A]

  def testBuildAndIterate[A]: Data = macro StdExtensionsFixtures.testBuildAndIterateImpl[A]

  def testSizeHint[A](value: A): Data = macro StdExtensionsFixtures.testSizeHintImpl[A]

  def testIsOptionOrElse[A](value: A, default: A): Data = macro StdExtensionsFixtures.testIsOptionOrElseImpl[A]

  def testIsEitherOrElse[A](value: A, default: A): Data = macro StdExtensionsFixtures.testIsEitherOrElseImpl[A]

  def testIsEitherTypes[A]: Data = macro StdExtensionsFixtures.testIsEitherTypesImpl[A]

  def testIsOptionTypes[A]: Data = macro StdExtensionsFixtures.testIsOptionTypesImpl[A]

  def testValueTypeInfo[A]: Data = macro StdExtensionsFixtures.testValueTypeInfoImpl[A]

  def testProvenance[A]: Data = macro StdExtensionsFixtures.testProvenanceImpl[A]

  def testIsMapProvenance[A]: Data = macro StdExtensionsFixtures.testIsMapProvenanceImpl[A]

  def testSkipReasons[A]: Data = macro StdExtensionsFixtures.testSkipReasonsImpl[A]

  def testUnapplyFailures[A]: Data = macro StdExtensionsFixtures.testUnapplyFailuresImpl[A]
}
