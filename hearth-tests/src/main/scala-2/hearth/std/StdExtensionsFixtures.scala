package hearth
package std

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class StdExtensionsFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with StdExtensions
    with StdExtensionsFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testIsCollectionImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsCollection[A](value)

  def testIsOptionImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsOption[A](value)

  def testIsEitherImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsEither[A](value)

  def testIsValueTypeImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[Data] = testIsValueType[A](value)
}

object StdExtensionsFixtures {

  def testIsCollection[A](value: A): Data = macro StdExtensionsFixtures.testIsCollectionImpl[A]

  def testIsOption[A](value: A): Data = macro StdExtensionsFixtures.testIsOptionImpl[A]

  def testIsEither[A](value: A): Data = macro StdExtensionsFixtures.testIsEitherImpl[A]

  def testIsValueType[A](value: A): Data = macro StdExtensionsFixtures.testIsValueTypeImpl[A]
}
