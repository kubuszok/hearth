package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ClassesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ClassesFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testClassImpl[A: c.WeakTypeTag](excluding: c.Expr[String]*): c.Expr[Data] = testClass[A](excluding)
}

object ClassesFixtures {

  def testClass[A](excluding: String*): Data = macro ClassesFixtures.testClassImpl[A]
}
