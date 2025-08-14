package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class MethodsFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with MethodsFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testMethodsExtractionImpl[A: c.WeakTypeTag](excluding: c.Expr[String]*): c.Expr[Data] =
    testMethodsExtractionS2Adapter[A](excluding)
}

object MethodsFixtures {

  def testMethodsExtraction[A](excluding: String*): Data = macro MethodsFixtures.testMethodsExtractionImpl[A]
}
