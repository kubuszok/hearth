package hearth
package typed

import hearth.testdata.{Data, DataSupports}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final class MethodsFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with DataSupports
    with MethodsFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testMethodsExtractionImpl[A: c.WeakTypeTag]: c.Expr[Data] = testMethodsExtraction[A]
}

object MethodsFixtures {

  def testMethodsExtraction[A]: Data = macro MethodsFixtures.testMethodsExtractionImpl[A]
}
