package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossExprsFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with CrossExprsFixturesImpl {

  // TODO: create macro annotation which would allow to do the following

  def testExprOfImpl[ExampleType: c.WeakTypeTag](example: c.Expr[ExampleType]): c.Expr[Data] =
    testExprOf[ExampleType](example)
}

object CrossExprsFixtures {

  def testExprOf[ExampleType](example: ExampleType): Data = macro CrossExprsFixtures.testExprOfImpl[ExampleType]
}
