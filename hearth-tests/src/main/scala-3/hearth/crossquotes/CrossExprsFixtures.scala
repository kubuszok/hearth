package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class CrossExprsFixtures(q: Quotes) extends MacroCommonsScala3(using q), CrossExprsFixturesImpl

object CrossExprsFixtures {

  // [hearth#176]

  inline def testExprOf[ExampleType](inline example: ExampleType): Data = ${ testExprOfImpl[ExampleType]('example) }
  private def testExprOfImpl[ExampleType: Type](example: Expr[ExampleType])(using q: Quotes): Expr[Data] =
    new CrossExprsFixtures(q).testExprOf[ExampleType](example)
}
