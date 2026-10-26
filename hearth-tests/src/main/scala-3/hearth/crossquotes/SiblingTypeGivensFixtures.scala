package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class SiblingTypeGivensFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      SiblingTypeGivensFixturesImpl

object SiblingTypeGivensFixtures {

  inline def testSiblingImplicitTypes: Data = ${ testSiblingImplicitTypesImpl }
  private def testSiblingImplicitTypesImpl(using q: Quotes): Expr[Data] =
    new SiblingTypeGivensFixtures(q).testSiblingImplicitTypes
}
