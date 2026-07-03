package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class Issue316ReproFixtures(q: Quotes) extends MacroCommonsScala3(using q), Issue316ReproFixturesImpl

object Issue316ReproFixtures {

  inline def testSiblingImplicitTypes: Data = ${ testSiblingImplicitTypesImpl }
  private def testSiblingImplicitTypesImpl(using q: Quotes): Expr[Data] =
    new Issue316ReproFixtures(q).testSiblingImplicitTypes
}
