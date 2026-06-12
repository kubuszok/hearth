package hearth
package crossquotes

import hearth.data.Data

import scala.quoted.*

final private class CrossTypeSelfRefFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      CrossTypeSelfRefFixturesImpl

object CrossTypeSelfRefFixtures {

  inline def testSelfReferentialImplicitType: Data = ${ testSelfReferentialImplicitTypeImpl }
  private def testSelfReferentialImplicitTypeImpl(using q: Quotes): Expr[Data] =
    new CrossTypeSelfRefFixtures(q).testSelfReferentialImplicitType

  inline def testSelfReferentialImplicitCtor: Data = ${ testSelfReferentialImplicitCtorImpl }
  private def testSelfReferentialImplicitCtorImpl(using q: Quotes): Expr[Data] =
    new CrossTypeSelfRefFixtures(q).testSelfReferentialImplicitCtor
}
