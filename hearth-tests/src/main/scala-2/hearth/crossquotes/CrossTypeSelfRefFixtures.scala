package hearth
package crossquotes

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class CrossTypeSelfRefFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CrossTypeSelfRefFixturesImpl {

  def testSelfReferentialImplicitTypeImpl: c.Expr[Data] = testSelfReferentialImplicitType

  def testSelfReferentialImplicitCtorImpl: c.Expr[Data] = testSelfReferentialImplicitCtor
}

object CrossTypeSelfRefFixtures {

  def testSelfReferentialImplicitType: Data = macro CrossTypeSelfRefFixtures.testSelfReferentialImplicitTypeImpl

  def testSelfReferentialImplicitCtor: Data = macro CrossTypeSelfRefFixtures.testSelfReferentialImplicitCtorImpl
}
