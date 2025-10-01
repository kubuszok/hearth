package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ClassesFixtures(q: Quotes) extends MacroCommonsScala3(using q), ClassesFixturesImpl

object ClassesFixtures {

  // TODO: create macro annotation which would allow to do the following

  inline def testClass[A](inline excluding: String*): Data = ${ testClassImpl[A]('{ excluding }) }
  private def testClassImpl[A: Type](excluding: Expr[Seq[String]])(using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testClass[A](excluding)
}
