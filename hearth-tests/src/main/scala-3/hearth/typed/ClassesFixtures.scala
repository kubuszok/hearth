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

  inline def testCaseClassConstructAndParConstruct[A]: String = ${ testCaseClassConstructAndParConstructImpl[A] }
  private def testCaseClassConstructAndParConstructImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassConstructAndParConstruct[A]

  inline def testCaseClassCaseFieldValuesAt[A](expr: A): String = ${ testCaseClassCaseFieldValuesAtImpl[A]('{ expr }) }
  private def testCaseClassCaseFieldValuesAtImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassCaseFieldValuesAt[A](expr)

  inline def testEnumMatchOnAndParMatchOn[A](expr: A): String = ${ testEnumMatchOnAndParMatchOnImpl[A]('{ expr }) }
  private def testEnumMatchOnAndParMatchOnImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testEnumMatchOnAndParMatchOn[A](expr)

  inline def testJavaBeanConstructWithSettersAndParConstructWithSetters[A]: String = ${
    testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A]
  }
  private def testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testJavaBeanConstructWithSettersAndParConstructWithSetters[A]
}
