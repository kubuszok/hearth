package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ClassesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ClassesFixturesImpl {

  // [hearth#176]

  def testClassImpl[A: c.WeakTypeTag](excluding: c.Expr[String]*): c.Expr[Data] = testClass[A](excluding)

  def testCaseClassConstructAndParConstructImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testCaseClassConstructAndParConstruct[A]

  def testCaseClassCaseFieldValuesAtImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testCaseClassCaseFieldValuesAt[A](expr)

  def testEnumMatchOnAndParMatchOnImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testEnumMatchOnAndParMatchOn[A](expr)

  def testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testJavaBeanConstructWithSettersAndParConstructWithSetters[A]
}

object ClassesFixtures {

  def testClass[A](excluding: String*): Data = macro ClassesFixtures.testClassImpl[A]

  def testCaseClassConstructAndParConstruct[A]: String =
    macro ClassesFixtures.testCaseClassConstructAndParConstructImpl[A]

  def testCaseClassCaseFieldValuesAt[A](expr: A): String = macro ClassesFixtures.testCaseClassCaseFieldValuesAtImpl[A]

  def testEnumMatchOnAndParMatchOn[A](expr: A): String = macro ClassesFixtures.testEnumMatchOnAndParMatchOnImpl[A]

  def testJavaBeanConstructWithSettersAndParConstructWithSetters[A]: String =
    macro ClassesFixtures.testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A]
}
