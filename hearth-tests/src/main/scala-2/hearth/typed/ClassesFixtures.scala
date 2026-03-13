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

  def testSingletonExprImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testSingletonExpr[A]

  def testCaseClassCaseFieldValuesAtImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testCaseClassCaseFieldValuesAt[A](expr)

  def testEnumMatchOnAndParMatchOnImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testEnumMatchOnAndParMatchOn[A](expr)

  def testDependentEnumDiagnosticImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testDependentEnumDiagnostic[A]

  def testCaseClassDefaultValuesImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testCaseClassDefaultValues[A]

  def testNamedTupleConstructAndFieldsImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testNamedTupleConstructAndFields[A]

  def testCaseClassConstructRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testCaseClassConstructRoundTrip[A]

  def testSingletonRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testSingletonRoundTrip[A]

  def testJavaBeanConstructRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testJavaBeanConstructRoundTrip[A]

  def testNamedTupleConstructRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testNamedTupleConstructRoundTrip[A]

  def testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testJavaBeanConstructWithSettersAndParConstructWithSetters[A]
}

object ClassesFixtures {

  def testClass[A](excluding: String*): Data = macro ClassesFixtures.testClassImpl[A]

  def testCaseClassConstructAndParConstruct[A]: String =
    macro ClassesFixtures.testCaseClassConstructAndParConstructImpl[A]

  def testSingletonExpr[A]: String =
    macro ClassesFixtures.testSingletonExprImpl[A]

  def testCaseClassCaseFieldValuesAt[A](expr: A): String = macro ClassesFixtures.testCaseClassCaseFieldValuesAtImpl[A]

  def testEnumMatchOnAndParMatchOn[A](expr: A): String = macro ClassesFixtures.testEnumMatchOnAndParMatchOnImpl[A]

  def testDependentEnumDiagnostic[A]: String =
    macro ClassesFixtures.testDependentEnumDiagnosticImpl[A]

  def testCaseClassDefaultValues[A]: String =
    macro ClassesFixtures.testCaseClassDefaultValuesImpl[A]

  def testNamedTupleConstructAndFields[A]: String =
    macro ClassesFixtures.testNamedTupleConstructAndFieldsImpl[A]

  def testCaseClassConstructRoundTrip[A]: String =
    macro ClassesFixtures.testCaseClassConstructRoundTripImpl[A]

  def testSingletonRoundTrip[A]: String =
    macro ClassesFixtures.testSingletonRoundTripImpl[A]

  def testJavaBeanConstructRoundTrip[A]: String =
    macro ClassesFixtures.testJavaBeanConstructRoundTripImpl[A]

  def testNamedTupleConstructRoundTrip[A]: String =
    macro ClassesFixtures.testNamedTupleConstructRoundTripImpl[A]

  def testJavaBeanConstructWithSettersAndParConstructWithSetters[A]: String =
    macro ClassesFixtures.testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A]
}
