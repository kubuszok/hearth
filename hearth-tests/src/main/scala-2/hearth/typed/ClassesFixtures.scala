package hearth
package typed

import hearth.data.Data

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ClassesFixtures(val c: blackbox.Context) extends MacroCommonsScala2 with ClassesFixturesImpl {

  def testClassImpl[A: c.WeakTypeTag](excluding: c.Expr[String]*): c.Expr[Data] = testClass[A](excluding)

  def testCaseClassConstructAndParConstructImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testCaseClassConstructAndParConstruct[A]

  def testSingletonExprImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testSingletonExpr[A]

  def testCaseClassCaseFieldValuesAtImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testCaseClassCaseFieldValuesAt[A](expr)

  def testCaseClassCaseFieldValuesAtCallSiteImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testCaseClassCaseFieldValuesAtCallSite[A](expr)

  def testCaseClassCaseFieldValuesAtEverywhereImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testCaseClassCaseFieldValuesAtEverywhere[A](expr)

  def testCaseClassCaseFieldValuesAtAnywhereImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testCaseClassCaseFieldValuesAtAnywhere[A](expr)

  def testCaseClassCopyMethodImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testCaseClassCopyMethod[A]

  def testCaseClassCopyRoundTripImpl[A: c.WeakTypeTag](instance: c.Expr[A]): c.Expr[String] =
    testCaseClassCopyRoundTrip[A](instance)

  def testEnumParMatchOnNestedQuoteImpl[A: c.WeakTypeTag](expr: c.Expr[A], suffix: c.Expr[String]): c.Expr[String] =
    testEnumParMatchOnNestedQuote[A](expr, suffix)

  def testEnumMatchOnAndParMatchOnImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] =
    testEnumMatchOnAndParMatchOn[A](expr)

  def testDependentEnumDiagnosticImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testDependentEnumDiagnostic[A]

  def testEnumParseDiagnosticImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testEnumParseDiagnostic[A]

  def testCaseClassDefaultValuesImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testCaseClassDefaultValues[A]

  def testNamedTupleConstructAndFieldsImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testNamedTupleConstructAndFields[A]

  def testCaseClassConstructRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testCaseClassConstructRoundTrip[A]

  def testVarargCaseClassConstructImpl[A: c.WeakTypeTag](expected: c.Expr[A]): c.Expr[Data] =
    testVarargCaseClassConstruct[A](expected)

  def testSingletonRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testSingletonRoundTrip[A]

  def testJavaBeanConstructRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testJavaBeanConstructRoundTrip[A]

  def testNamedTupleConstructRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testNamedTupleConstructRoundTrip[A]

  def testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testJavaBeanConstructWithSettersAndParConstructWithSetters[A]

  def testClassViewParseReasonsImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testClassViewParseReasons[A]

  def testClassViewKindImpl[A: c.WeakTypeTag]: c.Expr[String] = testClassViewKind[A]

  def testClassMethodOverloadsImpl[A: c.WeakTypeTag](name: c.Expr[String]): c.Expr[Data] =
    testClassMethodOverloads[A](name)

  def testCaseClassSummaryImpl[A: c.WeakTypeTag]: c.Expr[Data] = testCaseClassSummary[A]

  def testCaseClassRuntimeRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] = testCaseClassRuntimeRoundTrip[A]

  def testCaseClassConstructVisibilityImpl[A: c.WeakTypeTag]: c.Expr[Data] = testCaseClassConstructVisibility[A]

  def testCaseClassConstructErrorAggregationImpl[A: c.WeakTypeTag]: c.Expr[Data] =
    testCaseClassConstructErrorAggregation[A]

  def testEnumChildrenImpl[A: c.WeakTypeTag]: c.Expr[Data] = testEnumChildren[A]

  def testEnumMatchOnRuntimeImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = testEnumMatchOnRuntime[A](expr)

  def testJavaBeanSummaryImpl[A: c.WeakTypeTag]: c.Expr[Data] = testJavaBeanSummary[A]

  def testJavaBeanConstructWithoutSettersRoundTripImpl[A: c.WeakTypeTag]: c.Expr[String] =
    testJavaBeanConstructWithoutSettersRoundTrip[A]

  def testJavaBeanConstructVisibilityImpl[A: c.WeakTypeTag]: c.Expr[Data] = testJavaBeanConstructVisibility[A]

  def testJavaBeanSetterErrorAggregationImpl[A: c.WeakTypeTag]: c.Expr[Data] = testJavaBeanSetterErrorAggregation[A]
}

object ClassesFixtures {

  def testClass[A](excluding: String*): Data = macro ClassesFixtures.testClassImpl[A]

  def testCaseClassConstructAndParConstruct[A]: String =
    macro ClassesFixtures.testCaseClassConstructAndParConstructImpl[A]

  def testSingletonExpr[A]: String =
    macro ClassesFixtures.testSingletonExprImpl[A]

  def testCaseClassCaseFieldValuesAt[A](expr: A): String = macro ClassesFixtures.testCaseClassCaseFieldValuesAtImpl[A]

  def testCaseClassCaseFieldValuesAtCallSite[A](expr: A): String =
    macro ClassesFixtures.testCaseClassCaseFieldValuesAtCallSiteImpl[A]

  def testCaseClassCaseFieldValuesAtEverywhere[A](expr: A): String =
    macro ClassesFixtures.testCaseClassCaseFieldValuesAtEverywhereImpl[A]

  def testCaseClassCaseFieldValuesAtAnywhere[A](expr: A): String =
    macro ClassesFixtures.testCaseClassCaseFieldValuesAtAnywhereImpl[A]

  def testCaseClassCopyMethod[A]: Data = macro ClassesFixtures.testCaseClassCopyMethodImpl[A]

  def testCaseClassCopyRoundTrip[A](instance: A): String =
    macro ClassesFixtures.testCaseClassCopyRoundTripImpl[A]

  def testEnumParMatchOnNestedQuote[A](expr: A, suffix: String): String =
    macro ClassesFixtures.testEnumParMatchOnNestedQuoteImpl[A]

  def testEnumMatchOnAndParMatchOn[A](expr: A): String = macro ClassesFixtures.testEnumMatchOnAndParMatchOnImpl[A]

  def testDependentEnumDiagnostic[A]: String =
    macro ClassesFixtures.testDependentEnumDiagnosticImpl[A]

  def testEnumParseDiagnostic[A]: String =
    macro ClassesFixtures.testEnumParseDiagnosticImpl[A]

  def testCaseClassDefaultValues[A]: String =
    macro ClassesFixtures.testCaseClassDefaultValuesImpl[A]

  def testNamedTupleConstructAndFields[A]: String =
    macro ClassesFixtures.testNamedTupleConstructAndFieldsImpl[A]

  def testCaseClassConstructRoundTrip[A]: String =
    macro ClassesFixtures.testCaseClassConstructRoundTripImpl[A]

  def testVarargCaseClassConstruct[A](expected: A): Data =
    macro ClassesFixtures.testVarargCaseClassConstructImpl[A]

  def testSingletonRoundTrip[A]: String =
    macro ClassesFixtures.testSingletonRoundTripImpl[A]

  def testJavaBeanConstructRoundTrip[A]: String =
    macro ClassesFixtures.testJavaBeanConstructRoundTripImpl[A]

  def testNamedTupleConstructRoundTrip[A]: String =
    macro ClassesFixtures.testNamedTupleConstructRoundTripImpl[A]

  def testJavaBeanConstructWithSettersAndParConstructWithSetters[A]: String =
    macro ClassesFixtures.testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A]

  def testClassViewParseReasons[A]: Data =
    macro ClassesFixtures.testClassViewParseReasonsImpl[A]

  def testClassViewKind[A]: String = macro ClassesFixtures.testClassViewKindImpl[A]

  def testClassMethodOverloads[A](name: String): Data = macro ClassesFixtures.testClassMethodOverloadsImpl[A]

  def testCaseClassSummary[A]: Data = macro ClassesFixtures.testCaseClassSummaryImpl[A]

  def testCaseClassRuntimeRoundTrip[A]: String = macro ClassesFixtures.testCaseClassRuntimeRoundTripImpl[A]

  def testCaseClassConstructVisibility[A]: Data = macro ClassesFixtures.testCaseClassConstructVisibilityImpl[A]

  def testCaseClassConstructErrorAggregation[A]: Data =
    macro ClassesFixtures.testCaseClassConstructErrorAggregationImpl[A]

  def testEnumChildren[A]: Data = macro ClassesFixtures.testEnumChildrenImpl[A]

  def testEnumMatchOnRuntime[A](expr: A): String = macro ClassesFixtures.testEnumMatchOnRuntimeImpl[A]

  def testJavaBeanSummary[A]: Data = macro ClassesFixtures.testJavaBeanSummaryImpl[A]

  def testJavaBeanConstructWithoutSettersRoundTrip[A]: String =
    macro ClassesFixtures.testJavaBeanConstructWithoutSettersRoundTripImpl[A]

  def testJavaBeanConstructVisibility[A]: Data = macro ClassesFixtures.testJavaBeanConstructVisibilityImpl[A]

  def testJavaBeanSetterErrorAggregation[A]: Data = macro ClassesFixtures.testJavaBeanSetterErrorAggregationImpl[A]
}
