package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ClassesFixtures(q: Quotes) extends MacroCommonsScala3(using q), ClassesFixturesImpl

object ClassesFixtures {

  inline def testClass[A](inline excluding: String*): Data = ${ testClassImpl[A]('excluding) }
  private def testClassImpl[A: Type](excluding: Expr[Seq[String]])(using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testClass[A](excluding)

  inline def testCaseClassConstructAndParConstruct[A]: String = ${ testCaseClassConstructAndParConstructImpl[A] }
  private def testCaseClassConstructAndParConstructImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassConstructAndParConstruct[A]

  inline def testSingletonExpr[A]: String = ${ testSingletonExprImpl[A] }
  private def testSingletonExprImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testSingletonExpr[A]

  inline def testCaseClassCaseFieldValuesAt[A](inline expr: A): String = ${
    testCaseClassCaseFieldValuesAtImpl[A]('expr)
  }
  private def testCaseClassCaseFieldValuesAtImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassCaseFieldValuesAt[A](expr)

  inline def testCaseClassCaseFieldValuesAtCallSite[A](inline expr: A): String = ${
    testCaseClassCaseFieldValuesAtCallSiteImpl[A]('expr)
  }
  private def testCaseClassCaseFieldValuesAtCallSiteImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassCaseFieldValuesAtCallSite[A](expr)

  inline def testCaseClassCaseFieldValuesAtEverywhere[A](inline expr: A): String = ${
    testCaseClassCaseFieldValuesAtEverywhereImpl[A]('expr)
  }
  private def testCaseClassCaseFieldValuesAtEverywhereImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassCaseFieldValuesAtEverywhere[A](expr)

  inline def testCaseClassCaseFieldValuesAtAnywhere[A](inline expr: A): String = ${
    testCaseClassCaseFieldValuesAtAnywhereImpl[A]('expr)
  }
  private def testCaseClassCaseFieldValuesAtAnywhereImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassCaseFieldValuesAtAnywhere[A](expr)

  inline def testCaseClassCopyMethod[A]: Data = ${ testCaseClassCopyMethodImpl[A] }
  private def testCaseClassCopyMethodImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testCaseClassCopyMethod[A]

  inline def testCaseClassCopyRoundTrip[A](inline instance: A): String = ${
    testCaseClassCopyRoundTripImpl[A]('instance)
  }
  private def testCaseClassCopyRoundTripImpl[A: Type](instance: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassCopyRoundTrip[A](instance)

  inline def testEnumParMatchOnNestedQuote[A](inline expr: A, inline suffix: String): String = ${
    testEnumParMatchOnNestedQuoteImpl[A]('expr, 'suffix)
  }
  private def testEnumParMatchOnNestedQuoteImpl[A: Type](expr: Expr[A], suffix: Expr[String])(using
      q: Quotes
  ): Expr[String] =
    new ClassesFixtures(q).testEnumParMatchOnNestedQuote[A](expr, suffix)

  inline def testEnumMatchOnAndParMatchOn[A](inline expr: A): String = ${
    testEnumMatchOnAndParMatchOnImpl[A]('expr)
  }
  private def testEnumMatchOnAndParMatchOnImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testEnumMatchOnAndParMatchOn[A](expr)

  inline def testEnumParseDiagnostic[A]: String = ${ testEnumParseDiagnosticImpl[A] }
  private def testEnumParseDiagnosticImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testEnumParseDiagnostic[A]

  inline def testDependentEnumDiagnostic[A]: String = ${ testDependentEnumDiagnosticImpl[A] }
  private def testDependentEnumDiagnosticImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testDependentEnumDiagnostic[A]

  inline def testDependentEnumTypeReprDiagnostic[A]: String = ${ testDependentEnumTypeReprDiagnosticImpl[A] }
  private def testDependentEnumTypeReprDiagnosticImpl[A: Type](using q: Quotes): Expr[String] = {
    import q.reflect.*

    val parentRepr = TypeRepr.of[A]
    val parentSym = parentRepr.typeSymbol

    if !parentSym.flags.is(Flags.Sealed) then Expr("<not sealed>")
    else {
      val children = parentSym.children
      val lines = children.map { childSym =>
        val childRepr = childSym.typeRef
        val isEnum = childSym.flags.is(Flags.Enum)
        val isStable = childSym.flags.is(Flags.StableRealizable)
        val flagsStr = childSym.flags.show

        val termRefStr = scala.util.Try(childSym.termRef.show).getOrElse("<no termRef>")

        val singletonRepr =
          if isEnum && (isStable || childSym.flags
            .is(Flags.JavaStatic)) then scala.util.Try(childSym.typeRef.show).getOrElse("<error>")
          else "<not a val>"

        val childType = childRepr.asType
        val ofRepr = childType match {
          case '[t] => TypeRepr.of[t].show
        }

        val widenedStr = scala.util.Try(childRepr.widen.show).getOrElse("<error>")

        val approach1 = scala.util
          .Try {
            if isEnum && isStable then Ident(childSym.termRef).show
            else "<n/a>"
          }
          .getOrElse("<error>")

        val approach2 = scala.util
          .Try {
            if isEnum && isStable then {
              val parentModule = parentSym.companionModule
              if !parentModule.isNoSymbol then Select(Ref(parentModule), childSym).show
              else "<no parent module>"
            } else "<n/a>"
          }
          .getOrElse("<error>")

        val approach3 = scala.util
          .Try {
            if isEnum && isStable then Ref(childSym).show
            else "<n/a>"
          }
          .getOrElse("<error>")

        val mirrorInfo = scala.util
          .Try {
            childType match {
              case '[t] =>
                Expr.summon[scala.deriving.Mirror.Of[t]] match {
                  case Some(mirror) => s"mirror=${mirror.asTerm.show}"
                  case None         => "<no mirror>"
                }
            }
          }
          .getOrElse("<mirror error>")

        s"""child=${childSym.name}:
           |  flags=$flagsStr
           |  typeRef=${childRepr.show}
           |  termRef=$termRefStr
           |  TypeRepr.of=$ofRepr
           |  widen=$widenedStr
           |  singletonRepr=$singletonRepr
           |  approach1(Ident(sym.termRef))=$approach1
           |  approach2(Select(parentModule,sym))=$approach2
           |  approach3(Ref(sym))=$approach3
           |  $mirrorInfo""".stripMargin
      }
      Expr(lines.mkString("\n---\n"))
    }
  }

  inline def testCaseClassDefaultValues[A]: String = ${ testCaseClassDefaultValuesImpl[A] }
  private def testCaseClassDefaultValuesImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassDefaultValues[A]

  inline def testNamedTupleConstructAndFields[A]: String = ${ testNamedTupleConstructAndFieldsImpl[A] }
  private def testNamedTupleConstructAndFieldsImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testNamedTupleConstructAndFields[A]

  inline def testCaseClassConstructRoundTrip[A]: String = ${ testCaseClassConstructRoundTripImpl[A] }
  private def testCaseClassConstructRoundTripImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassConstructRoundTrip[A]

  inline def testVarargCaseClassConstruct[A](inline expected: A): Data = ${
    testVarargCaseClassConstructImpl[A]('expected)
  }
  private def testVarargCaseClassConstructImpl[A: Type](expected: Expr[A])(using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testVarargCaseClassConstruct[A](expected)

  inline def testSingletonRoundTrip[A]: String = ${ testSingletonRoundTripImpl[A] }
  private def testSingletonRoundTripImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testSingletonRoundTrip[A]

  inline def testJavaBeanConstructRoundTrip[A]: String = ${ testJavaBeanConstructRoundTripImpl[A] }
  private def testJavaBeanConstructRoundTripImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testJavaBeanConstructRoundTrip[A]

  inline def testNamedTupleConstructRoundTrip[A]: String = ${ testNamedTupleConstructRoundTripImpl[A] }
  private def testNamedTupleConstructRoundTripImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testNamedTupleConstructRoundTrip[A]

  inline def testJavaBeanConstructWithSettersAndParConstructWithSetters[A]: String = ${
    testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A]
  }
  private def testJavaBeanConstructWithSettersAndParConstructWithSettersImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testJavaBeanConstructWithSettersAndParConstructWithSetters[A]

  inline def testClassViewParseReasons[A]: Data = ${ testClassViewParseReasonsImpl[A] }
  private def testClassViewParseReasonsImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testClassViewParseReasons[A]

  inline def testClassViewKind[A]: String = ${ testClassViewKindImpl[A] }
  private def testClassViewKindImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testClassViewKind[A]

  inline def testCaseClassSummary[A]: Data = ${ testCaseClassSummaryImpl[A] }
  private def testCaseClassSummaryImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testCaseClassSummary[A]

  inline def testCaseClassRuntimeRoundTrip[A]: String = ${ testCaseClassRuntimeRoundTripImpl[A] }
  private def testCaseClassRuntimeRoundTripImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testCaseClassRuntimeRoundTrip[A]

  inline def testCaseClassConstructVisibility[A]: Data = ${ testCaseClassConstructVisibilityImpl[A] }
  private def testCaseClassConstructVisibilityImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testCaseClassConstructVisibility[A]

  inline def testCaseClassConstructErrorAggregation[A]: Data = ${ testCaseClassConstructErrorAggregationImpl[A] }
  private def testCaseClassConstructErrorAggregationImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testCaseClassConstructErrorAggregation[A]

  inline def testEnumChildren[A]: Data = ${ testEnumChildrenImpl[A] }
  private def testEnumChildrenImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testEnumChildren[A]

  inline def testJavaBeanSummary[A]: Data = ${ testJavaBeanSummaryImpl[A] }
  private def testJavaBeanSummaryImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testJavaBeanSummary[A]

  inline def testJavaBeanConstructWithoutSettersRoundTrip[A]: String = ${
    testJavaBeanConstructWithoutSettersRoundTripImpl[A]
  }
  private def testJavaBeanConstructWithoutSettersRoundTripImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testJavaBeanConstructWithoutSettersRoundTrip[A]

  inline def testJavaBeanConstructVisibility[A]: Data = ${ testJavaBeanConstructVisibilityImpl[A] }
  private def testJavaBeanConstructVisibilityImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testJavaBeanConstructVisibility[A]

  inline def testJavaBeanSetterErrorAggregation[A]: Data = ${ testJavaBeanSetterErrorAggregationImpl[A] }
  private def testJavaBeanSetterErrorAggregationImpl[A: Type](using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testJavaBeanSetterErrorAggregation[A]

  inline def testClassMethodOverloads[A](inline name: String): Data = ${ testClassMethodOverloadsImpl[A]('name) }
  private def testClassMethodOverloadsImpl[A: Type](name: Expr[String])(using q: Quotes): Expr[Data] =
    new ClassesFixtures(q).testClassMethodOverloads[A](name)

  inline def testEnumMatchOnRuntime[A](inline expr: A): String = ${ testEnumMatchOnRuntimeImpl[A]('expr) }
  private def testEnumMatchOnRuntimeImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testEnumMatchOnRuntime[A](expr)
}
