package hearth
package typed

import hearth.data.Data

import scala.quoted.*

final private class ClassesFixtures(q: Quotes) extends MacroCommonsScala3(using q), ClassesFixturesImpl

object ClassesFixtures {

  // [hearth#176]

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

  inline def testEnumMatchOnAndParMatchOn[A](inline expr: A): String = ${
    testEnumMatchOnAndParMatchOnImpl[A]('expr)
  }
  private def testEnumMatchOnAndParMatchOnImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testEnumMatchOnAndParMatchOn[A](expr)

  inline def testDependentEnumDiagnostic[A]: String = ${ testDependentEnumDiagnosticImpl[A] }
  private def testDependentEnumDiagnosticImpl[A: Type](using q: Quotes): Expr[String] =
    new ClassesFixtures(q).testDependentEnumDiagnostic[A]

  /** Scala 3-specific deep diagnostic for dependent enum types.
    *
    * Reports TypeRepr details, erasure, and attempts different tree construction strategies for pattern matching.
    */
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

        // What does .termRef give us?
        val termRefStr = scala.util.Try(childSym.termRef.show).getOrElse("<no termRef>")

        // What does the singleton type look like?
        val singletonRepr =
          if isEnum && (isStable || childSym.flags
            .is(Flags.JavaStatic)) then scala.util.Try(childSym.typeRef.show).getOrElse("<error>")
          else "<not a val>"

        // What does TypeRepr.of give us with the child type?
        val childType = childRepr.asType
        val ofRepr = childType match {
          case '[t] => TypeRepr.of[t].show
        }

        // Try .widen to see the erasure
        val widenedStr = scala.util.Try(childRepr.widen.show).getOrElse("<error>")

        // Try different tree construction approaches
        val approach1 = scala.util
          .Try {
            // Approach 1: Ident(sym.termRef) - current approach
            if isEnum && isStable then Ident(childSym.termRef).show
            else "<n/a>"
          }
          .getOrElse("<error>")

        val approach2 = scala.util
          .Try {
            // Approach 2: Select from parent module
            if isEnum && isStable then {
              val parentModule = parentSym.companionModule
              if !parentModule.isNoSymbol then Select(Ref(parentModule), childSym).show
              else "<no parent module>"
            } else "<n/a>"
          }
          .getOrElse("<error>")

        val approach3 = scala.util
          .Try {
            // Approach 3: Ref(childSym) directly
            if isEnum && isStable then Ref(childSym).show
            else "<n/a>"
          }
          .getOrElse("<error>")

        // Check what a Mirror would give us
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
}
