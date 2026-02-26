package hearth
package treeprinter

import scala.quoted.*

final private class RuntimeAwareTypePrinterFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      RuntimeAwareTypePrinterFixturesImpl {

  private lazy val typeNameCtor = Type.Ctor1.of[examples.TypeName]

  def testWithTypeName[A: Type]: Expr[String] =
    Type.runtimePlainPrint[A] { tpe =>
      import tpe.Underlying
      val typeNameType = typeNameCtor.apply[tpe.Underlying]
      Expr.summonImplicit(using typeNameType).toOption.map { expr =>
        '{ $expr.name }
      }
    }
}

object RuntimeAwareTypePrinterFixtures {

  inline def testNoOverride[A]: String = ${ testNoOverrideImpl[A] }
  private def testNoOverrideImpl[A: Type](using q: Quotes): Expr[String] =
    new RuntimeAwareTypePrinterFixtures(q).testNoOverride[A]

  inline def testWithOverride[A]: String = ${ testWithOverrideImpl[A] }
  private def testWithOverrideImpl[A: Type](using q: Quotes): Expr[String] =
    new RuntimeAwareTypePrinterFixtures(q).testWithOverride[A]

  inline def testPrettyWithOverride[A]: String = ${ testPrettyWithOverrideImpl[A] }
  private def testPrettyWithOverrideImpl[A: Type](using q: Quotes): Expr[String] =
    new RuntimeAwareTypePrinterFixtures(q).testPrettyWithOverride[A]

  inline def testShortWithOverride[A]: String = ${ testShortWithOverrideImpl[A] }
  private def testShortWithOverrideImpl[A: Type](using q: Quotes): Expr[String] =
    new RuntimeAwareTypePrinterFixtures(q).testShortWithOverride[A]

  inline def testWithTypeName[A]: String = ${ testWithTypeNameImpl[A] }
  private def testWithTypeNameImpl[A: Type](using q: Quotes): Expr[String] =
    new RuntimeAwareTypePrinterFixtures(q).testWithTypeName[A]
}
