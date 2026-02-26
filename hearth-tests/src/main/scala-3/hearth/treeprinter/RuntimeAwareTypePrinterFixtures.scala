package hearth
package treeprinter

import scala.quoted.*

final private class RuntimeAwareTypePrinterFixtures(q: Quotes)
    extends MacroCommonsScala3(using q),
      RuntimeAwareTypePrinterFixturesImpl

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
}
