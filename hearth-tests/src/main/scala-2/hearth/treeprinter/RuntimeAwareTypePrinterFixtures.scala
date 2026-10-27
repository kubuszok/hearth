package hearth
package treeprinter

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class RuntimeAwareTypePrinterFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with RuntimeAwareTypePrinterFixturesImpl {

  def testPlainPrintImpl[A: c.WeakTypeTag]: c.Expr[String] = testPlainPrint[A]
  def testNoOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testNoOverride[A]

  def testWithOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testWithOverride[A]

  def testPrettyWithOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testPrettyWithOverride[A]

  def testShortWithOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testShortWithOverride[A]

}

object RuntimeAwareTypePrinterFixtures {

  def testPlainPrint[A]: String = macro RuntimeAwareTypePrinterFixtures.testPlainPrintImpl[A]
  def testNoOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testNoOverrideImpl[A]

  def testWithOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testWithOverrideImpl[A]

  def testPrettyWithOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testPrettyWithOverrideImpl[A]

  def testShortWithOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testShortWithOverrideImpl[A]
}
