package hearth
package treeprinter

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class RuntimeAwareTypePrinterFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with RuntimeAwareTypePrinterFixturesImpl {

  def testNoOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testNoOverride[A]

  def testWithOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testWithOverride[A]

  def testPrettyWithOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testPrettyWithOverride[A]

  def testShortWithOverrideImpl[A: c.WeakTypeTag]: c.Expr[String] = testShortWithOverride[A]

  def testWithTypeNameImpl[A: c.WeakTypeTag]: c.Expr[String] = {
    val typeNameCtor = Type.Ctor1.of[examples.TypeName]
    Type.runtimePlainPrint[A] { tpe =>
      import tpe.Underlying
      val typeNameType = typeNameCtor.apply[tpe.Underlying]
      Expr.summonImplicit(using typeNameType).toOption.map { expr =>
        c.Expr[String](c.universe.Select(expr.tree, c.universe.TermName("name")))
      }
    }
  }
}

object RuntimeAwareTypePrinterFixtures {

  def testNoOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testNoOverrideImpl[A]

  def testWithOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testWithOverrideImpl[A]

  def testPrettyWithOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testPrettyWithOverrideImpl[A]

  def testShortWithOverride[A]: String = macro RuntimeAwareTypePrinterFixtures.testShortWithOverrideImpl[A]

  def testWithTypeName[A]: String = macro RuntimeAwareTypePrinterFixtures.testWithTypeNameImpl[A]
}
