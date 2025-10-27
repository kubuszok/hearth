package hearth
package treeprinter

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final private class ShowCodePrettyFixtures(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ShowCodePrettyFixturesImpl {

  // TODO: create macro annotation which would allow to do the following
  def testExprPrettyPrintImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = testExprPrettyPrint[A](expr)

  def testExprPlainPrintImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = testExprPlainPrint[A](expr)

  def testExprPrettyASTImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = testExprPrettyAST[A](expr)

  def testExprPlainASTImpl[A: c.WeakTypeTag](expr: c.Expr[A]): c.Expr[String] = testExprPlainAST[A](expr)

  def testTypePrettyPrintImpl[A: c.WeakTypeTag]: c.Expr[String] = testTypePrettyPrint[A]

  def testTypePlainPrintImpl[A: c.WeakTypeTag]: c.Expr[String] = testTypePlainPrint[A]
}

object ShowCodePrettyFixtures {

  def testExprPrettyPrint[A](expr: A): String = macro ShowCodePrettyFixtures.testExprPrettyPrintImpl[A]

  def testExprPlainPrint[A](expr: A): String = macro ShowCodePrettyFixtures.testExprPlainPrintImpl[A]

  def testExprPrettyAST[A](expr: A): String = macro ShowCodePrettyFixtures.testExprPrettyASTImpl[A]

  def testExprPlainAST[A](expr: A): String = macro ShowCodePrettyFixtures.testExprPlainASTImpl[A]

  def testTypePrettyPrint[A]: String = macro ShowCodePrettyFixtures.testTypePrettyPrintImpl[A]

  def testTypePlainPrint[A]: String = macro ShowCodePrettyFixtures.testTypePlainPrintImpl[A]
}
