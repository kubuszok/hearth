package hearth
package treeprinter

import scala.quoted.*

final private class ShowCodePrettyFixtures(q: Quotes) extends MacroCommonsScala3(using q), ShowCodePrettyFixturesImpl

object ShowCodePrettyFixtures {

  inline def testExprPrettyPrint[A](inline expr: A): String = ${ testExprPrettyPrintImpl[A]('expr) }
  private def testExprPrettyPrintImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ShowCodePrettyFixtures(q).testExprPrettyPrint[A](expr)

  inline def testExprPlainPrint[A](inline expr: A): String = ${ testExprPlainPrintImpl[A]('expr) }
  private def testExprPlainPrintImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ShowCodePrettyFixtures(q).testExprPlainPrint[A](expr)

  inline def testExprPrettyAST[A](inline expr: A): String = ${ testExprPrettyASTImpl[A]('expr) }
  private def testExprPrettyASTImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ShowCodePrettyFixtures(q).testExprPrettyAST[A](expr)

  inline def testExprPlainAST[A](inline expr: A): String = ${ testExprPlainASTImpl[A]('expr) }
  private def testExprPlainASTImpl[A: Type](expr: Expr[A])(using q: Quotes): Expr[String] =
    new ShowCodePrettyFixtures(q).testExprPlainAST[A](expr)

  inline def testTypePrettyPrint[A]: String = ${ testTypePrettyPrintImpl[A] }
  private def testTypePrettyPrintImpl[A: Type](using q: Quotes): Expr[String] =
    new ShowCodePrettyFixtures(q).testTypePrettyPrint[A]

  inline def testTypePlainPrint[A]: String = ${ testTypePlainPrintImpl[A] }
  private def testTypePlainPrintImpl[A: Type](using q: Quotes): Expr[String] =
    new ShowCodePrettyFixtures(q).testTypePlainPrint[A]
}
