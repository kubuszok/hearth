package hearth
package typed

import hearth.data.Data

trait SourceTextFixturesImpl { this: MacroCommons =>

  /** Returns the source text of the passed expression (trimmed), or "<none>" if not available. */
  def testExprSourceText[A: Type](expr: Expr[A]): Expr[Data] =
    Expr(Data(expr.sourceCode.map(_.trim).getOrElse("<none>")))

  /** Source text of a position that does not point at real source (a NoPosition on Scala 2, an out-of-range span on
    * Scala 3). Implemented per-platform because there is no portable way to obtain a "positionless" tree: on Scala 3
    * every term built during macro expansion (even a freshly-built `Literal`) inherits the macro-expansion position, so
    * we must construct a deliberately invalid [[Position]] to exercise the guard.
    */
  protected def noSourcePositionSourceCode: Option[String]

  /** Returns the source text for a position with no real source. Proves the negative path (guard returns None). */
  def testSyntheticSourceText: Expr[Data] =
    Expr(Data(noSourcePositionSourceCode.map(_.trim).getOrElse("<none>")))

  /** Destructures an expression and reports the source text of each direct subexpression — proves the assert-macro use
    * case: walk subexpressions and recover their original source.
    */
  def testDestructuredSourceText[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    def sourceOf(e: DestructuredExpr): Data =
      Data(e.toUntypedExpr.sourceCode.map(_.trim).getOrElse("<none>"))
    parsed match {
      case mc: DestructuredExpr.MethodCall =>
        val argSources: List[Data] = mc.applied.flatMap {
          case av: DestructuredExpr.MethodCall.AppliedValues => av.args.map(sourceOf)
          case _                                             => Nil
        }
        Expr(
          Data.map(
            "whole" -> Data(expr.sourceCode.map(_.trim).getOrElse("<none>")),
            "methodName" -> Data(mc.method.name),
            "argSources" -> Data(argSources)
          )
        )
      case other =>
        Expr(
          Data.map(
            "whole" -> Data(expr.sourceCode.map(_.trim).getOrElse("<none>")),
            "node" -> Data(other.plainPrint)
          )
        )
    }
  }
}
