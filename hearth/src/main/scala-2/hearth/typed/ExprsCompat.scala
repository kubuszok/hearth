package hearth
package typed

private[typed] trait ExprsCompat { this: MacroCommons =>
  trait ExprCompat { this: Expr.type => }
  trait ExprCodecCompat
}
