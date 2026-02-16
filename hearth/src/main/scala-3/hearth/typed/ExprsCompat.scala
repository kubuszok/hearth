package hearth
package typed

private[typed] trait ExprsCompat { this: MacroCommons =>

  trait ExprCompat { this: Expr.type =>
    def IArrayExprCodec[A: ExprCodec: Type]: ExprCodec[IArray[A]]
  }

  trait ExprCodecCompat {
    implicit def IArrayExprCodec[A: ExprCodec: Type]: ExprCodec[IArray[A]] = Expr.IArrayExprCodec[A]
  }
}
