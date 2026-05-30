package hearth
package typed

import scala.language.experimental.macros

private[typed] trait ExprsCompat { this: MacroCommons =>
  trait ExprCompat { this: Expr.type => }
  trait ExprCodecCompat {
    def derived[A]: ExprCodec[A] = macro ExprCodecDerivationMacros.derivedImpl[A]
  }
}

class ExprCodecDerivationMacros(val c: scala.reflect.macros.blackbox.Context) extends MacroCommonsScala2 {
  import c.universe.*

  def derivedImpl[A: c.WeakTypeTag]: c.Tree = {
    val aType = weakTypeOf[A]
    q"deriveExprCodecInternal[$aType](Type.of[$aType])"
  }
}
