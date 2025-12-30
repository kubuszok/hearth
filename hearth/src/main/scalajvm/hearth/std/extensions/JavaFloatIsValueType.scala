package hearth
package std
package extensions

/** Macro extension providing support for Java floats.
  *
  * Supports all Java [[java.lang.Float]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class JavaFloatIsValueType extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Float = Type.of[Float]
      private lazy val JFloat = Type.of[java.lang.Float]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Float] = {
        implicit val float: Type[Float] = Float
        implicit val jFloat: Type[java.lang.Float] = JFloat
        Existential[IsValueTypeOf[java.lang.Float, *], Float](new IsValueTypeOf[java.lang.Float, Float] {
          override val unwrap: Expr[java.lang.Float] => Expr[Float] =
            expr => Expr.quote(Expr.splice(expr).floatValue())
          override val wrap: PossibleSmartCtor[Float, java.lang.Float] =
            PossibleSmartCtor.PlainValue[Float, java.lang.Float](expr => Expr.quote(java.lang.Float.valueOf(Expr.splice(expr))))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JFloat) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
