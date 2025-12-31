package hearth
package std
package extensions

/** Macro extension providing support for Java doubles.
  *
  * Supports all Java [[java.lang.Double]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaDouble extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Double = Type.of[Double]
      private lazy val JDouble = Type.of[java.lang.Double]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Double] = {
        implicit val double: Type[Double] = Double
        implicit val jDouble: Type[java.lang.Double] = JDouble
        Existential[IsValueTypeOf[java.lang.Double, *], Double](new IsValueTypeOf[java.lang.Double, Double] {
          override val unwrap: Expr[java.lang.Double] => Expr[Double] =
            expr => Expr.quote(Expr.splice(expr).doubleValue())
          override val wrap: PossibleSmartCtor[Double, java.lang.Double] =
            PossibleSmartCtor.PlainValue[Double, java.lang.Double](expr =>
              Expr.quote(java.lang.Double.valueOf(Expr.splice(expr)))
            )
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JDouble) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
