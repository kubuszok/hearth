package hearth
package std
package extensions

/** Macro extension providing support for Java booleans.
  *
  * Supports all Java [[java.lang.Boolean]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaBoolean extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Boolean = Type.of[Boolean]
      private lazy val JBoolean = Type.of[java.lang.Boolean]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Boolean] = {
        implicit val boolean: Type[Boolean] = Boolean
        implicit val jBoolean: Type[java.lang.Boolean] = JBoolean
        Existential[IsValueTypeOf[java.lang.Boolean, *], Boolean](new IsValueTypeOf[java.lang.Boolean, Boolean] {
          override val unwrap: Expr[java.lang.Boolean] => Expr[Boolean] =
            expr => Expr.quote(Expr.splice(expr).booleanValue())
          override val wrap: PossibleSmartCtor[Boolean, java.lang.Boolean] =
            PossibleSmartCtor.PlainValue[Boolean, java.lang.Boolean](expr =>
              Expr.quote(java.lang.Boolean.valueOf(Expr.splice(expr)))
            )
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JBoolean) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
