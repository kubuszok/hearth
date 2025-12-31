package hearth
package std
package extensions

/** Macro extension providing support for Java shorts.
  *
  * Supports all Java [[java.lang.Short]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaShort extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Short = Type.of[Short]
      private lazy val JShort = Type.of[java.lang.Short]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Short] = {
        implicit val short: Type[Short] = Short
        implicit val jShort: Type[java.lang.Short] = JShort
        Existential[IsValueTypeOf[java.lang.Short, *], Short](new IsValueTypeOf[java.lang.Short, Short] {
          override val unwrap: Expr[java.lang.Short] => Expr[Short] =
            expr => Expr.quote(Expr.splice(expr).shortValue())
          override val wrap: PossibleSmartCtor[Short, java.lang.Short] =
            PossibleSmartCtor.PlainValue[Short, java.lang.Short](expr =>
              Expr.quote(java.lang.Short.valueOf(Expr.splice(expr)))
            )
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JShort) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
