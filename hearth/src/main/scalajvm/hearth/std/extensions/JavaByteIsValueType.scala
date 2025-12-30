package hearth
package std
package extensions

/** Macro extension providing support for Java bytes.
  *
  * Supports all Java [[java.lang.Byte]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class JavaByteIsValueType extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Byte = Type.of[Byte]
      private lazy val JByte = Type.of[java.lang.Byte]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Byte] = {
        implicit val byte: Type[Byte] = Byte
        implicit val jByte: Type[java.lang.Byte] = JByte
        Existential[IsValueTypeOf[java.lang.Byte, *], Byte](new IsValueTypeOf[java.lang.Byte, Byte] {
          override val unwrap: Expr[java.lang.Byte] => Expr[Byte] =
            expr => Expr.quote(Expr.splice(expr).byteValue())
          override val wrap: PossibleSmartCtor[Byte, java.lang.Byte] =
            PossibleSmartCtor.PlainValue[Byte, java.lang.Byte](expr => Expr.quote(java.lang.Byte.valueOf(Expr.splice(expr))))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JByte) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
