package hearth
package std
package extensions

/** Macro extension providing support for Java longs.
  *
  * Supports all Java [[java.lang.Long]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class JavaLongIsValueType extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Long = Type.of[Long]
      private lazy val JLong = Type.of[java.lang.Long]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Long] = {
        implicit val long: Type[Long] = Long
        implicit val jLong: Type[java.lang.Long] = JLong
        Existential[IsValueTypeOf[java.lang.Long, *], Long](new IsValueTypeOf[java.lang.Long, Long] {
          override val unwrap: Expr[java.lang.Long] => Expr[Long] =
            expr => Expr.quote(Expr.splice(expr).longValue())
          override val wrap: PossibleSmartCtor[Long, java.lang.Long] =
            PossibleSmartCtor.PlainValue[Long, java.lang.Long](expr => Expr.quote(java.lang.Long.valueOf(Expr.splice(expr))))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JLong) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
