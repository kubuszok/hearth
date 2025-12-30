package hearth
package std
package extensions

/** Macro extension providing support for Java integers.
  *
  * Supports all Java [[java.lang.Integer]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class JavaIntegerIsValueType extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Int = Type.of[Int]
      private lazy val JInteger = Type.of[java.lang.Integer]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Integer] = {
        implicit val int: Type[Int] = Int
        implicit val jInteger: Type[java.lang.Integer] = JInteger
        Existential[IsValueTypeOf[java.lang.Integer, *], Int](new IsValueTypeOf[java.lang.Integer, Int] {
          override val unwrap: Expr[java.lang.Integer] => Expr[Int] =
            expr => Expr.quote(Expr.splice(expr).intValue())
          override val wrap: PossibleSmartCtor[Int, java.lang.Integer] =
            PossibleSmartCtor.PlainValue[Int, java.lang.Integer](expr => Expr.quote(java.lang.Integer.valueOf(Expr.splice(expr))))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JInteger) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
