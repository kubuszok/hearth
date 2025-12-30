package hearth
package std
package extensions

/** Macro extension providing support for Java characters.
  *
  * Supports all Java [[java.lang.Character]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class JavaCharacterIsValueType extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      private lazy val Char = Type.of[Char]
      private lazy val JCharacter = Type.of[java.lang.Character]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Character] = {
        implicit val char: Type[Char] = Char
        implicit val jCharacter: Type[java.lang.Character] = JCharacter
        Existential[IsValueTypeOf[java.lang.Character, *], Char](new IsValueTypeOf[java.lang.Character, Char] {
          override val unwrap: Expr[java.lang.Character] => Expr[Char] =
            expr => Expr.quote(Expr.splice(expr).charValue())
          override val wrap: PossibleSmartCtor[Char, java.lang.Character] =
            PossibleSmartCtor.PlainValue[Char, java.lang.Character](expr => Expr.quote(java.lang.Character.valueOf(Expr.splice(expr))))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JCharacter) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
