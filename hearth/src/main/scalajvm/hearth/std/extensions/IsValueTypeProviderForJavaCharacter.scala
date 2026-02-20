package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for Java characters.
  *
  * Supports all Java [[java.lang.Character]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaCharacter extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Char = Type.of[Char]
      private lazy val JCharacter = Type.of[java.lang.Character]

      private lazy val valueOfMethod: Option[Method.Returning[java.lang.Character]] = {
        implicit val jCharacter: Type[java.lang.Character] = JCharacter
        Method.methodsOf[java.lang.Character].collectFirst {
          case Method.NoInstance(m)
              if m.name == "valueOf" && m.isUnary &&
                m.parameters.flatten.headOption.exists { case (_, p) => p.tpe.Underlying <:< Char } =>
            m.asReturning
        }
      }

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Character] = {
        implicit val char: Type[Char] = Char
        implicit val jCharacter: Type[java.lang.Character] = JCharacter
        Existential[IsValueTypeOf[java.lang.Character, *], Char](new IsValueTypeOf[java.lang.Character, Char] {
          override val unwrap: Expr[java.lang.Character] => Expr[Char] =
            expr => Expr.quote(Expr.splice(expr).charValue())
          override val wrap: CtorLikeOf[Char, java.lang.Character] =
            CtorLikeOf.PlainValue(
              (expr: Expr[Char]) => Expr.quote(java.lang.Character.valueOf(Expr.splice(expr))),
              valueOfMethod
            )
          override lazy val ctors: CtorLikes[java.lang.Character] = CtorLikes
            .unapply(JCharacter)
            .getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, java.lang.Character], Char](wrap)))
        })
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe <:< JCharacter) ProviderResult.Matched(isValueType.asInstanceOf[IsValueType[A]])
        else skipped(s"${tpe.prettyPrint} is not <: java.lang.Character")
    })
  }
}
