package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

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
          override val wrap: CtorLikeOf[Boolean, java.lang.Boolean] =
            CtorLikeOf.PlainValue(
              (expr: Expr[Boolean]) => Expr.quote(java.lang.Boolean.valueOf(Expr.splice(expr))),
              None
            ) // TODO: we should provide a method for this
          override val ctors: CtorLikes[java.lang.Boolean] = CtorLikes
            .unapply(JBoolean)
            .getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, java.lang.Boolean], Boolean](wrap)))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JBoolean) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
