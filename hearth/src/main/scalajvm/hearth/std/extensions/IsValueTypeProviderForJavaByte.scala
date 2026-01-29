package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for Java bytes.
  *
  * Supports all Java [[java.lang.Byte]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaByte extends StandardMacroExtension {

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
          override val wrap: CtorLikeOf[Byte, java.lang.Byte] =
            CtorLikeOf.PlainValue(
              (expr: Expr[Byte]) => Expr.quote(java.lang.Byte.valueOf(Expr.splice(expr))),
              None // TODO: we should provide a method for this
            )
          override lazy val ctors: CtorLikes[java.lang.Byte] =
            CtorLikes.unapply(JByte).getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, java.lang.Byte], Byte](wrap)))
        })(using Byte)
      }

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if (tpe <:< JByte) Some(isValueType.asInstanceOf[IsValueType[A]])
        else None
    })
  }
}
