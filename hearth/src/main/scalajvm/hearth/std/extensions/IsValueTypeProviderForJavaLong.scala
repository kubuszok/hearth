package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for Java longs.
  *
  * Supports all Java [[java.lang.Long]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaLong extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Long = Type.of[Long]
      private lazy val JLong = Type.of[java.lang.Long]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Long] = {
        implicit val long: Type[Long] = Long
        implicit val jLong: Type[java.lang.Long] = JLong
        Existential[IsValueTypeOf[java.lang.Long, *], Long](new IsValueTypeOf[java.lang.Long, Long] {
          override val unwrap: Expr[java.lang.Long] => Expr[Long] =
            expr => Expr.quote(Expr.splice(expr).longValue())
          override val wrap: CtorLikeOf[Long, java.lang.Long] =
            CtorLikeOf.PlainValue(
              (expr: Expr[Long]) => Expr.quote(java.lang.Long.valueOf(Expr.splice(expr))),
              None // TODO: we should provide a method for this
            )
          override lazy val ctors: CtorLikes[java.lang.Long] =
            CtorLikes.unapply(JLong).getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, java.lang.Long], Long](wrap)))
        })(using Long)
      }

      override def unapply[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe <:< JLong) ProviderResult.Matched(isValueType.asInstanceOf[IsValueType[A]])
        else skipped(s"${tpe.prettyPrint} is not <: java.lang.Long")
    })
  }
}
