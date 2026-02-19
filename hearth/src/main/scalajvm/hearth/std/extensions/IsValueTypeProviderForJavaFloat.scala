package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for Java floats.
  *
  * Supports all Java [[java.lang.Float]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaFloat extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Float = Type.of[Float]
      private lazy val JFloat = Type.of[java.lang.Float]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Float] = {
        implicit val float: Type[Float] = Float
        implicit val jFloat: Type[java.lang.Float] = JFloat
        Existential[IsValueTypeOf[java.lang.Float, *], Float](new IsValueTypeOf[java.lang.Float, Float] {
          override val unwrap: Expr[java.lang.Float] => Expr[Float] =
            expr => Expr.quote(Expr.splice(expr).floatValue())
          override val wrap: CtorLikeOf[Float, java.lang.Float] =
            CtorLikeOf.PlainValue(
              (expr: Expr[Float]) => Expr.quote(java.lang.Float.valueOf(Expr.splice(expr))),
              None // TODO: we should provide a method for this
            )
          override lazy val ctors: CtorLikes[java.lang.Float] = CtorLikes
            .unapply(JFloat)
            .getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, java.lang.Float], Float](wrap)))
        })
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe <:< JFloat) ProviderResult.Matched(isValueType.asInstanceOf[IsValueType[A]])
        else skipped(s"${tpe.prettyPrint} is not <: java.lang.Float")
    })
  }
}
