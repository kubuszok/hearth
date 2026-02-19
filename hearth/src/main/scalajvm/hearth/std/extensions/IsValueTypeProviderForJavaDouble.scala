package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for Java doubles.
  *
  * Supports all Java [[java.lang.Double]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaDouble extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Double = Type.of[Double]
      private lazy val JDouble = Type.of[java.lang.Double]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Double] = {
        implicit val double: Type[Double] = Double
        implicit val jDouble: Type[java.lang.Double] = JDouble
        Existential[IsValueTypeOf[java.lang.Double, *], Double](new IsValueTypeOf[java.lang.Double, Double] {
          override val unwrap: Expr[java.lang.Double] => Expr[Double] =
            expr => Expr.quote(Expr.splice(expr).doubleValue())
          override val wrap: CtorLikeOf[Double, java.lang.Double] =
            CtorLikeOf.PlainValue(
              (expr: Expr[Double]) => Expr.quote(java.lang.Double.valueOf(Expr.splice(expr))),
              None // TODO: we should provide a method for this
            )
          override lazy val ctors: CtorLikes[java.lang.Double] = CtorLikes
            .unapply(JDouble)
            .getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, java.lang.Double], Double](wrap)))
        })
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe <:< JDouble) ProviderResult.Matched(isValueType.asInstanceOf[IsValueType[A]])
        else skipped(s"${tpe.prettyPrint} is not <: java.lang.Double")
    })
  }
}
