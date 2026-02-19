package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for Java integers.
  *
  * Supports all Java [[java.lang.Integer]]. Treats them as value types.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForJavaInteger extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Int = Type.of[Int]
      private lazy val JInteger = Type.of[java.lang.Integer]

      @scala.annotation.nowarn
      private val isValueType: IsValueType[java.lang.Integer] = {
        implicit val int: Type[Int] = Int
        implicit val jInteger: Type[java.lang.Integer] = JInteger
        Existential[IsValueTypeOf[java.lang.Integer, *], Int](new IsValueTypeOf[java.lang.Integer, Int] {
          override val unwrap: Expr[java.lang.Integer] => Expr[Int] =
            expr => Expr.quote(Expr.splice(expr).intValue())
          override val wrap: CtorLikeOf[Int, java.lang.Integer] =
            CtorLikeOf.PlainValue(
              (expr: Expr[Int]) => Expr.quote(java.lang.Integer.valueOf(Expr.splice(expr))),
              None // TODO: we should provide a method for this
            )
          override lazy val ctors: CtorLikes[java.lang.Integer] = CtorLikes
            .unapply(JInteger)
            .getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, java.lang.Integer], Int](wrap)))
        })(using Int)
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe <:< JInteger) ProviderResult.Matched(isValueType.asInstanceOf[IsValueType[A]])
        else skipped(s"${tpe.prettyPrint} is not <: java.lang.Integer")
    })
  }
}
