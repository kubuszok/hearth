package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for plain value constructors.
  *
  * Supports all types that have a single constructor argument.
  *
  * @since 0.3.0
  */
final class CtorLikeProviderForPlainValue extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    import CtorLikeOf.*
    import CtorLikes.*

    CtorLikes.registerProvider(new CtorLikes.Provider {
      override def unapply[A](tpe: Type[A]): Option[CtorLikes[A]] = {
        implicit val A: Type[A] = tpe
        NonEmptyList.fromList(
          extractCtorLikesResult(new CtorBuilder[A, A] {
            def apply[Input: Type](
                ctor: Expr[Input] => Expr[A],
                method: Method.Returning[A]
            ): CtorLikeOf[Input, A] =
              PlainValue[Input, A](ctor, Some(method))
          })
        )
      }
    })
  }
}
