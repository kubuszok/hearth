package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for Either[Iterable[Throwable], Value] constructors.
  *
  * Supports all types that have a single constructor argument.
  *
  * @since 0.3.0
  */
final class CtorLikeProviderForEitherIterableThrowableOrValue extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    import CtorLikeOf.*
    import CtorLikes.*

    CtorLikes.registerProvider(new CtorLikes.Provider {
      override def name: String = loader.getClass.getName
      override def parse[A](tpe: Type[A]): ProviderResult[CtorLikes[A]] = {
        implicit val A: Type[A] = tpe
        import EitherIterableThrowableOrValue.Result
        implicit val ResultA: Type[Result[A]] = Result[A]
        NonEmptyList.fromList(
          extractCtorLikesResult[A, Result[A]](new CtorBuilder[A, Result[A]] {
            def apply[Input: Type](
                ctor: Expr[Input] => Expr[Result[A]],
                method: Method.Returning[Result[A]]
            ): CtorLikeOf[Input, A] =
              EitherIterableThrowableOrValue[Input, A](ctor, Some(method))
          })
        ) match {
          case Some(ctors) => ProviderResult.Matched(ctors)
          case None        => skipped(s"no Either[Iterable[Throwable], A] constructors found for ${tpe.prettyPrint}")
        }
      }
    })
  }
}
