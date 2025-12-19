package hearth
package std
package extensions

/** Macro extension providing support for AnyVal types.
  *
  * Supports each AnyVal which has a single constructor argument, where both constructor and argument ara available at
  * call site.
  *
  * @since 0.3.0
  */
final class AnyValIsWrapperProvider extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsWrapper.registerProvider(new IsWrapper.Provider {

      private lazy val AnyVal = Type.of[AnyVal]

      private def isWrapper[A, Inner: Type](
          unwrapExpr: Expr[A] => Expr[Inner],
          wrapExpr: Expr[Inner] => Expr[A]
      ): IsWrapper[A] =
        Existential[IsWrapperOf[A, *], Inner](new IsWrapperOf[A, Inner] {
          override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
          override val wrap: PossibleSmartCtor[Inner, A] = PossibleSmartCtor.PlainValue(wrapExpr)
        })

      override def unapply[A](tpe: Type[A]): Option[IsWrapper[A]] = if (tpe <:< AnyVal) {
        for {
          // Since we've already checked that the type is an AnyVal, let's see if we can wrap/unwrap it here.
          // We're looking for a primary constructor that is available at call site and has exactly one parameter.
          ctor <- tpe.primaryConstructor.filter { ctor =>
            ctor.isAvailable(AtCallSite) && ctor.parameters.flatten.sizeIs == 1
          }
          (name, argument) <- ctor.parameters.flatten.headOption
          ctorArgumentMethods = tpe.methods.filter { method =>
            method.value.isConstructorArgument
          }
          if ctorArgumentMethods.sizeIs == 1
          // We're looking for a method that is a constructor argument and is available at call site and has the same type as the primary constructor argument.
          getArgument <- ctorArgumentMethods.headOption.collect {
            case Method.OfInstance.Of(method)
                if method.value.isAvailable(AtCallSite) && method.Underlying =:= argument.tpe.Underlying =>
              method
          }
        } yield {
          import getArgument.{Underlying as Inner, value as unwrap}
          val unwrapExpr: Expr[A] => Expr[Inner] = wrapped =>
            unwrap(wrapped, Map()) match {
              case Right(unwrapped) => unwrapped
              case Left(error)      => hearthAssertionFailed("AnyVal unwrapping failed: " + error)
            }
          val wrapExpr: Expr[Inner] => Expr[A] = unwrapped =>
            ctor(Map(name -> unwrapped.as_??)) match {
              case Right(wrapped) => wrapped
              case Left(error)    => hearthAssertionFailed("AnyVal wrapping failed: " + error)
            }
          isWrapper[A, Inner](unwrapExpr, wrapExpr)
        }
      } else None
    })
  }
}
