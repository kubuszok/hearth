package hearth
package std
package extensions

import hearth.fp.data.NonEmptyList

/** Macro extension providing support for AnyVal types.
  *
  * Supports each AnyVal which has a single constructor argument, where both constructor and argument are available at
  * call site.
  *
  * @since 0.3.0
  */
final class IsValueTypeProviderForAnyVal extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      private lazy val AnyVal = Type.of[AnyVal]

      private def isValueType[A: Type, Inner: Type](
          unwrapExpr: Expr[A] => Expr[Inner],
          wrapExpr: Expr[Inner] => Expr[A],
          ctorMethod: Method.NoInstance[A]
      ): IsValueType[A] =
        Existential[IsValueTypeOf[A, *], Inner](new IsValueTypeOf[A, Inner] {
          override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
          override val wrap: CtorLikeOf[Inner, A] =
            CtorLikeOf.PlainValue(wrapExpr, Some(ctorMethod.asReturning))
          override lazy val ctors: CtorLikes[A] =
            CtorLikes.unapply(Type[A]).getOrElse(NonEmptyList.one(Existential[CtorLikeOf[*, A], Inner](wrap)))
        })

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] = if (tpe <:< AnyVal) {
        val result = for {
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
          implicit val A: Type[A] = tpe
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
          isValueType[A, Inner](unwrapExpr, wrapExpr, ctor)
        }
        result match {
          case Some(value) => ProviderResult.Matched(value)
          case None => skipped(s"${tpe.prettyPrint} is <: AnyVal but no suitable single-param public constructor found")
        }
      } else skipped(s"${tpe.prettyPrint} is not <: AnyVal")
    })
  }
}
