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
          ctorMethod: Method
      ): IsValueType[A] =
        Existential[IsValueTypeOf[A, *], Inner](new IsValueTypeOf[A, Inner] {
          override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
          override val wrap: CtorLikeOf[Inner, A] =
            CtorLikeOf.PlainValue(wrapExpr, Some(ctorMethod))
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
          ctorArgumentMethods = tpe.unsortedMethods.filter { method =>
            method.isConstructorArgument
          }
          if ctorArgumentMethods.sizeIs == 1
          // We're looking for a method that is a constructor argument and is available at call site and has the same type as the primary constructor argument.
          getArgument <- ctorArgumentMethods.headOption.collect {
            case method: Method.OnInstance
                if method
                  .isAvailable(AtCallSite) && method.knownReturning.exists(_.Underlying =:= argument.tpe.Underlying) =>
              method
          }
        } yield {
          @scala.annotation.nowarn
          implicit val _A: Type[A] = tpe
          val returnType = getArgument.knownReturning.get
          import returnType.Underlying as Inner
          val unwrapExpr: Expr[A] => Expr[Inner] = wrapped => {
            val instanceTpe = UntypedType.fromTyped[A](using tpe)
            getArgument.asUntyped
              .unsafeApplyInstance(instanceTpe)(wrapped.asUntyped, Map.empty)
              .asTyped[Inner]
          }
          val wrapExpr: Expr[Inner] => Expr[A] = unwrapped => {
            val applied = ctor match {
              case av: Method.ApplyValues => av(Map(name -> unwrapped.as_??))
              case other => hearthAssertionFailed("AnyVal wrapping: expected ApplyValues, got " + other)
            }
            applied match {
              case r: Method.Result[?] =>
                r.build() match {
                  case Right(wrapped) => wrapped.asInstanceOf[Expr[A]]
                  case Left(error)    => hearthAssertionFailed("AnyVal wrapping failed: " + error)
                }
              case other => hearthAssertionFailed("AnyVal wrapping: expected Result, got " + other)
            }
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
