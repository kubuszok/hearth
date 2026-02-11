package hearth
package std
package extensions

import scala.quoted.Quotes

/** Macro extension providing [[IsValueType]] for Scala 3 opaque types.
  *
  * Supports opaque types that have a companion with a method taking a single argument of the underlying type and
  * returning the opaque type, `Either[String, Opaque]`, or `Either[Iterable[String], Opaque]` (see [[CtorLikes]]).
  *
  * @since 0.3.0
  */
@scala.annotation.experimental
final class IsValueTypeProviderForOpaque extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = ctx match {
    case ctx3: (MacroCommonsScala3 & StdExtensions) =>
      // Extracts platform-specific type representation, so that it would not clash with Cross-Quotes
      import ctx3.{*, given}
      import ctx3.quotes.reflect.*

      /** Get the underlying type of an opaque type.
        *
        * Strategy:
        *   1. Try `simplified` which reduces TypeRefs to their underlying type when in scope
        *   2. Outside the opaque type's scope
        *    - try to infer from PlainValue ctor (the one that returns A directly, not Either[?, A])
        *    - fall back to the underlying type of the opaque type
        *
        * Note: We cannot use `sym.tree` from outside the opaque type's scope because it only shows TypeBounds, not the
        * actual underlying type.
        */
      def underlyingTypeReprImpl(repr: TypeRepr)(using ctx: MacroCommons & StdExtensions): TypeRepr = {
        val tpe = repr.dealias
        val sym = tpe.typeSymbol
        if sym.flags.is(Flags.Opaque) then {
          // Strategy 1: simplified reduces TypeRefs to their underlying type when in scope.
          val simplifiedTpe = tpe.simplified
          if simplifiedTpe.typeSymbol != sym then simplifiedTpe
          else {
            // Strategy 2: Infer from PlainValue smart constructor.
            // A PlainValue ctor has signature (Input => Output) where Output is the opaque type.
            // The Input type is the underlying type.
            CtorLikes
              .unapply(tpe.asTyped[Any])
              .map { nel =>
                def preferPlain = nel.toList.collectFirst {
                  case existential if existential.value.isInstanceOf[CtorLikeOf.PlainValue[?, ?]] =>
                    existential.Underlying.asUntyped
                }
                def fallback = nel.head.Underlying.asUntyped
                preferPlain getOrElse fallback
              }
              .getOrElse(tpe)
          }
        } else sym.typeRef
      }

      extend(ctx3, underlyingTypeReprImpl(_)(using ctx3))
    case _ =>
  }

  private def extend(
      ctx: MacroCommons & StdExtensions,
      underlyingTypeRepr: ctx.UntypedType => ctx.UntypedType
  ): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def unapply[A](tpe: Type[A]): Option[IsValueType[A]] =
        if !tpe.isOpaqueType then None
        else {
          implicit val A: Type[A] = tpe
          val repr: UntypedType = UntypedType.fromTyped[A]
          val underlyingRepr: UntypedType = underlyingTypeRepr(repr)

          val inner = underlyingRepr.as_??
          import inner.Underlying as Inner

          // Find smart constructors via CtorLikes
          CtorLikes
            .unapply(tpe)
            .flatMap { ctors =>
              // Find a ctor whose input type matches the underlying type
              ctors.toList
                .find { existential =>
                  import existential.Underlying as Input
                  Input <:< Inner
                }
                .map(_ -> ctors)
            }
            .map { case (ctor, allCtors) =>
              import ctor.value as wrapCtor
              val unwrapExpr: Expr[A] => Expr[Inner] =
                outer => Expr.quote(Expr.splice(outer).asInstanceOf[Inner])
              Existential[IsValueTypeOf[A, *], Inner](new IsValueTypeOf[A, Inner] {
                override val unwrap: Expr[A] => Expr[Inner] = unwrapExpr
                override val wrap: CtorLikeOf[Inner, A] = wrapCtor.asInstanceOf[CtorLikeOf[Inner, A]]
                override val ctors: CtorLikes[A] = allCtors
              })
            }
        }
    })
  }
}
