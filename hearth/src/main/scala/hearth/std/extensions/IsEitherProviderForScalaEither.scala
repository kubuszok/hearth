package hearth
package std
package extensions

/** Macro extension providing support for Scala eithers.
  *
  * Supports all Scala built-in eithers, turns them into [[scala.Either]] by upcasting. Treats them as types without
  * smart constructors.
  *
  * @since 0.3.0
  */
final class IsEitherProviderForScalaEither extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsEither.registerProvider(new IsEither.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Either = Type.Ctor2.of[Either]

      private def isEither[A: Type, LeftValue0, RightValue0](
          toEither: Expr[A] => Expr[Either[LeftValue0, RightValue0]],
          fromEither: Expr[Either[LeftValue0, RightValue0]] => Expr[A]
      )(implicit LeftValue0: Type[LeftValue0], RightValue0: Type[RightValue0]): IsEither[A] = {
        val impl = new IsEitherOf[A, LeftValue0, RightValue0] {

          override def left(leftValue: Expr[LeftValue0]): Expr[A] =
            fromEither(
              Expr.quote(Left[LeftValue0, RightValue0](Expr.splice(leftValue)): Either[LeftValue0, RightValue0])
            )

          override def right(rightValue: Expr[RightValue0]): Expr[A] =
            fromEither(
              Expr.quote(Right[LeftValue0, RightValue0](Expr.splice(rightValue)): Either[LeftValue0, RightValue0])
            )

          override def fold[B: Type](
              either: Expr[A]
          )(onLeft: Expr[LeftValue0] => Expr[B], onRight: Expr[RightValue0] => Expr[B]): Expr[B] =
            Expr.quote {
              Expr
                .splice(toEither(either))
                .fold[B](
                  Expr.splice(LambdaBuilder.of1[LeftValue0]("left").buildWith(onLeft)),
                  Expr.splice(LambdaBuilder.of1[RightValue0]("right").buildWith(onRight))
                )
            }

          override def getOrElse(either: Expr[A])(default: Expr[RightValue0]): Expr[RightValue0] =
            Expr.quote {
              Expr.splice(toEither(either)).getOrElse(Expr.splice(default))
            }

          override def orElse(either: Expr[A])(default: Expr[A]): Expr[A] =
            fromEither(Expr.quote(Expr.splice(toEither(either)).orElse(Expr.splice(toEither(default)))))
        }
        new IsEither[A] {

          override type LeftValue = LeftValue0
          implicit override val LeftValue: Type[LeftValue] = LeftValue0

          override type RightValue = RightValue0
          implicit override val RightValue: Type[RightValue] = RightValue0

          override val value: IsEitherOf[A, LeftValue0, RightValue0] = impl
        }
      }

      @scala.annotation.nowarn
      override def parse[A](tpe: Type[A]): ProviderResult[IsEither[A]] = tpe match {
        case Either(left, right) =>
          import left.Underlying as LeftValue
          import right.Underlying as RightValue
          implicit val A: Type[A] = tpe
          implicit val EitherLR: Type[Either[LeftValue, RightValue]] = Either[LeftValue, RightValue]
          ProviderResult.Matched(
            isEither[A, LeftValue, RightValue](_.upcast[Either[LeftValue, RightValue]], _.upcast[A])
          )
        case _ => skipped(s"${tpe.prettyPrint} is not Either[_, _]")
      }
    })
  }
}
