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
final class ScalaEitherIsEitherProvider extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsEither.registerProvider(new IsEither.Provider {

      private lazy val Either = Type.Ctor2.of[Either]

      private def isEither[A: Type, LeftValue0, RightValue0](
          leftValue: Type[LeftValue0],
          rightValue: Type[RightValue0],
          toEither: Expr[A] => Expr[Either[LeftValue0, RightValue0]],
          fromEither: Expr[Either[LeftValue0, RightValue0]] => Expr[A]
      ): IsEither[A] = new IsEither[A] {

        override type LeftValue = LeftValue0
        implicit override val LeftValue: Type[LeftValue] = leftValue

        override type RightValue = RightValue0
        implicit override val RightValue: Type[RightValue] = rightValue

        override def left(leftValue: Expr[LeftValue]): Expr[A] =
          fromEither(Expr.quote(Left(Expr.splice(leftValue)): Either[LeftValue0, RightValue0]))

        override def right(rightValue: Expr[RightValue]): Expr[A] =
          fromEither(Expr.quote(Right(Expr.splice(rightValue)): Either[LeftValue0, RightValue0]))

        override def fold[B: Type](
            either: Expr[A]
        )(onLeft: Expr[LeftValue] => Expr[B], onRight: Expr[RightValue] => Expr[B]): Expr[B] =
          Expr.quote {
            Expr
              .splice(toEither(either))
              .fold[B](
                Expr.splice(LambdaBuilder.of1[LeftValue]("left").buildWith(onLeft)),
                Expr.splice(LambdaBuilder.of1[RightValue]("right").buildWith(onRight))
              )
          }

        override def getOrElse(either: Expr[A])(default: Expr[RightValue]): Expr[RightValue] =
          Expr.quote {
            Expr.splice(toEither(either)).getOrElse(Expr.splice(default))
          }

        override def orElse(either: Expr[A])(default: Expr[A]): Expr[A] =
          fromEither(Expr.quote(Expr.splice(toEither(either)).orElse(Expr.splice(toEither(default)))))
      }

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsEither[A]] = tpe match {
        case Either(left, right) =>
          import left.Underlying as LeftValue
          import right.Underlying as RightValue
          implicit val A: Type[A] = tpe
          implicit val EitherLR: Type[Either[LeftValue, RightValue]] = Either[LeftValue, RightValue]
          Some(
            isEither[A, LeftValue, RightValue](
              LeftValue,
              RightValue,
              _.upcast[Either[LeftValue, RightValue]],
              _.upcast[A]
            )
          )
        case _ => None
      }
    })
  }
}
