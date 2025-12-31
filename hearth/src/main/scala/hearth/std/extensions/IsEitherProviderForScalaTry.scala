package hearth
package std
package extensions

import scala.util.{Failure, Success, Try}

/** Macro extension providing support for Scala tries.
  *
  * Supports all Scala built-in tries, turns them into [[scala.util.Try]] by upcasting. Treats them as types without
  * smart constructors.
  *
  * @since 0.3.0
  */
final class IsEitherProviderForScalaTry extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsEither.registerProvider(new IsEither.Provider {

      private lazy val Try = Type.Ctor1.of[Try]
      private lazy val Throwable = Type.of[Throwable]

      private def isTry[A: Type, Item](
          item: Type[Item],
          toTry: Expr[A] => Expr[Try[Item]],
          fromTry: Expr[Try[Item]] => Expr[A]
      ): IsEither[A] = new IsEither[A] {

        override type LeftValue = Throwable
        implicit override val LeftValue: Type[LeftValue] = Throwable

        override type RightValue = Item
        implicit override val RightValue: Type[RightValue] = item

        override def left(leftValue: Expr[LeftValue]): Expr[A] =
          fromTry(Expr.quote(Failure(Expr.splice(leftValue))))

        override def right(rightValue: Expr[RightValue]): Expr[A] =
          fromTry(Expr.quote(Success(Expr.splice(rightValue))))

        override def fold[B: Type](
            `try`: Expr[A]
        )(onLeft: Expr[LeftValue] => Expr[B], onRight: Expr[RightValue] => Expr[B]): Expr[B] =
          Expr.quote {
            Expr
              .splice(toTry(`try`))
              .toEither
              .fold[B](
                Expr.splice(LambdaBuilder.of1[LeftValue]("success").buildWith(onLeft)),
                Expr.splice(LambdaBuilder.of1[RightValue]("failure").buildWith(onRight))
              )
          }

        override def getOrElse(`try`: Expr[A])(default: Expr[RightValue]): Expr[RightValue] =
          Expr.quote {
            Expr.splice(toTry(`try`)).getOrElse(Expr.splice(default))
          }

        override def orElse(`try`: Expr[A])(default: Expr[A]): Expr[A] =
          fromTry(Expr.quote(Expr.splice(toTry(`try`)).orElse(Expr.splice(toTry(default)))))
      }

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsEither[A]] = tpe match {
        case Try(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val TryItem: Type[Try[Item]] = Try[Item]
          Some(isTry[A, Item](Item, _.upcast[Try[Item]], _.upcast[A]))
        case _ => None
      }
    })
  }
}
