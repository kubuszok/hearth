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
final class IsEitherProviderForScalaTry extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsEither.registerProvider(new IsEither.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Try = Type.Ctor1.of[Try]
      private lazy val Throwable = Type.of[Throwable]

      private def isTry[A: Type, Item](
          toTry: Expr[A] => Expr[Try[Item]],
          fromTry: Expr[Try[Item]] => Expr[A]
      )(implicit Item: Type[Item]): IsEither[A] = {
        val impl = new IsEitherOf[A, Throwable, Item] {
          implicit private val Throwable0: Type[Throwable] = Throwable

          override def left(leftValue: Expr[Throwable]): Expr[A] =
            fromTry(Expr.quote(Failure(Expr.splice(leftValue))))

          override def right(rightValue: Expr[Item]): Expr[A] =
            fromTry(Expr.quote(Success(Expr.splice(rightValue))))

          override def fold[B: Type](
              `try`: Expr[A]
          )(onLeft: Expr[Throwable] => Expr[B], onRight: Expr[Item] => Expr[B]): Expr[B] =
            Expr.quote {
              Expr
                .splice(toTry(`try`))
                .toEither
                .fold[B](
                  Expr.splice(LambdaBuilder.of1[Throwable]("failure").buildWith(onLeft)),
                  Expr.splice(LambdaBuilder.of1[Item]("success").buildWith(onRight))
                )
            }

          override def getOrElse(`try`: Expr[A])(default: Expr[Item]): Expr[Item] =
            Expr.quote {
              Expr.splice(toTry(`try`)).getOrElse(Expr.splice(default))
            }

          override def orElse(`try`: Expr[A])(default: Expr[A]): Expr[A] =
            fromTry(Expr.quote(Expr.splice(toTry(`try`)).orElse(Expr.splice(toTry(default)))))
        }

        new IsEither[A] {

          override type LeftValue = Throwable
          implicit override val LeftValue: Type[LeftValue] = Throwable

          override type RightValue = Item
          implicit override val RightValue: Type[RightValue] = Item

          override val value: IsEitherOf[A, Throwable, Item] = impl
        }
      }

      @scala.annotation.nowarn
      override def parse[A](tpe: Type[A]): ProviderResult[IsEither[A]] = tpe match {
        case Try(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val TryItem: Type[Try[Item]] = Try[Item]
          ProviderResult.Matched(isTry[A, Item](_.upcast[Try[Item]], _.upcast[A]))
        case _ => skipped(s"${tpe.prettyPrint} is not Try[_]")
      }
    })
  }
}
