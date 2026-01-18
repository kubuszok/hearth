package hearth
package std
package extensions

/** Macro extension providing support for Scala options.
  *
  * Supports all Scala [[scala.Option]]. Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsOptionProviderForScalaOption extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsOption.registerProvider(new IsOption.Provider {

      private lazy val Option = Type.Ctor1.of[scala.Option]
      private lazy val Some = Type.Ctor1.of[scala.Some]

      private def isOption[A: Type, Item: Type](
          toOption: Expr[A] => Expr[Option[Item]],
          fromOption: Expr[Option[Item]] => Expr[A]
      ): IsOption[A] =
        Existential[IsOptionOf[A, *], Item](new IsOptionOf[A, Item] {
          override val empty: Expr[A] =
            fromOption(Expr.quote(scala.Option.empty[Item])) // FIXME: Expr(None) is not compiling
          override def of(value: Expr[Item]): Expr[A] =
            fromOption(Expr.quote(scala.Some(Expr.splice(value))))
          override def fold[B: Type](option: Expr[A])(onEmpty: Expr[B], onSome: Expr[Item] => Expr[B]): Expr[B] =
            Expr.quote {
              Expr
                .splice(toOption(option))
                .fold[B](Expr.splice(onEmpty))(Expr.splice(LambdaBuilder.of1[Item]("item").buildWith(onSome)))
            }
          override def getOrElse(option: Expr[A])(default: Expr[Item]): Expr[Item] =
            Expr.quote(Expr.splice(toOption(option)).getOrElse(Expr.splice(default)))
          override def orElse(option: Expr[A])(default: Expr[A]): Expr[A] =
            fromOption(Expr.quote(Expr.splice(toOption(option)).orElse(Expr.splice(toOption(default)))))
        })

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsOption[A]] = tpe match {
        case Some(_) => scala.None // Some is a special case, cannot be empty
        case Option(item)
            if !(item.Underlying =:= Type.of[Nothing]) /* Nothing is a special case, cannot be of something */ =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val OptionItem: Type[Option[Item]] = Option[Item]
          scala.Some(isOption[A, Item](_.upcast[Option[Item]], _.upcast[A]))
        case _ => None
      }
    })
  }
}
