package hearth
package std
package extensions

/** Macro extension providing support for Scala options.
  *
  * Supports all Java [[java.util.Optional]]. Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class JavaOptionalIsOptionProvider extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsOption.registerProvider(new IsOption.Provider {

      private lazy val Optional = Type.Ctor1.of[java.util.Optional]

      private def isOption[A: Type, Item: Type](
          toOptional: Expr[A] => Expr[java.util.Optional[Item]],
          fromOptional: Expr[java.util.Optional[Item]] => Expr[A]
      ): IsOption[A] =
        Existential[IsOptionOf[A, *], Item](new IsOptionOf[A, Item] {
          override val empty: Expr[A] =
            fromOptional(Expr.quote(java.util.Optional.empty[Item]))
          override def of(value: Expr[Item]): Expr[A] =
            fromOptional(Expr.quote(java.util.Optional.of(Expr.splice(value))))
          override def fold[B: Type](option: Expr[A])(onEmpty: Expr[B], onSome: Expr[Item] => Expr[B]): Expr[B] =
            Expr.quote {
              Expr
                .splice(toOptional(option))
                .map(item => Expr.splice(onSome(Expr.quote(item))))
                .orElse(Expr.splice(onEmpty))
            }
          override def getOrElse(option: Expr[A])(default: Expr[Item]): Expr[Item] =
            Expr.quote(Expr.splice(toOptional(option)).orElse(Expr.splice(default)))
          override def orElse(option: Expr[A])(default: Expr[A]): Expr[A] =
            fromOptional(Expr.quote(Expr.splice(toOptional(option)).or(() => Expr.splice(toOptional(default)))))
        })

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsOption[A]] = tpe match {
        case Optional(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val OptionalItem: Type[java.util.Optional[Item]] = Optional[Item]
          Some(isOption[A, Item](_.upcast[java.util.Optional[Item]], _.upcast[A]))
        case _ => None
      }
    })
  }
}
