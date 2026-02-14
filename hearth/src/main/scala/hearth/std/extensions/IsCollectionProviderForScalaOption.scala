package hearth
package std
package extensions

/** Macro extension providing support for [[scala.Option]] as an at-most-1-element collection.
  *
  * Treats [[scala.Option]][Item] as [[IsCollectionOf]][Option[Item], Item]. Converts to [[scala.collection.Iterable]]
  * using `toList`. Provides a [[scala.collection.Factory]] that collects items into a [[List]], and a smart constructor
  * ([[CtorLikeOf.EitherStringOrValue]]) that validates the list contains at most 1 element.
  *
  * Does not match [[scala.Some]] (it cannot be empty).
  *
  * @since 0.4.0
  */
final class IsCollectionProviderForScalaOption extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val Option = Type.Ctor1.of[scala.Option]
      private lazy val Some = Type.Ctor1.of[scala.Some]
      private lazy val ListType = Type.Ctor1.of[List]
      private lazy val BuilderType = Type.Ctor2.of[scala.collection.mutable.Builder]

      private def isOption[A: Type, Item: Type](
          toOption: Expr[A] => Expr[scala.Option[Item]],
          fromOption: Expr[scala.Option[Item]] => Expr[A]
      ): IsCollection[A] = {
        implicit val listItemType: Type[List[Item]] = ListType[Item]
        implicit val builderType: Type[scala.collection.mutable.Builder[Item, List[Item]]] =
          BuilderType[Item, List[Item]]

        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            Expr.splice(toOption(value)).toList
          }
          // CtorResult is List[Item] so the smart constructor can validate element count.
          override type CtorResult = List[Item]
          implicit override val CtorResult: Type[CtorResult] = listItemType
          override def factory: Expr[scala.collection.Factory[Item, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Item, List[Item]] {
              override def newBuilder: scala.collection.mutable.Builder[Item, List[Item]] = List.newBuilder[Item]
              override def fromSpecific(it: IterableOnce[Item]): List[Item] = List.from(it)
            }
          }
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], A] =
            CtorLikeOf.EitherStringOrValue(
              (builder: Expr[scala.collection.mutable.Builder[Item, List[Item]]]) =>
                Expr.quote {
                  val list = Expr.splice(builder).result()
                  if (list.size <= 1)
                    Right(Expr.splice(Expr.quote(list.headOption).asInstanceOf[Expr[A]]))
                  else Left(s"Expected at most 1 element for Option, got $${list.size}")
                },
              None
            )
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        case Some(_) => scala.None // Some is a special case, cannot be empty
        case Option(item) if !(item.Underlying =:= Type.of[Nothing]) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val OptionItem: Type[scala.Option[Item]] = Option[Item]
          scala.Some(isOption[A, Item](_.upcast[scala.Option[Item]], _.upcast[A]))
        case _ => None
      }
    })
  }
}
