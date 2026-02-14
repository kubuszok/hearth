package hearth
package std
package extensions

/** Macro extension providing support for [[java.util.Optional]] as an at-most-1-element collection.
  *
  * Treats [[java.util.Optional]][Item] as [[IsCollectionOf]][Optional[Item], Item]. Converts to
  * [[scala.collection.Iterable]] by checking `isPresent`. Provides a [[scala.collection.Factory]] that collects items
  * into a [[List]], and a smart constructor ([[CtorLikeOf.EitherStringOrValue]]) that validates the list contains at
  * most 1 element.
  *
  * This is separate from [[IsOptionProviderForJavaOptional]] which implements [[StdExtensions.IsOptionOf]].
  *
  * @since 0.4.0
  */
final class IsCollectionProviderForJavaOptional extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val Optional = Type.Ctor1.of[java.util.Optional]
      private lazy val ListType = Type.Ctor1.of[List]
      private lazy val BuilderType = Type.Ctor2.of[scala.collection.mutable.Builder]

      private def isOptional[A: Type, Item: Type](
          toOptional: Expr[A] => Expr[java.util.Optional[Item]],
          fromOptional: Expr[java.util.Optional[Item]] => Expr[A]
      ): IsCollection[A] = {
        implicit val listItemType: Type[List[Item]] = ListType[Item]
        implicit val builderType: Type[scala.collection.mutable.Builder[Item, List[Item]]] =
          BuilderType[Item, List[Item]]

        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            val opt = Expr.splice(toOptional(value))
            if (opt.isPresent) List(opt.get()) else Nil
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
                    Right(
                      Expr.splice(
                        Expr
                          .quote(
                            list.headOption
                              .map(x => java.util.Optional.of(x))
                              .getOrElse(java.util.Optional.empty[Item])
                          )
                          .asInstanceOf[Expr[A]]
                      )
                    )
                  else Left(s"Expected at most 1 element for Optional, got $${list.size}")
                },
              None
            )
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        case Optional(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val OptionalItem: Type[java.util.Optional[Item]] = Optional[Item]
          Some(isOptional[A, Item](_.upcast[java.util.Optional[Item]], _.upcast[A]))
        case _ => None
      }
    })
  }
}
