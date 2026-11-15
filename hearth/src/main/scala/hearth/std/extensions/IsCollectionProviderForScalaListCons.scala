package hearth
package std
package extensions

/** Macro extension providing support for [[scala.collection.immutable.::]], a non-empty `List`.
  *
  * There is no [[scala.collection.Factory]] building a `::`, so the elements are collected into a `List` and the smart
  * constructor fails for an empty one.
  *
  * @since 0.4.5
  */
final class IsCollectionProviderForScalaListCons extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Cons = Type.Ctor1.of[scala.collection.immutable.::]
      private lazy val ListType = Type.Ctor1.of[List]

      private def isCons[A: Type, Item: Type]: IsCollection[A] = {
        implicit val listItemType: Type[List[Item]] = ListType[Item]
        implicit val consItemType: Type[scala.collection.immutable.::[Item]] = Cons[Item]

        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = value.asInstanceOf[Expr[Iterable[Item]]]
          override def sizeHintForBuilder(value: Expr[A]): Option[Expr[Int]] = Some(Expr.quote {
            Expr.splice(value.asInstanceOf[Expr[Iterable[Item]]]).knownSize
          })
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
                  if (list.isEmpty) Left("Expected a non-empty list for ::")
                  else
                    Right(
                      Expr.splice(
                        Expr
                          .quote(new scala.collection.immutable.::(list.head, list.tail))
                          .asInstanceOf[Expr[A]]
                      )
                    )
                },
              None
            )
        })
      }

      override def mightMatch[A](tpe: Type[A]): Boolean = tpe match {
        case Cons(_) => true
        case _       => false
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case Cons(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          ProviderResult.Matched(isCons[A, Item])
        case _ => skippedLazily(s"${tpe.prettyPrint} is not ::[_]")
      }
    })
  }
}
