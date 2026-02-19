package hearth
package std
package extensions

/** Macro extension providing support for [[scala.collection.Iterator]] as a collection.
  *
  * Treats [[scala.collection.Iterator]][Item] as [[IsCollectionOf]][Iterator[Item], Item]. Converts to
  * [[scala.collection.Iterable]] by consuming the iterator into a [[List]]. Provides a [[scala.collection.Factory]]
  * that builds a [[List]] internally and returns its iterator. Treats it as a type without smart constructors.
  *
  * Note: Iterators are consumed when converted to Iterable, so they can only be used once.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForScalaIterator extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Iterator = Type.Ctor1.of[scala.collection.Iterator]

      private def isIterator[A, Item: Type](
          A: Type[A],
          toIterator: Expr[A] => Expr[scala.collection.Iterator[Item]],
          fromIterator: Expr[scala.collection.Iterator[Item]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // Consuming the iterator into a List to convert to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            Expr.splice(toIterator(value)).toList
          }
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Item, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = List.newBuilder[Item]
                  override def clear(): Unit = impl.clear()
                  override def result(): A = {
                    val it = impl.result().iterator
                    Expr.splice(fromIterator(Expr.quote(it)))
                  }
                  override def addOne(elem: Item): this.type = { impl.addOne(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], A] =
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Item, CtorResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              None
            )
        })

      @scala.annotation.nowarn
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case Iterator(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val IteratorItem: Type[scala.collection.Iterator[Item]] = Iterator[Item]
          ProviderResult.Matched(isIterator[A, Item](tpe, _.upcast[scala.collection.Iterator[Item]], _.upcast[A]))
        case _ => skipped(s"${tpe.prettyPrint} is not Iterator[_]")
      }
    })
  }
}
