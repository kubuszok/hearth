package hearth
package std
package extensions

/** Macro extension providing support for Java iterators.
  *
  * Supports all Java [[java.util.Iterator]]. Converts them to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and provides a [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaIterator extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val Iterator = Type.Ctor1.of[java.util.Iterator]

      private def isCollection[A, Item: Type](
          A: Type[A],
          toIterator: Expr[A] => Expr[java.util.Iterator[Item]],
          fromIterator: Expr[java.util.Iterator[Item]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // To convert Iterator to Iterable we will use scala.jdk converters.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(toIterator(value))).to(Iterable)
          }
          // Java iterators have no smart constructors, we'll provide a Factory that builds them as plain values.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Item, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = new java.util.ArrayList[Item]
                  override def clear(): Unit = impl.clear()
                  override def result(): A = {
                    val it = impl.iterator() // Scala 3 requires Expr.quote to take a value, not an expression(?).
                    Expr.splice(fromIterator(Expr.quote(it)))
                  }
                  override def addOne(elem: Item): this.type = { impl.add(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], A] =
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Item, CtorResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              None // TODO: we should provide a method for this
            )
        })

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        // All Java iterators can be converted to Iterable.
        case Iterator(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val IteratorItem: Type[java.util.Iterator[Item]] = Iterator[Item]
          Some(isCollection[A, Item](A, _.upcast[java.util.Iterator[Item]], _.upcast[A]))

        // Other types are not Java iterators - if they should be supported, another extension can take care of it.
        case _ => None
      }
    })
  }
}
