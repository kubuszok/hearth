package hearth
package std
package extensions

/** Macro extension providing support for Java iterables.
  *
  * Supports all Java [[java.lang.Iterable]]. Converts them to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and providing as [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaIterable extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val Iterable = Type.Ctor1.of[java.lang.Iterable]

      private def isCollection[A, Item: Type](
          A: Type[A],
          toIterable: Expr[A] => Expr[java.lang.Iterable[Item]],
          fromIterable: Expr[java.lang.Iterable[Item]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // To convert Iterable to Iterable we will use scala.jdk converters.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(toIterable(value))).to(Iterable)
          }
          // Java iterables have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override def factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = new java.util.ArrayList[Item]
                  override def clear(): Unit = impl.clear()
                  override def result(): A = {
                    val it = impl.asInstanceOf[java.lang.Iterable[Item]]
                    Expr.splice(fromIterable(Expr.quote(it)))
                  }
                  override def addOne(elem: Item): this.type = { impl.add(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Item, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        // All Java iterables can be converted to Iterable.
        case Iterable(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val IterableItem: Type[java.lang.Iterable[Item]] = Iterable[Item]
          Some(isCollection[A, Item](A, _.upcast[java.lang.Iterable[Item]], _.upcast[A]))

        // Other types are not Java iterables - if they should be supported, another extension can take care of it.
        case _ => None
      }
    })
  }
}
