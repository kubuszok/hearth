package hearth
package std
package extensions

/** Macro extension providing support for Java Optional.
  *
  * Supports [[java.util.Optional]] as an at-most-1-element collection. Converts it to [[scala.collection.Iterable]]
  * using [[scala.jdk.javaapi.OptionConverters.asScala]], and providing as [[scala.collection.Factory]] implementation.
  * Treats it as a type without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaOptional extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val juOptional = Type.Ctor1.of[java.util.Optional]

      private def isCollection[A, Item: Type](
          A: Type[A],
          toOptional: Expr[A] => Expr[java.util.Optional[Item]],
          fromOptional: Expr[java.util.Optional[Item]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // To convert Optional to Iterable we will use scala.jdk converters.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            scala.jdk.javaapi.OptionConverters.asScala(Expr.splice(toOptional(value))).to(Iterable)
          }
          // Java optionals have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override def factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private var impl: Option[Item] = None
                  override def clear(): Unit = impl = None
                  override def result(): A = {
                    val opt = impl match {
                      case Some(value) => java.util.Optional.of(value)
                      case None        => java.util.Optional.empty[Item]()
                    }
                    Expr.splice(fromOptional(Expr.quote(opt)))
                  }
                  override def addOne(elem: Item): this.type = {
                    if (impl.nonEmpty) {
                      throw new IllegalArgumentException("Optional can have at most one element")
                    }
                    impl = Some(elem)
                    this
                  }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = {
                val builder = newBuilder
                val iterator = it.iterator
                if (iterator.hasNext) {
                  builder.addOne(iterator.next())
                }
                if (iterator.hasNext) {
                  throw new IllegalArgumentException("Optional can have at most one element")
                }
                builder.result()
              }
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Item, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        // All Java optionals can be converted to Iterable.
        case juOptional(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val OptionalItem: Type[java.util.Optional[Item]] = juOptional[Item]
          Some(isCollection[A, Item](A, _.upcast[java.util.Optional[Item]], _.upcast[A]))

        // Other types are not Java optionals - if they should be supported, another extension can take care of it.
        case _ => None
      }
    })
  }
}
