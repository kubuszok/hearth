package hearth
package std
package extensions

/** Macro extension providing support for Java enumerations.
  *
  * Supports all Java [[java.util.Enumeration]]. Converts them to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and providing as [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaEnumeration extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val Enumeration = Type.Ctor1.of[java.util.Enumeration]

      private def isCollection[A, Item: Type](
          A: Type[A],
          toEnumeration: Expr[A] => Expr[java.util.Enumeration[Item]],
          fromEnumeration: Expr[java.util.Enumeration[Item]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // To convert Enumeration to Iterable we will use scala.jdk converters.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(toEnumeration(value))).to(Iterable)
          }
          // Java enumerations have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          // TODO:
          // Without `this.` on Scala 2 we're getting code like $newBuilder, $cast, $impl - which does not compile.
          // We should make a proper fix to printing this types.
          override val factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = new java.util.Vector[Item]
                  private def cast(enumeration: java.util.Enumeration[Item]): A =
                    Expr.splice(fromEnumeration(Expr.quote(enumeration)))
                  override def clear(): Unit = this.impl.clear()
                  override def result(): A = this.cast(this.impl.elements())
                  override def add(elem: Item): Unit = { this.impl.add(elem); () }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = this.newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Item, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        // All Java enumerations can be converted to Iterable.
        case Enumeration(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val EnumerationItem: Type[java.util.Enumeration[Item]] = Enumeration[Item]
          Some(isCollection[A, Item](A, _.upcast[java.util.Enumeration[Item]], _.upcast[A]))

        // Other types are not Java enumerations - if they should be supported, another extension can take care of it.
        case _ => None
      }
    })
  }
}
