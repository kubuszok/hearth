package hearth
package std
package extensions

/** Macro extension providing support for Java EnumSet.
  *
  * Supports all [[java.util.EnumSet]] with proper extraction of the `E <: Enum[E]` type parameter.
  * Converts it to [[scala.collection.Iterable]] using [[scala.jdk.javaapi.CollectionConverters.asScala]], and
  * providing as [[scala.collection.Factory]] implementation. Treats it as a type without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaEnumSet extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      // EnumSet is a special Set - its item type must be an Enum
      private lazy val juEnumSet = Type.Ctor1.of[java.util.EnumSet]

      private def isCollection[A, Item: Type](
          A: Type[A],
          emptyCollExpr: Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // We will use scala.jdk.javaapi.CollectionConverters.asScala to convert the collection to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(value).iterator()).to(Iterable)
          }
          // Java enum sets have no smart constructors, we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override def factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = Expr.splice(emptyCollExpr)
                  override def clear(): Unit = impl.clear()
                  override def result(): A = impl
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
      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = {
        // Try to match EnumSet[E] where E <: Enum[E]
        tpe match {
          case juEnumSet(item) =>
            import item.Underlying as Item
            // Verify that Item is an Enum
            val enumType = Type.of[java.lang.Enum[?]]
            // Check if Item <: Enum[Item]
            if (Item <:< Type.of[java.lang.Enum[Item]]) {
              implicit val A: Type[A] = tpe
              implicit val EnumSetItem: Type[java.util.EnumSet[Item]] = juEnumSet[Item]
              // For EnumSet, we need to create an empty EnumSet of the proper type
              val emptyExpr: Expr[A] = Expr.quote {
                java.util.EnumSet.noneOf(classOf[Item]).asInstanceOf[A]
              }
              Some(isCollection[A, Item](A, emptyExpr))
            } else {
              None
            }

          case _ => None
        }
      }
    })
  }
}
