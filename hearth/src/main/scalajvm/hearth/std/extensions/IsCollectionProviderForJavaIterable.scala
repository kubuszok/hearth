package hearth
package std
package extensions

/** Macro extension providing support for [[java.lang.Iterable]].
  *
  * Supports [[java.lang.Iterable]][Item] as a collection. Converts to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and provides a [[scala.collection.Factory]] implementation
  * backed by [[java.util.ArrayList]]. Treats it as a type without smart constructors.
  *
  * Note: This provider is sorted after [[IsCollectionProviderForJavaCollection]] alphabetically, so
  * [[java.util.Collection]] subtypes are handled by their own provider first.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaIterable extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Iterable = Type.Ctor1.of[java.lang.Iterable]

      private def isIterable[A, Item: Type](
          A: Type[A],
          toIterable: Expr[A] => Expr[java.lang.Iterable[Item]],
          fromIterable: Expr[java.lang.Iterable[Item]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          override def asIterable(value: Expr[A]): Expr[scala.collection.Iterable[Item]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters
              .asScala(Expr.splice(toIterable(value)).iterator())
              .to(scala.collection.Iterable)
          }
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Item, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = new java.util.ArrayList[Item]
                  override def clear(): Unit = impl.clear()
                  override def result(): A = {
                    val it: java.lang.Iterable[Item] = impl
                    Expr.splice(fromIterable(Expr.quote(it)))
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
              None
            )
        })

      @scala.annotation.nowarn
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case Iterable(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          implicit val IterableItem: Type[java.lang.Iterable[Item]] = Iterable[Item]
          ProviderResult.Matched(isIterable[A, Item](A, _.upcast[java.lang.Iterable[Item]], _.upcast[A]))
        case _ => skipped(s"${tpe.prettyPrint} is not <: java.lang.Iterable[_]")
      }
    })
  }
}
