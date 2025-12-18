package hearth
package std
package extensions

/** Macro extension providing support for Scala collections.
  *
  * Supports all Scala built-in collections, by upcasting them to Iterable and using the factory to build the
  * collection.
  *
  * @since 0.3.0
  */
final class ScalaCollectionsMacroExtension extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private val Iterable = Type.Ctor1.of[Iterable]

      private def isCollection[A, Item: Type](
          A: Type[A],
          factoryExpr: Expr[scala.collection.Factory[Item, A]],
          buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // We're just upcasting the collection to Iterable, to avoid things like type constructor extraction from generic F[A].
          override type Coll[A0] = Iterable[A0]
          override val Coll: Type.Ctor1[Coll] = Iterable
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = value.upcast(using A, Iterable[Item])
          // Standard scala collections have no smart constructors, so we just return the collection itself.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override val factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = factoryExpr
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue(buildExpr)
        })

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        // Scala collections are Iterables with Factories, we're start by finding the item type...
        case Iterable(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe

          // ...then we can summon the Factory...
          Expr
            .summonImplicit(using Type.of[scala.collection.Factory[Item, A]])
            .toOption
            .map { factoryExpr =>
              // ...and use it to build the collection.
              val buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A] =
                builder => Expr.quote(Expr.splice(builder).result())
              isCollection(A, factoryExpr, buildExpr)
            }

        // Other types are not (Scala built-in) collections - if they should be supported, another extension can take care of it.
        case _ => None
      }
    })
  }
}
