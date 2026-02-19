package hearth
package std
package extensions

/** Macro extension providing support for Scala collections.
  *
  * Supports all Scala built-in collections, turns them into [[scala.collection.Iterable]] by upcasting, and using the
  * summoned [[scala.collection.Factory]] as Factory implementation. Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForScalaCollection extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Iterable = Type.Ctor1.of[Iterable]
      private lazy val Map = Type.Ctor2.of[scala.collection.Map]
      private lazy val Tuple2 = Type.Ctor2.of[Tuple2]

      private def isCollection[A, Item: Type](
          A: Type[A],
          factoryExpr: Expr[scala.collection.Factory[Item, A]],
          buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // We're just upcasting the collection to Iterable, to avoid things like type constructor extraction from generic F[A].
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = value.asInstanceOf[Expr[Iterable[Item]]]
          // Standard scala collections have no smart constructors, so we just return the collection itself.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Item, CtorResult]] = factoryExpr
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], A] =
            CtorLikeOf.PlainValue(
              buildExpr,
              None // TODO: we should provide a method for this
            )
        })

      private def isMap[A, Pair: Type, Key0, Value0](
          A: Type[A],
          factoryExpr: Expr[scala.collection.Factory[Pair, A]],
          buildExpr: Expr[scala.collection.mutable.Builder[Pair, A]] => Expr[A],
          keyType: Type[Key0],
          valueType: Type[Value0]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          // We're just upcasting the collection to Iterable, to avoid things like type constructor extraction from generic F[A].
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = value.asInstanceOf[Expr[Iterable[Pair]]]
          // Standard scala collections have no smart constructors, so we just return the collection itself.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Pair, CtorResult]] = factoryExpr
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Pair, CtorResult], A] =
            CtorLikeOf.PlainValue(
              buildExpr,
              None // TODO: we should provide a method for this
            )
          override type Key = Key0
          implicit override val Key: Type[Key] = keyType
          override type Value = Value0
          implicit override val Value: Type[Value] = valueType
          override def key(pair: Expr[Pair]): Expr[Key] =
            Expr.quote(Expr.splice(pair.asInstanceOf[Expr[(Key, Value)]])._1)
          override def value(pair: Expr[Pair]): Expr[Value] =
            Expr.quote(Expr.splice(pair.asInstanceOf[Expr[(Key, Value)]])._2)
          override def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair] =
            Expr.quote((Expr.splice(key), Expr.splice(value))).asInstanceOf[Expr[Pair]]
        })

      @scala.annotation.nowarn
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        // Scala collections are Iterables with Factories, we're start by finding the item type...
        case Iterable(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe

          // ...then we can summon the Factory...
          Expr.summonImplicit(using Type.of[scala.collection.Factory[Item, A]]).toOption match {
            case Some(factoryExpr) =>
              // ...and use it to build the collection.
              val buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A] =
                builder => Expr.quote(Expr.splice(builder).result())

              tpe match {
                case Map(key, value) =>
                  import key.Underlying as Key
                  import value.Underlying as Value
                  assert(Item =:= Tuple2[Key, Value])

                  ProviderResult.Matched(isMap(A, factoryExpr, buildExpr, Key, Value))
                case _ =>
                  ProviderResult.Matched(isCollection(A, factoryExpr, buildExpr))
              }
            case None =>
              skipped(s"${tpe.prettyPrint} is <: Iterable[${Item.prettyPrint}] but Factory not found")
          }

        // Other types are not (Scala built-in) collections - if they should be supported, another extension can take care of it.
        case _ => skipped(s"${tpe.prettyPrint} is not <: Iterable[_]")
      }
    })
  }
}
