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
final class IsCollectionProviderForScalaCollection extends StandardMacroExtension {

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

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
          valueType: Type[Value0],
          keyExpr: Expr[Pair] => Expr[Key0],
          valueExpr: Expr[Pair] => Expr[Value0],
          pairExpr: (Expr[Key0], Expr[Value0]) => Expr[Pair]
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
          // Key and Value expressions are provided from the outside
          override type Key = Key0
          implicit override val Key: Type[Key] = keyType
          override type Value = Value0
          implicit override val Value: Type[Value] = valueType
          // We pass these from the outside, because Cross-Quotes on Scala 2 was missing Key and Value type substitution.
          override def key(pair: Expr[Pair]): Expr[Key] = keyExpr(pair)
          override def value(pair: Expr[Pair]): Expr[Value] = valueExpr(pair)
          override def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair] = pairExpr(key, value)
        })

      // FIXME: we had to make a workaround because we got:
      //   scala.collection.Factory[scala.Tuple2[java.lang.String, scala.Int], <none>.A]
      // when it was inlined. We should find a proper solution for this.
      private def findFactory[A: Type, Item: Type]: SummoningResult[scala.collection.Factory[Item, A]] =
        Expr.summonImplicit(using Type.of[scala.collection.Factory[Item, A]])

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        // Scala collections are Iterables with Factories, we're start by finding the item type...
        case Iterable(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe

          // ...then we can summon the Factory...
          findFactory[A, Item].toOption
            .map { factoryExpr =>
              // ...and use it to build the collection.
              val buildExpr: Expr[scala.collection.mutable.Builder[Item, A]] => Expr[A] =
                builder => Expr.quote(Expr.splice(builder).result())

              tpe match {
                case Map(key, value) =>
                  import key.Underlying as Key
                  import value.Underlying as Value
                  assert(Item =:= Tuple2[Key, Value])

                  val keyExpr: Expr[Item] => Expr[Key] =
                    pair => Expr.quote(Expr.splice(pair.asInstanceOf[Expr[(Key, Value)]])._1)
                  val valueExpr: Expr[Item] => Expr[Value] =
                    pair => Expr.quote(Expr.splice(pair.asInstanceOf[Expr[(Key, Value)]])._2)
                  val pairExpr: (Expr[Key], Expr[Value]) => Expr[Item] =
                    (k, v) => Expr.quote((Expr.splice(k), Expr.splice(v))).asInstanceOf[Expr[Item]]

                  isMap(A, factoryExpr, buildExpr, Key, Value, keyExpr, valueExpr, pairExpr)
                case _ =>
                  isCollection(A, factoryExpr, buildExpr)
              }
            }

        // Other types are not (Scala built-in) collections - if they should be supported, another extension can take care of it.
        case _ => None
      }
    })
  }
}
