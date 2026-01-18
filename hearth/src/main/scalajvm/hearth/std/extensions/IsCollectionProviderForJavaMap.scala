package hearth
package std
package extensions

/** Macro extension providing support for Java maps.
  *
  * Supports all Java [[java.util.Map]]. Converts them to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and providing as [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaMap extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val Ordering = Type.Ctor1.of[Ordering]

      private lazy val juMap = Type.Ctor2.of[java.util.Map]
      private lazy val juAbstractMap = Type.Ctor2.of[java.util.AbstractMap]

      private lazy val juHashMap = Type.Ctor2.of[java.util.HashMap]
      private lazy val juLinkedHashMap = Type.Ctor2.of[java.util.LinkedHashMap]

      private lazy val juSortedMap = Type.Ctor2.of[java.util.SortedMap]
      private lazy val juNavigableMap = Type.Ctor2.of[java.util.NavigableMap]
      private lazy val juTreeMap = Type.Ctor2.of[java.util.TreeMap]

      private lazy val juWeakHashMap = Type.Ctor2.of[java.util.WeakHashMap]
      private lazy val juIdentityHashMap = Type.Ctor2.of[java.util.IdentityHashMap]

      // FIXME: Same issue as in IsCollectionProviderForJavaStream.scala: we have a bug in Type.Ctor.
      // private lazy val Entry = Type.Ctor2.of[java.util.Map.Entry]
      private def Entry[A: Type, B: Type]: Type[java.util.Map.Entry[A, B]] = Type.of[java.util.Map.Entry[A, B]]

      private lazy val Entry = Type.Ctor2.of[java.util.Map.Entry]

      private def isMap[Map0[K, V] <: java.util.Map[K, V], Key0, Value0, A <: Map0[Key0, Value0]](
          A: Type[A],
          Map0: Type.Ctor2[Map0],
          emptyMapExpr: => Expr[A],
          keyType: Type[Key0],
          valueType: Type[Value0],
          keyExpr: Expr[java.util.Map.Entry[Key0, Value0]] => Expr[Key0],
          valueExpr: Expr[java.util.Map.Entry[Key0, Value0]] => Expr[Value0],
          pairExpr: (Expr[Key0], Expr[Value0]) => Expr[java.util.Map.Entry[Key0, Value0]]
      ): IsCollection[A] = {
        type Pair = java.util.Map.Entry[Key0, Value0]
        implicit val Pair: Type[Pair] = Entry[Key0, Value0](keyType, valueType)

        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          // We will use scala.jdk.javaapi.CollectionConverters.asScala to convert the map to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(value).entrySet().iterator()).to(Iterable)
          }
          // Java maps have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override def factory: Expr[scala.collection.Factory[Pair, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Pair, A] {
              override def newBuilder: scala.collection.mutable.Builder[Pair, A] =
                new scala.collection.mutable.Builder[Pair, A] {
                  private val impl: A = Expr.splice(emptyMapExpr)
                  override def clear(): Unit = impl.clear()
                  override def result(): A = impl
                  override def addOne(elem: Pair): this.type = { impl.put(elem.getKey(), elem.getValue()); this }
                }
              override def fromSpecific(it: IterableOnce[Pair]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Pair, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Pair, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
          // Key and Value expressions are provided from the outside
          override type Key = Key0
          implicit override val Key: Type[Key] = keyType
          override type Value = Value0
          implicit override val Value: Type[Value] = valueType
          // FIXME: We pass these from the outside, because Cross-Quotes on Scala 2 was missing Key and Value type substitution.
          override def key(pair: Expr[Pair]): Expr[Key] = keyExpr(pair)
          override def value(pair: Expr[Pair]): Expr[Value] = valueExpr(pair)
          override def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair] = pairExpr(key, value)
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = {
        def isMapOf[Map0[K, V] <: java.util.Map[K, V], Key: Type, Value: Type, Map1[K, V] <: Map0[K, V]](
            map: Type.Ctor2[Map0],
            emptyMapExpr: => Expr[Map1[Key, Value]],
            keyType: Type[Key],
            valueType: Type[Value]
        ): IsCollection[A] =
          isMap[Map0, Key, Value, Map0[Key, Value]](
            tpe.asInstanceOf[Type[Map0[Key, Value]]],
            map,
            emptyMapExpr.asInstanceOf[Expr[Map0[Key, Value]]],
            keyType,
            valueType,
            (pair: Expr[java.util.Map.Entry[Key, Value]]) => Expr.quote(Expr.splice(pair).getKey()),
            (pair: Expr[java.util.Map.Entry[Key, Value]]) => Expr.quote(Expr.splice(pair).getValue()),
            (key: Expr[Key], value: Expr[Value]) =>
              Expr.quote(java.util.Map.entry(Expr.splice(key), Expr.splice(value)))
          ).asInstanceOf[IsCollection[A]]

        // tpe is handled by one of Map's subclasses OR it's exactly Map[Key, Value]
        // so we can safely provide any implementation. Or we yield.
        def node[Map0[K, V] <: java.util.Map[K, V], Map2[K, V] <: Map0[K, V], Key: Type, Value: Type](
            ctor: Type.Ctor2[Map0],
            emptyExpr: => Expr[Map2[Key, Value]]
        )(
            forSubtype: => Option[IsCollection[A]]
        ): Option[IsCollection[A]] = {
          val map = ctor[Key, Value]
          if (tpe <:< map) {
            forSubtype orElse {
              if (tpe =:= map) Some(isMapOf(ctor, emptyExpr, Type[Key], Type[Value]))
              else None
            }
          } else None
        }
        // tpe is exactly Map[Key, Value], so we can provide a specific implementation. Or we yield.
        def leaf[Map0[K, V] <: java.util.Map[K, V], Map2[K, V] <: Map0[K, V], Key: Type, Value: Type](
            ctor: Type.Ctor2[Map0],
            emptyExpr: => Expr[Map2[Key, Value]]
        ): Option[IsCollection[A]] =
          if (tpe =:= ctor[Key, Value]) Some(isMapOf(ctor, emptyExpr, Type[Key], Type[Value]))
          else None

        tpe match {
          case juMap(key, value) =>
            import key.Underlying as Key
            import value.Underlying as Value

            implicit lazy val OrderingKey: Type[Ordering[Key]] = Ordering[Key]
            lazy val orderingExprOpt = Expr.summonImplicit[Ordering[Key]].toOption

            // based on https://docs.oracle.com/javase/8/docs/api/java/util/package-tree.html
            def classHierarchy = node(juAbstractMap, Expr.quote(new java.util.HashMap[Key, Value])) {
              node(juHashMap, Expr.quote(new java.util.HashMap[Key, Value])) {
                leaf(juLinkedHashMap, Expr.quote(new java.util.LinkedHashMap[Key, Value]))
              }
                .orElse(
                  orderingExprOpt.flatMap { orderingExpr =>
                    node(juSortedMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr)))) {
                      node(juNavigableMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr)))) {
                        leaf(juTreeMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr))))
                      }
                    }
                  }
                )
                .orElse(leaf(juWeakHashMap, Expr.quote(new java.util.WeakHashMap[Key, Value])))
                .orElse(leaf(juIdentityHashMap, Expr.quote(new java.util.IdentityHashMap[Key, Value])))
              // TODO: handle java.util.EnumMaps
            }
            def interfaceHierarchy = node(juMap, Expr.quote(new java.util.HashMap[Key, Value])) {
              orderingExprOpt
                .flatMap { orderingExpr =>
                  node(juSortedMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr)))) {
                    node(juNavigableMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr)))) {
                      leaf(juTreeMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr))))
                    }
                  }
                }
            }

            classHierarchy orElse interfaceHierarchy
          case _ => None
        }
      }
    })
  }
}
