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

      private lazy val Entry = Type.Ctor2.of[java.util.Map.Entry]

      private def isMap[Map0[K, V] <: java.util.Map[K, V], Key0, Value0, A <: Map0[Key0, Value0]](
          A: Type[A],
          Map0: Type.Ctor2[Map0],
          emptyMapExpr: Expr[A],
          keyType: Type[Key0],
          valueType: Type[Value0]
      ): IsCollection[A] = {
        type Pair = java.util.Map.Entry[Key0, Value0]
        implicit val Pair: Type[Pair] = Entry[Key0, Value0](keyType, valueType)

        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          override type Coll[A0] = Iterable[A0]
          override val Coll: Type.Ctor1[Coll] = Type.Ctor1.of[Iterable]
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(value).entrySet().iterator()).to(Iterable)
          }
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override val factory: Expr[scala.collection.Factory[Pair, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Pair, A] {
              override def newBuilder: scala.collection.mutable.Builder[Pair, A] =
                new scala.collection.mutable.Builder[Pair, A] {
                  private val impl = Expr.splice(emptyMapExpr)
                  override def clear(): Unit = this.impl.clear()
                  override def result(): A = this.impl
                  override def addOne(elem: Pair): this.type = { this.impl.put(elem.getKey(), elem.getValue()); this }
                }
              override def fromSpecific(it: IterableOnce[Pair]): A = this.newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Pair, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Pair, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }

          override type Key = Key0
          implicit override val Key: Type[Key] = keyType
          override type Value = Value0
          implicit override val Value: Type[Value] = valueType
          override def key(pair: Expr[Pair]): Expr[Key] = Expr.quote(Expr.splice(pair).getKey())
          override def value(pair: Expr[Pair]): Expr[Value] = Expr.quote(Expr.splice(pair).getValue())
          override def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair] =
            Expr.quote(java.util.Map.entry(Expr.splice(key), Expr.splice(value)))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = {
        def isMapOf[Map0[K, V] <: java.util.Map[K, V], Key: Type, Value: Type, Map1[K, V] <: Map0[K, V]](
            map: Type.Ctor2[Map0],
            emptyMapExpr: Expr[Map1[Key, Value]],
            keyType: Type[Key],
            valueType: Type[Value]
        ): Option[IsCollection[A]] =
          isMap[Map0, Key, Value, Map0[Key, Value]](
            tpe.asInstanceOf[Type[Map0[Key, Value]]],
            map,
            emptyMapExpr.asInstanceOf[Expr[Map0[Key, Value]]],
            keyType,
            valueType
          )
            .asInstanceOf[Option[IsCollection[A]]]

        // format: off
        tpe match {
          case juMap(key, value) =>
            import key.Underlying as Key
            import value.Underlying as Value

            implicit lazy val OrderingKey: Type[Ordering[Key]] = Ordering[Key]
            lazy val orderingExprOpt = Expr.summonImplicit[Ordering[Key]].toOption

            tpe match {
              case juAbstractMap(key, value) => tpe match {
                case juHashMap(_, _) => tpe match {
                  case juLinkedHashMap(_, _) => isMapOf(juLinkedHashMap, Expr.quote(new java.util.LinkedHashMap[Key, Value]), Key, Value)
                  // handle remaining java.util.HashMaps as java.util.HashMap
                  case _ => isMapOf(juHashMap, Expr.quote(new java.util.HashMap[Key, Value]), Key, Value)
                }
                case juSortedMap(_, _) if orderingExprOpt.isDefined =>
                  val orderingExpr = orderingExprOpt.get
                  tpe match {
                    case juNavigableMap(_, _) => tpe match {
                      case juTreeMap(_, _) => isMapOf(juTreeMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr))), Key, Value)
                      // handle remaining java.util.NavigableMaps as java.util.TreeMap
                      case _ => isMapOf(juNavigableMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr))), Key, Value)
                    }
                    // handle remaining java.util.SortedMaps as java.util.NavigableMap as java.util.TreeMap
                    case _ => isMapOf(juNavigableMap, Expr.quote(new java.util.TreeMap[Key, Value](Expr.splice(orderingExpr))), Key, Value)
                  }
                case juWeakHashMap(_, _) => isMapOf(juWeakHashMap, Expr.quote(new java.util.WeakHashMap[Key, Value]), Key, Value)
                case juIdentityHashMap(_, _) => isMapOf(juIdentityHashMap, Expr.quote(new java.util.IdentityHashMap[Key, Value]), Key, Value)
                // TODO: handle java.util.EnumMaps
                // handle remaining java.util.AbstractMaps as java.util.HashMap
                case _ => isMapOf(juAbstractMap, Expr.quote(new java.util.HashMap[Key, Value]), Key, Value)
              }
              // handle remaining java.util.Maps as java.util.AbstractMap as java.util.HashMap
              case _ => isMapOf(juMap, Expr.quote(new java.util.HashMap[Key, Value]), Key, Value)
            }
          case _ => None
        }
        // format: on
      }
    })
  }
}
