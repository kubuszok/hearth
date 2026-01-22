package hearth
package std
package extensions

/** Macro extension providing support for Java EnumMap.
  *
  * Supports all [[java.util.EnumMap]] with proper extraction of the `E <: Enum[E]` key type parameter.
  * Converts it to [[scala.collection.Iterable]] using [[scala.jdk.javaapi.CollectionConverters.asScala]],
  * and providing as [[scala.collection.Factory]] implementation. Treats it as a type without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaEnumMap extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val juEnumMap = Type.Ctor2.of[java.util.EnumMap]

      // FIXME: Same issue as in IsCollectionProviderForJavaStream.scala: we have a bug in Type.Ctor.
      // private lazy val Entry = Type.Ctor2.of[java.util.Map.Entry]
      private def Entry[A: Type, B: Type]: Type[java.util.Map.Entry[A, B]] = Type.of[java.util.Map.Entry[A, B]]

      private lazy val Entry = Type.Ctor2.of[java.util.Map.Entry]

      private def isMap[Key0, Value0, A <: java.util.EnumMap[Key0, Value0]](
          A: Type[A],
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
          // Java enum maps have no smart constructors, we'll provide a Factory that build them as plain values.
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

      @scala.annotation.nowarn
      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = {
        // Try to match EnumMap[K, V] where K <: Enum[K]
        tpe match {
          case juEnumMap(key, value) =>
            import key.Underlying as Key
            import value.Underlying as Value

            // Verify that Key is an Enum
            if (Key <:< Type.of[java.lang.Enum[Key]]) {
              implicit val A: Type[A] = tpe
              implicit val EnumMapKV: Type[java.util.EnumMap[Key, Value]] = juEnumMap[Key, Value]
              // For EnumMap, we need to create an empty EnumMap of the proper type
              val emptyExpr: Expr[A] = Expr.quote {
                new java.util.EnumMap[Key, Value](classOf[Key]).asInstanceOf[A]
              }
              Some(
                isMap[Key, Value, A](
                  A,
                  emptyExpr,
                  Type[Key],
                  Type[Value],
                  (pair: Expr[java.util.Map.Entry[Key, Value]]) => Expr.quote(Expr.splice(pair).getKey()),
                  (pair: Expr[java.util.Map.Entry[Key, Value]]) => Expr.quote(Expr.splice(pair).getValue()),
                  (k: Expr[Key], v: Expr[Value]) =>
                    Expr.quote {
                      new java.util.AbstractMap.SimpleEntry[Key, Value](Expr.splice(k), Expr.splice(v))
                    }
                )
              )
            } else {
              None
            }

          case _ => None
        }
      }
    })
  }
}
