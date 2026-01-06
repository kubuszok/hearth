package hearth
package std
package extensions

/** Macro extension providing support for Java dictionaries.
  *
  * Supports all Java [[java.util.Dictionary]]. Converts them to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and providing as [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaDictionary extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val juDictionary = Type.Ctor2.of[java.util.Dictionary]
      private lazy val juHashtable = Type.Ctor2.of[java.util.Hashtable]
      private lazy val juProperties = Type.of[java.util.Properties]

      private lazy val Tuple2 = Type.Ctor2.of[Tuple2]

      private lazy val String = Type.of[String]

      private def isDictionary[Dict0[K, V] <: java.util.Dictionary[K, V], Key0, Value0, A <: Dict0[Key0, Value0]](
          A: Type[A],
          Dict0: Type.Ctor2[Dict0],
          emptyDictExpr: Expr[A],
          keyType: Type[Key0],
          valueType: Type[Value0]
      ): IsCollection[A] = {
        type Pair = Tuple2[Key0, Value0]
        implicit val Pair: Type[Pair] = Tuple2[Key0, Value0](keyType, valueType)

        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          // We will use scala.jdk.javaapi.CollectionConverters.asScala to convert the dictionary to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = Expr.quote {
            val dict = Expr.splice(value.asInstanceOf[Expr[java.util.Dictionary[Key, Value]]])
            scala.jdk.javaapi.CollectionConverters.asScala(dict.keys()).map(key => (key, dict.get(key))).to(Iterable)
          }
          // Java dictionaries have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A

          override type Key = Key0
          implicit override val Key: Type[Key] = keyType
          override type Value = Value0
          implicit override val Value: Type[Value] = valueType
          override val factory: Expr[scala.collection.Factory[Pair, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Pair, A] {
              override def newBuilder: scala.collection.mutable.Builder[Pair, A] =
                new scala.collection.mutable.Builder[Pair, A] {
                  private val impl = Expr.splice(emptyDictExpr)
                  override def clear(): Unit = impl match {
                    case hashtable: java.util.Hashtable[?, ?] => hashtable.clear()
                    case dictionary                           =>
                      dictionary.synchronized {
                        val keys = java.util.Collections.list(dictionary.keys())
                        keys.forEach(dictionary.remove)
                      }
                  }
                  override def result(): A = impl
                  override def addOne(elem: Pair): this.type = { impl.put(elem._1, elem._2); this }
                }
              override def fromSpecific(it: IterableOnce[Pair]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Pair, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Pair, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
          override def key(pair: Expr[Pair]): Expr[Key] = Expr.quote(Expr.splice(pair)._1)
          override def value(pair: Expr[Pair]): Expr[Value] = Expr.quote(Expr.splice(pair)._2)
          override def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair] =
            Expr.quote((Expr.splice(key), Expr.splice(value)))
        })
      }

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = {
        def isDictionaryOf[Dict0[K, V] <: java.util.Dictionary[K, V], Key, Value, Dict1[K, V] <: Dict0[K, V]](
            dict: Type.Ctor2[Dict0],
            emptyDictExpr: Expr[Dict1[Key, Value]],
            keyType: Type[Key],
            valueType: Type[Value]
        ): Option[IsCollection[A]] =
          isDictionary[Dict0, Key, Value, Dict0[Key, Value]](
            tpe.asInstanceOf[Type[Dict0[Key, Value]]],
            dict,
            emptyDictExpr.asInstanceOf[Expr[Dict0[Key, Value]]],
            keyType,
            valueType
          )
            .asInstanceOf[Option[IsCollection[A]]]

        def isPropertiesOf(): Option[IsCollection[A]] =
          isDictionaryOf(
            juHashtable,
            Expr.quote(new java.util.Properties()).asInstanceOf[Expr[java.util.Hashtable[String, String]]],
            String,
            String
          ).asInstanceOf[Option[IsCollection[A]]]

        // format: off
        tpe match {
          case juDictionary(key, value) =>
            import key.Underlying as Key
            import value.Underlying as Value

            tpe match {
              case juHashtable(_, _) =>
                // java.util.Properties is a special case - it only supports String keys and values
                if (tpe =:= juProperties) isPropertiesOf()
                else isDictionaryOf(juHashtable, Expr.quote(new java.util.Hashtable[Key, Value]), Key, Value)
              // handle remaining java.util.Dictionaries as java.util.Hashtable
              case _ => isDictionaryOf(juDictionary, Expr.quote(new java.util.Hashtable[Key, Value]), Key, Value)
            }
          case _ if tpe =:= juProperties => isPropertiesOf()
          case _ => None
        }
        // format: on
      }
    })
  }
}
