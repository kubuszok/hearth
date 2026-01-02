package hearth
package std
package extensions

final class IsCollectionProviderForJavaDictionary extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val juDictionary = Type.Ctor2.of[java.util.Dictionary]
      private lazy val juHashtable = Type.Ctor2.of[java.util.Hashtable]
      private lazy val juProperties = Type.of[java.util.Properties]

      private lazy val Entry = Type.Ctor2.of[java.util.Map.Entry]

      private lazy val String = Type.of[String]

      private def isDictionary[Dict0[K, V] <: java.util.Dictionary[K, V], Key0, Value0, A <: Dict0[Key0, Value0]](
          A: Type[A],
          Dict0: Type.Ctor2[Dict0],
          emptyDictExpr: Expr[A],
          keyType: Type[Key0],
          valueType: Type[Value0]
      ): IsCollection[A] = {
        type Pair = java.util.Map.Entry[Key0, Value0]
        implicit val Pair: Type[Pair] = Entry[Key0, Value0](keyType, valueType)

        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          override type Coll[A0] = Iterable[A0]
          override val Coll: Type.Ctor1[Coll] = Type.Ctor1.of[Iterable]
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters
              .asScala(Expr.splice(value).keys())
              .map { key =>
                val dict = Expr.splice(value)
                java.util.Map.entry(key, dict.get(key))
              }
              .to(Iterable)
          }
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
                  override def clear(): Unit = this.impl match {
                    case hashtable: java.util.Hashtable[?, ?] => hashtable.clear()
                    case dictionary                           =>
                      dictionary.synchronized {
                        val keys = java.util.Collections.list(dictionary.keys())
                        keys.forEach(dictionary.remove)
                      }
                  }
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
          override def key(pair: Expr[Pair]): Expr[Key] = Expr.quote(Expr.splice(pair).getKey())
          override def value(pair: Expr[Pair]): Expr[Value] = Expr.quote(Expr.splice(pair).getValue())
          override def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair] =
            Expr.quote(java.util.Map.entry(Expr.splice(key), Expr.splice(value)))
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
