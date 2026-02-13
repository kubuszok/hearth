package hearth
package std
package extensions

/** Macro extension providing support for Java dictionaries.
  *
  * Supports all Java [[java.util.Dictionary]]. Converts them to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and provides a [[scala.collection.Factory]] implementation.
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

      // FIXME: Same issue as in IsCollectionProviderForJavaStream.scala: we have a bug in Type.Ctor.
      // private lazy val Tuple2 = Type.Ctor2.of[Tuple2]
      private def Tuple2[A: Type, B: Type]: Type[Tuple2[A, B]] = Type.of[Tuple2[A, B]]

      private lazy val String = Type.of[String]

      private def isDictionary[Dict0[K, V] <: java.util.Dictionary[K, V], Key0, Value0, A <: Dict0[Key0, Value0]](
          A: Type[A],
          emptyDictExpr: Expr[A],
          keyType: Type[Key0],
          valueType: Type[Value0]
      ): IsCollection[A] = {
        type Pair = Tuple2[Key0, Value0]
        implicit val Pair: Type[Pair] = Tuple2[Key0, Value0](keyType, valueType)

        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          // We will use scala.jdk.javaapi.CollectionConverters.asScala to convert the dictionary to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = Expr.quote {
            val dict: java.util.Dictionary[Key, Value] =
              Expr.splice(value).asInstanceOf[java.util.Dictionary[Key, Value]]
            scala.jdk.javaapi.CollectionConverters
              .asScala(dict.keys())
              .map(key => (key, dict.get(key)))
              .to(Iterable)
          }
          // Java dictionaries have no smart constructors, we'll provide a Factory that builds them as plain values.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A

          override type Key = Key0
          implicit override val Key: Type[Key] = keyType
          override type Value = Value0
          implicit override val Value: Type[Value] = valueType
          override def factory: Expr[scala.collection.Factory[Pair, CtorResult]] = Expr.quote {
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
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Pair, CtorResult], A] =
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Pair, CtorResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              None // TODO: we should provide a method for this
            )
          override def key(pair: Expr[Pair]): Expr[Key] = Expr.quote(Expr.splice(pair)._1)
          override def value(pair: Expr[Pair]): Expr[Value] = Expr.quote(Expr.splice(pair)._2)
          override def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair] =
            Expr.quote((Expr.splice(key), Expr.splice(value)))
        })
      }

      private lazy val isProperties = {
        // Key =:= Value, so we had to extract these to avoid ambiguous implicit resolution (on Scala 3).
        val asScalaExpr = (dictionary: Expr[java.util.Properties]) =>
          Expr.quote {
            val properties: java.util.Properties = Expr.splice(dictionary)
            scala.jdk.javaapi.CollectionConverters
              .asScala(properties.keys())
              .map(key => (key.asInstanceOf[String], properties.get(key).asInstanceOf[String]))
              .to(Iterable)
          }
        def factoryExpr: Expr[scala.collection.Factory[(String, String), java.util.Properties]] = Expr.quote {
          new scala.collection.Factory[(String, String), java.util.Properties] {
            override def newBuilder: scala.collection.mutable.Builder[(String, String), java.util.Properties] =
              new scala.collection.mutable.Builder[(String, String), java.util.Properties] {
                private val impl = new java.util.Properties()
                override def clear(): Unit = impl.clear()
                override def result(): java.util.Properties = impl
                override def addOne(elem: (String, String)): this.type = { impl.put(elem._1, elem._2); this }
              }
            override def fromSpecific(it: IterableOnce[(String, String)]): java.util.Properties =
              newBuilder.addAll(it).result()
          }
        }
        val buildExpr = (expr: Expr[scala.collection.mutable.Builder[(String, String), java.util.Properties]]) =>
          Expr.quote(Expr.splice(expr).result())
        val keyExpr = (pair: Expr[(String, String)]) => Expr.quote(Expr.splice(pair)._1)
        val valueExpr = (pair: Expr[(String, String)]) => Expr.quote(Expr.splice(pair)._2)
        val pairExpr = (key: Expr[String], value: Expr[String]) => Expr.quote((Expr.splice(key), Expr.splice(value)))

        Existential[IsCollectionOf[java.util.Properties, *], (String, String)](
          new IsMapOf[java.util.Properties, (String, String)] {
            // We will use scala.jdk.javaapi.CollectionConverters.asScala to convert the dictionary to Iterable.
            override def asIterable(value: Expr[java.util.Properties]): Expr[Iterable[(String, String)]] =
              asScalaExpr(value)
            // Java dictionaries have no smart constructors, we'll provide a Factory that builds them as plain values.
            override type CtorResult = java.util.Properties
            implicit override val CtorResult: Type[CtorResult] = Type.of[java.util.Properties]

            override type Key = String
            override val Key: Type[Key] = String
            override type Value = String
            override val Value: Type[Value] = String
            // Key =:= Value, so here we have ambiguous implicit resolution.
            override def factory: Expr[scala.collection.Factory[(String, String), java.util.Properties]] = factoryExpr
            override def build: CtorLikeOf[
              scala.collection.mutable.Builder[(String, String), java.util.Properties],
              java.util.Properties
            ] = CtorLikeOf.PlainValue(
              buildExpr,
              None // TODO: we should provide a method for this
            )
            override def key(pair: Expr[(String, String)]): Expr[String] = keyExpr(pair)
            override def value(pair: Expr[(String, String)]): Expr[String] = valueExpr(pair)
            override def pair(key: Expr[String], value: Expr[String]): Expr[(String, String)] = pairExpr(key, value)
          }
        )(using Tuple2(using String, String))
      }

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = {
        def isDictionaryOf[Dict0[K, V] <: java.util.Dictionary[K, V], Key, Value, Dict1[K, V] <: Dict0[K, V]](
            dict: Type.Ctor2[Dict0], // just for type inference
            emptyDictExpr: Expr[Dict1[Key, Value]]
        )(implicit Key: Type[Key], Value: Type[Value]): IsCollection[A] =
          isDictionary[Dict0, Key, Value, Dict1[Key, Value]](
            tpe.asInstanceOf[Type[Dict1[Key, Value]]],
            emptyDictExpr.asInstanceOf[Expr[Dict1[Key, Value]]],
            Key,
            Value
          ).asInstanceOf[IsCollection[A]]

        // tpe is handled by one of Dictionary's subclasses OR it's exactly Dictionary[Key, Value]
        // so we can safely provide any implementation. Or we yield.
        def node[Dict0[K, V] <: java.util.Dictionary[K, V], Dict2[K, V] <: Dict0[K, V], Key: Type, Value: Type](
            ctor: Type.Ctor2[Dict0],
            emptyExpr: => Expr[Dict2[Key, Value]]
        )(
            forSubtype: => Option[IsCollection[A]]
        ): Option[IsCollection[A]] = {
          val dict = ctor[Key, Value]
          if (tpe <:< dict) {
            forSubtype orElse {
              if (tpe =:= dict) Some(isDictionaryOf(ctor, emptyExpr))
              else None
            }
          } else None
        }
        // tpe is exactly Dictionary[Key, Value], so we can provide a specific implementation. Or we yield.
        def leaf[Dict0[K, V] <: java.util.Dictionary[K, V], Dict2[K, V] <: Dict0[K, V], Key: Type, Value: Type](
            ctor: Type.Ctor2[Dict0],
            emptyExpr: => Expr[Dict2[Key, Value]]
        ): Option[IsCollection[A]] =
          if (tpe =:= ctor[Key, Value]) Some(isDictionaryOf(ctor, emptyExpr))
          else None

        // format: off
        tpe match {
          case juDictionary(key, value) =>
            import key.Underlying as Key
            import value.Underlying as Value

            node(juDictionary, Expr.quote(new java.util.Hashtable[Key, Value])) {
              node(juHashtable, Expr.quote(new java.util.Hashtable[Key, Value])) {
                if (tpe =:= juProperties) Some(isProperties.asInstanceOf[IsCollection[A]])
                else None
              }
            }
          case _ => None
        }
        // format: on
      }
    })
  }
}
