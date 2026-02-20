package hearth
package std
package extensions

/** Macro extension providing support for Java maps.
  *
  * Supports all Java [[java.util.Map]]. Converts them to [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and provides a [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaMap extends StandardMacroExtension { loader =>

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

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

      private lazy val juEnumType = Type.of[java.lang.Enum[?]]

      private lazy val Entry = Type.Ctor2.of[java.util.Map.Entry]
      private lazy val Builder = Type.Ctor2.of[scala.collection.mutable.Builder]

      // Helper for EnumMap: mirrors isMap but without requiring Type.Ctor2 (which can't express EnumMap bounds).
      // Uses Type.classOfType + casts through Nothing to create the EnumMap in generated code.
      private def isEnumMap[Key0, Value0, A](
          A: Type[A],
          keyType: Type[Key0],
          valueType: Type[Value0],
          keyClassExpr: Expr[java.lang.Class[Key0]]
      ): IsCollection[A] = {
        type Pair = java.util.Map.Entry[Key0, Value0]
        implicit val Pair: Type[Pair] = Entry[Key0, Value0](keyType, valueType)

        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters
              .asScala(
                Expr.splice(value).asInstanceOf[java.util.Map[Key0, Value0]].entrySet().iterator()
              )
              .to(Iterable)
          }
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Pair, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Pair, A] {
              override def newBuilder: scala.collection.mutable.Builder[Pair, A] =
                new scala.collection.mutable.Builder[Pair, A] {
                  @SuppressWarnings(Array("unchecked"))
                  private val impl = new java.util.EnumMap[Nothing, Value0](
                    Expr.splice(keyClassExpr).asInstanceOf[java.lang.Class[Nothing]]
                  ).asInstanceOf[java.util.Map[Key0, Value0]]
                  override def clear(): Unit = impl.clear()
                  override def result(): A = impl.asInstanceOf[A]
                  override def addOne(elem: Pair): this.type = {
                    impl.put(elem.getKey(), elem.getValue()); this
                  }
                }
              override def fromSpecific(it: IterableOnce[Pair]): A = newBuilder.addAll(it).result()
            }
          }
          @scala.annotation.nowarn
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Pair, CtorResult], A] = {
            implicit val builderType: Type[scala.collection.mutable.Builder[Pair, CtorResult]] =
              Builder[Pair, CtorResult]
            val resultMethod = Method.methodsOf[scala.collection.mutable.Builder[Pair, CtorResult]].collectFirst {
              case Method.OfInstance.Of(m) if m.value.name == "result" && m.value.isNullary =>
                m.value.asReturning.asInstanceOf[Method.Returning[A]]
            }
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Pair, CtorResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              resultMethod
            )
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

      private def isMap[Map0[K, V] <: java.util.Map[K, V], Key0, Value0, A <: Map0[Key0, Value0]](
          A: Type[A],
          Map0: Type.Ctor2[Map0],
          emptyMapExpr: => Expr[A],
          keyType: Type[Key0],
          valueType: Type[Value0]
      ): IsCollection[A] = {
        type Pair = java.util.Map.Entry[Key0, Value0]
        implicit val Pair: Type[Pair] = Entry[Key0, Value0](keyType, valueType)

        Existential[IsCollectionOf[A, *], Pair](new IsMapOf[A, Pair] {
          // We will use scala.jdk.javaapi.CollectionConverters.asScala to convert the map to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Pair]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(value).entrySet().iterator()).to(Iterable)
          }
          // Java maps have no smart constructors, we'll provide a Factory that builds them as plain values.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Pair, CtorResult]] = Expr.quote {
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
          @scala.annotation.nowarn
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Pair, CtorResult], A] = {
            implicit val builderType: Type[scala.collection.mutable.Builder[Pair, CtorResult]] =
              Builder[Pair, CtorResult]
            val resultMethod = Method.methodsOf[scala.collection.mutable.Builder[Pair, CtorResult]].collectFirst {
              case Method.OfInstance.Of(m) if m.value.name == "result" && m.value.isNullary =>
                m.value.asReturning.asInstanceOf[Method.Returning[A]]
            }
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Pair, CtorResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              resultMethod
            )
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

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = {
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
            valueType
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
                .orElse {
                  // EnumMap requires K <: Enum[K] bound which cannot be expressed as a Type.Ctor2.
                  // We check for EnumMap by comparing the runtime class at macro time, and delegate
                  // to isEnumMap helper which uses casts through Nothing for bounds.
                  if (Key <:< juEnumType) {
                    val isEnumMapType = Type.classOfType(using tpe).exists { c =>
                      classOf[java.util.EnumMap[?, ?]].isAssignableFrom(c)
                    }
                    if (isEnumMapType) {
                      Type.classOfType[Key].flatMap { keyClass =>
                        val keyClassExpr: Expr[java.lang.Class[Key]] = {
                          implicit val classCodec: ExprCodec[java.lang.Class[Key]] =
                            Expr.ClassExprCodec[Key]
                          Expr(keyClass)
                        }
                        Some(
                          isEnumMap[Key, Value, A](tpe, Type[Key], Type[Value], keyClassExpr)
                            .asInstanceOf[IsCollection[A]]
                        )
                      }
                    } else None
                  } else None
                }
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

            (classHierarchy orElse interfaceHierarchy) match {
              case Some(result) => ProviderResult.Matched(result)
              case None => skipped(s"${tpe.prettyPrint} is <: java.util.Map[_, _] but no matching concrete type found")
            }
          case _ => skipped(s"${tpe.prettyPrint} is not <: java.util.Map[_, _]")
        }
      }
    })
  }
}
