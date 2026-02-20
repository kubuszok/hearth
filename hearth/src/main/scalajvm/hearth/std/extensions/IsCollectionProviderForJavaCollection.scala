package hearth
package std
package extensions

/** Macro extension providing support for Java collections.
  *
  * Supports all Java built-in collections, turns them into [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and provides a [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaCollection extends StandardMacroExtension { loader =>

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val Ordering = Type.Ctor1.of[Ordering]
      private lazy val Builder = Type.Ctor2.of[scala.collection.mutable.Builder]

      private lazy val juCollection = Type.Ctor1.of[java.util.Collection]
      private lazy val juAbstractCollection = Type.Ctor1.of[java.util.AbstractCollection]

      private lazy val juList = Type.Ctor1.of[java.util.List]
      private lazy val juAbstractList = Type.Ctor1.of[java.util.AbstractList]
      private lazy val juAbstractSequentialList = Type.Ctor1.of[java.util.AbstractSequentialList]
      private lazy val juArrayList = Type.Ctor1.of[java.util.ArrayList]
      private lazy val juLinkedList = Type.Ctor1.of[java.util.LinkedList]
      private lazy val juVector = Type.Ctor1.of[java.util.Vector]
      private lazy val juStack = Type.Ctor1.of[java.util.Stack]

      private lazy val juDeque = Type.Ctor1.of[java.util.Deque]
      private lazy val juArrayDeque = Type.Ctor1.of[java.util.ArrayDeque]

      private lazy val juQueue = Type.Ctor1.of[java.util.Queue]
      private lazy val juAbstractQueue = Type.Ctor1.of[java.util.AbstractQueue]
      private lazy val juPriorityQueue = Type.Ctor1.of[java.util.PriorityQueue]

      private lazy val juSet = Type.Ctor1.of[java.util.Set]
      private lazy val juAbstractSet = Type.Ctor1.of[java.util.AbstractSet]
      private lazy val juHashSet = Type.Ctor1.of[java.util.HashSet]
      private lazy val juLinkedHashSet = Type.Ctor1.of[java.util.LinkedHashSet]
      private lazy val juSortedSet = Type.Ctor1.of[java.util.SortedSet]
      private lazy val juNavigableSet = Type.Ctor1.of[java.util.NavigableSet]
      private lazy val juTreeSet = Type.Ctor1.of[java.util.TreeSet]
      private lazy val juEnumType = Type.of[java.lang.Enum[?]]

      private def isCollection[Coll0[I] <: java.util.Collection[I], Item: Type, A <: Coll0[Item]](
          A: Type[A],
          Coll0: Type.Ctor1[Coll0],
          emptyCollExpr: Expr[A]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // We will use scala.jdk.javaapi.CollectionConverters.asScala to convert the collection to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            scala.jdk.javaapi.CollectionConverters.asScala(Expr.splice(value).iterator()).to(Iterable)
          }
          // Java collections have no smart constructors, we'll provide a Factory that builds them as plain values.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Item, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = Expr.splice(emptyCollExpr)
                  override def clear(): Unit = impl.clear()
                  override def result(): A = impl
                  override def addOne(elem: Item): this.type = { impl.add(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = newBuilder.addAll(it).result()
            }
          }
          @scala.annotation.nowarn
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], A] = {
            implicit val builderType: Type[scala.collection.mutable.Builder[Item, CtorResult]] =
              Builder[Item, CtorResult]
            val resultMethod = Method.methodsOf[scala.collection.mutable.Builder[Item, CtorResult]].collectFirst {
              case Method.OfInstance.Of(m) if m.value.name == "result" && m.value.isNullary =>
                m.value.asReturning.asInstanceOf[Method.Returning[A]]
            }
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Item, CtorResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              resultMethod
            )
          }
        })

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = {
        def isCollectionOf[Coll[I] <: java.util.Collection[I], Item: Type, Coll2[I] <: Coll[I]](
            coll: Type.Ctor1[Coll],
            emptyCollExpr: Expr[Coll2[Item]]
        ): Option[IsCollection[A]] =
          Some(
            isCollection[Coll, Item, Coll[Item]](
              tpe.asInstanceOf[Type[Coll[Item]]],
              coll,
              emptyCollExpr.asInstanceOf[Expr[Coll[Item]]]
            ).asInstanceOf[IsCollection[A]]
          )

        // tpe is handled by one of Coll's subclasses OR it's exactly Coll[Item]
        // so we can safely provide any implementation. Or we yield.
        def node[Coll[I] <: java.util.Collection[I], Coll2[I] <: Coll[I], Item: Type](
            ctor: Type.Ctor1[Coll],
            emptyExpr: => Expr[Coll2[Item]]
        )(
            forSubtype: => Option[IsCollection[A]]
        ): Option[IsCollection[A]] = {
          val coll = ctor[Item]
          if (tpe <:< coll) {
            forSubtype orElse {
              if (tpe =:= coll) isCollectionOf(ctor, emptyExpr)
              else None
            }
          } else None
        }
        // tpe is exactly Coll[Item], so we can provide a specific implementation. Or we yield.
        def leaf[Coll[I] <: java.util.Collection[I], Coll2[I] <: Coll[I], Item: Type](
            ctor: Type.Ctor1[Coll],
            emptyExpr: => Expr[Coll2[Item]]
        ): Option[IsCollection[A]] =
          if (tpe =:= ctor[Item]) isCollectionOf(ctor, emptyExpr)
          else None

        tpe match {
          case juCollection(item) =>
            import item.Underlying as Item

            implicit lazy val OrderingItem: Type[Ordering[Item]] = Ordering[Item]
            lazy val orderingExprOpt = Expr.summonImplicit[Ordering[Item]].toOption

            // based on https://docs.oracle.com/javase/8/docs/api/java/util/package-tree.html
            def classHierarchy = node(juAbstractCollection, Expr.quote(new java.util.ArrayList[Item])) {
              node(juAbstractList, Expr.quote(new java.util.ArrayList[Item])) {
                node(juAbstractSequentialList, Expr.quote(new java.util.LinkedList[Item])) {
                  leaf(juLinkedList, Expr.quote(new java.util.LinkedList[Item]))
                }
                  .orElse(leaf(juArrayList, Expr.quote(new java.util.ArrayList[Item])))
                  .orElse(node(juVector, Expr.quote(new java.util.Vector[Item])) {
                    leaf(juStack, Expr.quote(new java.util.Stack[Item]))
                  })
              }
                .orElse(node(juAbstractQueue, Expr.quote(new java.util.PriorityQueue[Item]())) {
                  orderingExprOpt
                    .flatMap { orderingExpr =>
                      leaf(juPriorityQueue, Expr.quote(new java.util.PriorityQueue[Item](Expr.splice(orderingExpr))))
                    }
                    .orElse(leaf(juPriorityQueue, Expr.quote(new java.util.PriorityQueue[Item]())))
                })
                .orElse(node(juAbstractSet, Expr.quote(new java.util.HashSet[Item])) {
                  node(juHashSet, Expr.quote(new java.util.HashSet[Item])) {
                    leaf(juLinkedHashSet, Expr.quote(new java.util.LinkedHashSet[Item]))
                  }
                    .orElse(orderingExprOpt.flatMap { orderingExpr =>
                      leaf(juTreeSet, Expr.quote(new java.util.TreeSet[Item](Expr.splice(orderingExpr))))
                    })
                    .orElse {
                      // EnumSet requires E <: Enum[E] bound which cannot be expressed as a Type.Ctor1.
                      // We check for EnumSet by comparing the runtime class at macro time, and use
                      // Type.classOfType + casts through Nothing to satisfy bounds in generated code.
                      if (Item <:< juEnumType) {
                        val isEnumSet = Type.classOfType(using tpe).exists { c =>
                          classOf[java.util.EnumSet[?]].isAssignableFrom(c)
                        }
                        if (isEnumSet) {
                          Type.classOfType[Item].flatMap { enumClass =>
                            val enumClassExpr: Expr[java.lang.Class[Item]] = {
                              implicit val classCodec: ExprCodec[java.lang.Class[Item]] =
                                Expr.ClassExprCodec[Item]
                              Expr(enumClass)
                            }
                            Some(
                              Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
                                override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
                                  scala.jdk.javaapi.CollectionConverters
                                    .asScala(
                                      Expr.splice(value).asInstanceOf[java.util.Set[Item]].iterator()
                                    )
                                    .to(Iterable)
                                }
                                override type CtorResult = A
                                implicit override val CtorResult: Type[CtorResult] = tpe
                                override def factory: Expr[scala.collection.Factory[Item, CtorResult]] =
                                  Expr.quote {
                                    new scala.collection.Factory[Item, A] {
                                      override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                                        new scala.collection.mutable.Builder[Item, A] {
                                          @SuppressWarnings(Array("unchecked"))
                                          private val impl: java.util.EnumSet[?] = {
                                            // Bypass EnumSet.noneOf type bound (E <: Enum[E]) via reflection:
                                            // Scala 2 cannot infer E for the static method call due to
                                            // F-bounded polymorphism + Class invariance.
                                            val cls: java.lang.Class[?] = Expr.splice(enumClassExpr)
                                            classOf[java.util.EnumSet[?]]
                                              .getMethod("noneOf", classOf[java.lang.Class[?]])
                                              .invoke(null, cls)
                                              .asInstanceOf[java.util.EnumSet[?]]
                                          }
                                          override def clear(): Unit = impl.clear()
                                          override def result(): A = impl.asInstanceOf[A]
                                          override def addOne(elem: Item): this.type = {
                                            impl.asInstanceOf[java.util.Set[AnyRef]].add(elem.asInstanceOf[AnyRef]);
                                            this
                                          }
                                        }
                                      override def fromSpecific(it: IterableOnce[Item]): A =
                                        newBuilder.addAll(it).result()
                                    }
                                  }
                                override def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], A] =
                                  CtorLikeOf.PlainValue(
                                    (expr: Expr[scala.collection.mutable.Builder[Item, CtorResult]]) =>
                                      Expr.quote(Expr.splice(expr).result()),
                                    None
                                  )
                              })
                            )
                          }
                        } else None
                      } else None
                    }
                })
                .orElse(leaf(juArrayDeque, Expr.quote(new java.util.ArrayDeque[Item])))
            }
            def interfaceHierarchy = node(juCollection, Expr.quote(new java.util.ArrayList[Item])) {
              leaf(juList, Expr.quote(new java.util.LinkedList[Item]))
                .orElse(node(juQueue, Expr.quote(new java.util.PriorityQueue[Item]())) {
                  orderingExprOpt
                    .flatMap { orderingExpr =>
                      leaf(juPriorityQueue, Expr.quote(new java.util.PriorityQueue[Item](Expr.splice(orderingExpr))))
                    }
                    .orElse(leaf(juPriorityQueue, Expr.quote(new java.util.PriorityQueue[Item]())))
                    .orElse(leaf(juDeque, Expr.quote(new java.util.ArrayDeque[Item])))
                })
                .orElse(node(juSet, Expr.quote(new java.util.HashSet[Item])) {
                  node(juSortedSet, Expr.quote(new java.util.TreeSet[Item])) {
                    orderingExprOpt
                      .flatMap { orderingExpr =>
                        leaf(juNavigableSet, Expr.quote(new java.util.TreeSet[Item](Expr.splice(orderingExpr))))
                      }
                      .orElse(leaf(juTreeSet, Expr.quote(new java.util.TreeSet[Item]())))
                  }
                })
            }

            (classHierarchy orElse interfaceHierarchy) match {
              case Some(result) => ProviderResult.Matched(result)
              case None         =>
                skipped(s"${tpe.prettyPrint} is <: java.util.Collection[_] but no matching concrete type found")
            }
          case _ => skipped(s"${tpe.prettyPrint} is not <: java.util.Collection[_]")
        }
      }
    })
  }
}
