package hearth
package std
package extensions

/** Macro extension providing support for Java collections.
  *
  * Supports all Java built-in collections, turns them into [[scala.collection.Iterable]] using
  * [[scala.jdk.javaapi.CollectionConverters.asScala]], and providing as [[scala.collection.Factory]] implementation.
  * Treats them as types without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaCollection extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val Ordering = Type.Ctor1.of[Ordering]

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
      // private lazy val juEnumSet = Type.Ctor1.UpperBounded.of[java.lang.Enum[?], java.util.EnumSet]

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
          // Java collections have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override val factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private val impl = Expr.splice(emptyCollExpr)
                  override def clear(): Unit = this.impl.clear()
                  override def result(): A = this.impl
                  override def add(elem: Item): Unit = { this.impl.add(elem); () }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = this.newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Item, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = {
        def isCollectionOf[Coll[I] <: java.util.Collection[I], Item: Type, Coll2[I] <: Coll[I]](
            coll: Type.Ctor1[Coll],
            emptyCollExpr: Expr[Coll2[Item]]
        ): Option[IsCollection[A]] =
          isCollection[Coll, Item, Coll[Item]](
            tpe.asInstanceOf[Type[Coll[Item]]],
            coll,
            emptyCollExpr.asInstanceOf[Expr[Coll[Item]]]
          ).asInstanceOf[Option[IsCollection[A]]]

        // format: off
        tpe match {
          case juCollection(item) =>
            import item.Underlying as Item

            implicit lazy val OrderingItem: Type[Ordering[Item]] = Ordering[Item]
            lazy val orderingExprOpt = Expr.summonImplicit[Ordering[Item]].toOption

            tpe match {
              case juAbstractCollection(item) => tpe match {
                case juList(item) => tpe match {
                  case juAbstractList(_) => tpe match {
                    case juAbstractSequentialList(_) => tpe match {
                      case juLinkedList(_) => isCollectionOf(juLinkedList, Expr.quote(new java.util.LinkedList[Item]))
                      // handle remaining java.util.AbstractSequentialLists as java.util.LinkedList
                      case _ => isCollectionOf(juAbstractSequentialList, Expr.quote(new java.util.LinkedList[Item]))
                    }
                    case juArrayList(_) => isCollectionOf(juArrayList, Expr.quote(new java.util.ArrayList[Item]))
                    case juVector(_) => tpe match {
                      case juStack(_) => isCollectionOf(juStack, Expr.quote(new java.util.Stack[Item]))
                      // handle remaining java.util.Vectors as java.util.Vector
                      case _ => isCollectionOf(juVector, Expr.quote(new java.util.Vector[Item]))
                    }
                    // handle remaining java.util.AbstractLists as java.util.ArrayList
                    case _ => isCollectionOf(juAbstractList, Expr.quote(new java.util.ArrayList[Item]))
                  }
                  // handle remaining java.util.Lists as java.util.AbstractList as java.util.ArrayList
                  case _ => isCollectionOf(juList, Expr.quote(new java.util.ArrayList[Item]))
                }
                case juDeque(item) => tpe match {
                  case juArrayDeque(_) => isCollectionOf(juArrayDeque, Expr.quote(new java.util.ArrayDeque[Item]))
                  // Commented out - a valid case but this branch would be unreachable because of the previous cases
                  // case juLinkedList(_) => isCollectionOf(juLinkedList, Expr.quote(new java.util.LinkedList[Item]))
                  // handle remaining java.util.Deques as java.util.ArrayDeque
                  case _ => isCollectionOf(juDeque, Expr.quote(new java.util.ArrayDeque[Item]))
                }
                case juQueue(item) => tpe match {
                  case juAbstractQueue(_) => tpe match {
                    case juPriorityQueue(_) if orderingExprOpt.isDefined =>
                      val orderingExpr = orderingExprOpt.get
                      isCollectionOf(juPriorityQueue, Expr.quote(new java.util.PriorityQueue[Item](Expr.splice(orderingExpr))))
                    // handle remaining java.util.AbstractQueues as java.util.PriorityQueue (with natural ordering)
                    case _ => isCollectionOf(juAbstractQueue, Expr.quote(new java.util.PriorityQueue[Item]()))
                  }
                  case juDeque(_) => tpe match {
                    case juArrayDeque(_) => isCollectionOf(juArrayDeque, Expr.quote(new java.util.ArrayDeque[Item]))
                    // Commented out - a valid case but this branch would be unreachable because of the previous cases
                    // case juLinkedList(_) => isCollectionOf(juLinkedList, Expr.quote(new java.util.LinkedList[Item]))
                    // handle remaining java.util.Deques as java.util.ArrayDeque
                    case _ => isCollectionOf(juDeque, Expr.quote(new java.util.ArrayDeque[Item]))
                  }
                  // handle remaining java.util.Queues as java.util.AbstractQueue as java.util.PriorityQueue
                  case _ => isCollectionOf(juQueue, Expr.quote(new java.util.PriorityQueue[Item]()))
                }
                case juSet(item) => tpe match {
                  case juAbstractSet(_) => tpe match {
                    case juHashSet(_) => tpe match {
                      case juLinkedHashSet(_) => isCollectionOf(juLinkedHashSet, Expr.quote(new java.util.LinkedHashSet[Item]))
                      // handle remaining java.util.HashSets as java.util.HashSet
                      case _ => isCollectionOf(juHashSet, Expr.quote(new java.util.HashSet[Item]))
                    }
                    case juSortedSet(_) if orderingExprOpt.isDefined =>
                      val orderingExpr = orderingExprOpt.get
                      tpe match {
                        case juNavigableSet(_) => tpe match {
                          case juTreeSet(_) => isCollectionOf(juTreeSet, Expr.quote(new java.util.TreeSet[Item](Expr.splice(orderingExpr))))
                          // handle remaining java.util.NavigableSets as java.util.TreeSet
                          case _ => isCollectionOf(juNavigableSet, Expr.quote(new java.util.TreeSet[Item](Expr.splice(orderingExpr))))
                        }
                        // handle remaining java.util.SortedSets as java.util.NavigableSet as java.util.TreeSet
                        case _ => isCollectionOf(juNavigableSet, Expr.quote(new java.util.TreeSet[Item](Expr.splice(orderingExpr))))
                      }
                    // TODO: handle java.util.EnumSets
                    // handle remaining java.util.AbstractSets as java.util.HashSet
                    case _ => isCollectionOf(juAbstractSet, Expr.quote(new java.util.HashSet[Item]))
                  }
                  // handle remaining java.util.Sets as java.util.AbstractSet as java.util.HashSet
                  case _ => isCollectionOf(juSet, Expr.quote(new java.util.HashSet[Item]))
                }
                // handle remaining java.util.AbstractCollections as java.util.ArrayList
                case _ => isCollectionOf(juAbstractCollection, Expr.quote(new java.util.ArrayList[Item]))
              }
              // handle remaining java.util.Collections as java.util.AbstractCollection as java.util.ArrayList
              case _ => isCollectionOf(juCollection, Expr.quote(new java.util.ArrayList[Item]))
            }
          case _ => None
        }
        // format: on
      }
    })
  }
}
