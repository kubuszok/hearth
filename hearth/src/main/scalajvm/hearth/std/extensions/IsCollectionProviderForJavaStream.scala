package hearth
package std
package extensions

/** Macro extension providing support for Java streams.
  *
  * Supports all Java [[java.util.stream.Stream]] types including primitive specializations. Converts them to
  * [[scala.collection.Iterable]] using [[scala.jdk.javaapi.StreamConverters.asScala]], and providing as
  * [[scala.collection.Factory]] implementation. Treats them as types without smart constructors.
  *
  * Note: Streams are consumed when converted to Iterable, so they can only be used once.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaStream extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val juStream = Type.Ctor1.of[java.util.stream.Stream]
      private lazy val juIntStream = Type.of[java.util.stream.IntStream]
      private lazy val juLongStream = Type.of[java.util.stream.LongStream]
      private lazy val juDoubleStream = Type.of[java.util.stream.DoubleStream]

      private lazy val Int = Type.of[Int]
      private lazy val Long = Type.of[Long]
      private lazy val Double = Type.of[Double]

      // TODO: we have a bug in Type.Ctor:
      // When I do Type.Ctor2.of[scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo]
      // and then apply it, it is printed as:
      //   scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo[Item, Iterable[Item]]
      // BUT
      //   it's resolved in implicit resolution as:
      //     scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo.noAccumulatorFactoryInfo[Item, Type.Ctor1.Apply[Any, Nothing, Iterable, Item]]
      // for some reason Type.Ctor1.Apply is not dealiased, I've spent several hours on that with no progress, so for now I am making a workaround.
      private def accumulatorFactoryInfo[Item: Type]
          : Type[scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo[Item, Iterable[Item]]] =
        Type.of[scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo[Item, Iterable[Item]]]

      private def isStream[A, Item: Type](
          A: Type[A],
          toStreamExpr: Expr[A] => Expr[java.util.stream.Stream[Item]],
          accumulatorFactoryInfoExpr: Expr[
            scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo[Item, Iterable[Item]]
          ]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Item](new IsCollectionOf[A, Item] {
          // TODO: Investigate why the resolved implicit it's printed as:
          //   (StreamExtensions.this.AccumulatorFactoryInfo.noAccumulatorFactoryInfo[java.lang.String, scala.collection.Iterable[java.lang.String]])
          // when it should be:
          //   (scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo....).
          override def asIterable(value: Expr[A]): Expr[Iterable[Item]] = Expr.quote {
            new scala.jdk.StreamConverters.StreamHasToScala(Expr.splice(toStreamExpr(value)))
              .toScala(Iterable)(using Expr.splice(accumulatorFactoryInfoExpr))
          }
          // Java streams have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override val factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Item, A] {
              override def newBuilder: scala.collection.mutable.Builder[Item, A] =
                new scala.collection.mutable.Builder[Item, A] {
                  private var impl = java.util.stream.Stream.builder[Item]()
                  override def clear(): Unit = impl = java.util.stream.Stream.builder[Item]()
                  override def result(): A = impl.build().asInstanceOf[A]
                  override def addOne(elem: Item): this.type = { impl.add(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Item]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Item, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })

      private def isIntStream[A](
          A: Type[A],
          toIntStreamExpr: Expr[A] => Expr[java.util.stream.IntStream],
          accumulatorFactoryInfoExpr: Expr[
            scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo[Int, Iterable[Int]]
          ]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Int](new IsCollectionOf[A, Int] {
          // We will use scala.jdk.StreamConverters.IntStreamHasToScala to convert the stream to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Int]] = Expr.quote {
            new scala.jdk.StreamConverters.IntStreamHasToScala(Expr.splice(toIntStreamExpr(value)))
              .toScala(Iterable)(using Expr.splice(accumulatorFactoryInfoExpr))
          }
          // Java int streams have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          // TODO:
          // Without `this.` on Scala 2 we're getting code like $newBuilder, $cast, $impl - which does not compile.
          // We should make a proper fix to printing this types.
          override val factory: Expr[scala.collection.Factory[Int, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Int, A] {
              override def newBuilder: scala.collection.mutable.Builder[Int, A] =
                new scala.collection.mutable.Builder[Int, A] {
                  private var impl = java.util.stream.IntStream.builder()
                  override def clear(): Unit = impl = java.util.stream.IntStream.builder()
                  override def result(): A = impl.build().asInstanceOf[A]
                  override def addOne(elem: Int): this.type = { impl.add(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Int]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Int, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Int, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })(using Int)

      private def isLongStream[A](
          A: Type[A],
          toLongStreamExpr: Expr[A] => Expr[java.util.stream.LongStream],
          accumulatorFactoryInfoExpr: Expr[
            scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo[Long, Iterable[Long]]
          ]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Long](new IsCollectionOf[A, Long] {
          // We will use scala.jdk.StreamConverters.LongStreamHasToScala to convert the stream to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Long]] = Expr.quote {
            new scala.jdk.StreamConverters.LongStreamHasToScala(Expr.splice(toLongStreamExpr(value)))
              .toScala(Iterable)(using Expr.splice(accumulatorFactoryInfoExpr))
          }
          // Java long streams have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override val factory: Expr[scala.collection.Factory[Long, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Long, A] {
              override def newBuilder: scala.collection.mutable.Builder[Long, A] =
                new scala.collection.mutable.Builder[Long, A] {
                  private var impl = java.util.stream.LongStream.builder()
                  override def clear(): Unit = impl = java.util.stream.LongStream.builder()
                  override def result(): A = impl.build().asInstanceOf[A]
                  override def addOne(elem: Long): this.type = { impl.add(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Long]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Long, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Long, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })(using Long)

      private def isDoubleStream[A](
          A: Type[A],
          toDoubleStreamExpr: Expr[A] => Expr[java.util.stream.DoubleStream],
          accumulatorFactoryInfoExpr: Expr[
            scala.collection.convert.StreamExtensions.AccumulatorFactoryInfo[Double, Iterable[Double]]
          ]
      ): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Double](new IsCollectionOf[A, Double] {
          // We will use scala.jdk.StreamConverters.DoubleStreamHasToScala to convert the stream to Iterable.
          override def asIterable(value: Expr[A]): Expr[Iterable[Double]] = Expr.quote {
            new scala.jdk.StreamConverters.DoubleStreamHasToScala(Expr.splice(toDoubleStreamExpr(value)))
              .toScala(Iterable)(using Expr.splice(accumulatorFactoryInfoExpr))
          }
          // Java double streams have no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override val factory: Expr[scala.collection.Factory[Double, PossibleSmartResult]] = Expr.quote {
            new scala.collection.Factory[Double, A] {
              override def newBuilder: scala.collection.mutable.Builder[Double, A] =
                new scala.collection.mutable.Builder[Double, A] {
                  private var impl = java.util.stream.DoubleStream.builder()
                  override def clear(): Unit = impl = java.util.stream.DoubleStream.builder()
                  override def result(): A = impl.build().asInstanceOf[A]
                  override def addOne(elem: Double): this.type = { impl.add(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Double]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: PossibleSmartCtor[scala.collection.mutable.Builder[Double, PossibleSmartResult], A] =
            PossibleSmartCtor.PlainValue { (expr: Expr[scala.collection.mutable.Builder[Double, PossibleSmartResult]]) =>
              Expr.quote(Expr.splice(expr).result())
            }
        })(using Double)

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        case _ if tpe =:= juIntStream =>
          implicit val A: Type[A] = tpe
          Expr.summonImplicit(using accumulatorFactoryInfo(using Int)).toOption.map { accumulatorFactoryInfoExpr =>
            implicit val IntStream: Type[java.util.stream.IntStream] = juIntStream
            isIntStream[A](A, _.upcast[java.util.stream.IntStream], accumulatorFactoryInfoExpr)
          }
        case _ if tpe =:= juLongStream =>
          implicit val A: Type[A] = tpe
          Expr.summonImplicit(using accumulatorFactoryInfo(using Long)).toOption.map { accumulatorFactoryInfoExpr =>
            implicit val LongStream: Type[java.util.stream.LongStream] = juLongStream
            isLongStream[A](A, _.upcast[java.util.stream.LongStream], accumulatorFactoryInfoExpr)
          }
        case _ if tpe =:= juDoubleStream =>
          implicit val A: Type[A] = tpe
          Expr.summonImplicit(using accumulatorFactoryInfo(using Double)).toOption.map { accumulatorFactoryInfoExpr =>
            implicit val DoubleStream: Type[java.util.stream.DoubleStream] = juDoubleStream
            isDoubleStream[A](A, _.upcast[java.util.stream.DoubleStream], accumulatorFactoryInfoExpr)
          }
        case juStream(item) =>
          import item.Underlying as Item
          implicit val A: Type[A] = tpe
          Expr.summonImplicit(using accumulatorFactoryInfo[Item]).toOption.map { accumulatorFactoryInfoExpr =>
            implicit val StreamItem: Type[java.util.stream.Stream[Item]] = juStream[Item]
            isStream[A, Item](A, _.upcast[java.util.stream.Stream[Item]], accumulatorFactoryInfoExpr)
          }
        case _ => None
      }
    })
  }
}
