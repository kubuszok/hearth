package hearth
package std
package extensions

/** Macro extension providing support for Java BitSet.
  *
  * Supports [[java.util.BitSet]] as a collection of [[Int]] values representing the indices of set bits. Converts it to
  * [[scala.collection.Iterable]] by iterating over set bit indices, and provides a [[scala.collection.Factory]]
  * implementation. Treats it as a type without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForJavaBitSet extends StandardMacroExtension { loader =>

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = loader.getClass.getName

      private lazy val juBitSet = Type.of[java.util.BitSet]
      private lazy val Int = Type.of[Int]
      private lazy val Builder = Type.Ctor2.of[scala.collection.mutable.Builder]

      private def isBitSet[A](A: Type[A]): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Int](new IsCollectionOf[A, Int] {
          // We will use Iterable.unfold to iterate over the set bit indices.
          override def asIterable(value: Expr[A]): Expr[Iterable[Int]] = Expr.quote {
            val bitSet = Expr.splice(value).asInstanceOf[java.util.BitSet]
            Iterable.unfold(bitSet.nextSetBit(0)) { currentValue =>
              if (currentValue < 0) None
              else Some(currentValue, bitSet.nextSetBit(currentValue + 1))
            }
          }
          override def foreach(value: Expr[A])(f: Expr[Int] => Expr[Unit]): Expr[Unit] = Expr.quote {
            val bitSet = Expr.splice(value).asInstanceOf[java.util.BitSet]
            var i = bitSet.nextSetBit(0)
            while (i >= 0) {
              val item = i
              Expr.splice(f(Expr.quote(item)))
              i = bitSet.nextSetBit(i + 1)
            }
          }
          // Java BitSet has no smart constructors, we'll provide a Factory that builds them as plain values.
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Int, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Int, A] {
              override def newBuilder: scala.collection.mutable.Builder[Int, A] =
                new scala.collection.mutable.Builder[Int, A] {
                  private val impl = new java.util.BitSet()
                  override def clear(): Unit = impl.clear()
                  override def result(): A = impl.asInstanceOf[A]
                  override def addOne(elem: Int): this.type = { impl.set(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Int]): A = newBuilder.addAll(it).result()
            }
          }
          @scala.annotation.nowarn
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Int, CtorResult], A] = {
            implicit val intType: Type[Int] = Int
            implicit val builderType: Type[scala.collection.mutable.Builder[Int, CtorResult]] =
              Builder[Int, CtorResult]
            val resultMethod =
              Method.unsortedMethodsNamed[scala.collection.mutable.Builder[Int, CtorResult]]("result").collectFirst {
                case m: Method.OnInstance if m.isNullary => m
              }
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Int, CtorResult]]) => Expr.quote(Expr.splice(expr).result()),
              resultMethod
            )
          }
        })(using Int)

      // Cheap sound negative gate: the provider matches only =:= java.util.BitSet, and =:= implies <:<.
      override def mightMatch[A](tpe: Type[A]): Boolean = tpe <:< juBitSet

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = tpe match {
        case _ if tpe =:= juBitSet => ProviderResult.Matched(isBitSet(tpe))
        case _                     => skipped(s"${tpe.prettyPrint} is not =:= java.util.BitSet")
      }
    })
  }
}
