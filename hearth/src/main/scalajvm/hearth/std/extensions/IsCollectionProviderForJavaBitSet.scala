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
final class IsCollectionProviderForJavaBitSet extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val juBitSet = Type.of[java.util.BitSet]
      private lazy val Int = Type.of[Int]

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
          // Java BitSet has no smart constructors, we we'll provide a Factory that build them as plain values.
          override type PossibleSmartResult = A
          implicit override val PossibleSmartResult: Type[PossibleSmartResult] = A
          override def factory: Expr[scala.collection.Factory[Int, PossibleSmartResult]] = Expr.quote {
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
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Int, PossibleSmartResult], A] =
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Int, PossibleSmartResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              None // TODO: we should provide a method for this
            )
        })(using Int)

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        case _ if tpe =:= juBitSet => Some(isBitSet(tpe))
        case _                     => None
      }
    })
  }
}
