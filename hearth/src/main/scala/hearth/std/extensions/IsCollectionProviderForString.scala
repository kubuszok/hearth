package hearth
package std
package extensions

/** Macro extension providing support for [[String]] as a collection of [[Char]].
  *
  * Treats [[String]] as [[IsCollectionOf]][String, Char]. Converts to [[scala.collection.Iterable]] by wrapping in
  * [[scala.collection.immutable.WrappedString]], and provides a [[scala.collection.Factory]] implementation using
  * [[StringBuilder]]. Treats it as a type without smart constructors.
  *
  * @since 0.3.0
  */
final class IsCollectionProviderForString extends StandardMacroExtension {

  @scala.annotation.nowarn
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsCollection.registerProvider(new IsCollection.Provider {

      private lazy val StringType = Type.of[String]
      private lazy val Char = Type.of[Char]

      private def isString[A](A: Type[A]): IsCollection[A] =
        Existential[IsCollectionOf[A, *], Char](new IsCollectionOf[A, Char] {
          override def asIterable(value: Expr[A]): Expr[Iterable[Char]] = Expr.quote {
            new scala.collection.immutable.WrappedString(Expr.splice(value).asInstanceOf[String])
          }
          override type CtorResult = A
          implicit override val CtorResult: Type[CtorResult] = A
          override def factory: Expr[scala.collection.Factory[Char, CtorResult]] = Expr.quote {
            new scala.collection.Factory[Char, A] {
              override def newBuilder: scala.collection.mutable.Builder[Char, A] =
                new scala.collection.mutable.Builder[Char, A] {
                  private val impl = new StringBuilder
                  override def clear(): Unit = impl.clear()
                  override def result(): A = impl.result().asInstanceOf[A]
                  override def addOne(elem: Char): this.type = { impl.addOne(elem); this }
                }
              override def fromSpecific(it: IterableOnce[Char]): A = newBuilder.addAll(it).result()
            }
          }
          override def build: CtorLikeOf[scala.collection.mutable.Builder[Char, CtorResult], A] =
            CtorLikeOf.PlainValue(
              (expr: Expr[scala.collection.mutable.Builder[Char, CtorResult]]) =>
                Expr.quote(Expr.splice(expr).result()),
              None
            )
        })(using Char)

      override def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = tpe match {
        case _ if tpe =:= StringType => Some(isString(tpe))
        case _                       => None
      }
    })
  }
}
