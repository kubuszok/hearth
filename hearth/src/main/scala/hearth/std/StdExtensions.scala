package hearth
package std

import hearth.fp.data.NonEmptyVector

trait StdExtensions { this: MacroCommons =>

  /** Represents a possible smart constructor for the given input and output types.
    *
    * Comes with a set of predefined possible smart constructors for common types:
    *   - no smart constructor (just plain value)
    *   - either string or value
    *   - either iterable of strings or value
    *   - either throwable or value
    *   - either iterable of throwables or value
    *
    * It should make it possible to handle most common cases, while also allowing the library's creators to e.g. handle
    * Validated[E, A] or other result types.
    *
    * @tparam Input
    *   the type of the input
    * @tparam Output
    *   the type of the output
    *
    * @since 0.3.0
    */
  trait PossibleSmartCtor[Input, Output] {

    type Result[A]
    val Result: Type.Ctor1[Result]

    val ctor: Expr[Input] => Expr[Result[Output]]

    def apply(input: Expr[Input]): Expr[Result[Output]] = ctor(input)
  }
  object PossibleSmartCtor {

    final case class PlainValue[Input, Output](
        ctor: Expr[Input] => Expr[Output]
    ) extends PossibleSmartCtor[Input, Output] {

      override type Result[A] = A
      override val Result: Type.Ctor1[Result] = new Type.Ctor1[Result] {
        def apply[A: Type]: Type[A] = Type[A]
        def unapply[A](A: Type[A]): Option[??] = Some(A.as_??)
      }

      override def toString: String = "PlainValue"
    }

    final case class EitherStringOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[String, Output]]
    ) extends PossibleSmartCtor[Input, Output] {

      override type Result[A] = Either[String, A]
      override val Result: Type.Ctor1[Result] =
        Type.Ctor2.of[Either].setA[String](using Type.of[String])

      override def toString: String = "EitherStringOrValue"
    }

    final case class EitherIterableStringOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[Iterable[String], Output]]
    ) extends PossibleSmartCtor[Input, Output] {

      override type Result[A] = Either[Iterable[String], A]
      override val Result: Type.Ctor1[Result] =
        Type.Ctor2.of[Either].setA[Iterable[String]](using Type.of[Iterable[String]])
    }

    final case class EitherThrowableOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[Throwable, Output]]
    ) extends PossibleSmartCtor[Input, Output] {

      override type Result[A] = Either[Throwable, A]
      override val Result: Type.Ctor1[Result] =
        Type.Ctor2.of[Either].setA[Throwable](using Type.of[Throwable])
    }

    final case class EitherIterableThrowableOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[Iterable[Throwable], Output]]
    ) extends PossibleSmartCtor[Input, Output] {

      override type Result[A] = Either[Iterable[Throwable], A]
      override val Result: Type.Ctor1[Result] =
        Type.Ctor2.of[Either].setA[Iterable[Throwable]](using Type.of[Iterable[Throwable]])
    }
  }

  // TODO: provide extension methods
  // TODO: provide a method to load the extensions from the classpath?

  /** Proof that the type is a collection of the given item type.
    *
    * Proof needs to provide a way to build the collection from its items, and to iterate over its items.
    *
    * Intended to both:
    *   - handle all built-in collections, Arrays, IArrays, etc with a single interface
    *   - make it possible to extend the support for custom collections coming from other libraries just by providing a
    *     std extension for macro, that would be loaded from the classpath
    *
    * @tparam CollA
    *   the type of the collection with applied item type
    * @tparam Item
    *   the type of the item
    *
    * @since 0.3.0
    */
  trait IsCollectionOf[CollA, Item] {

    type Coll[A0]
    val Coll: Type.Ctor1[Coll]

    def asIterable(value: Expr[CollA]): Expr[Iterable[Item]]

    type PossibleSmartResult
    implicit val PossibleSmartResult: Type[PossibleSmartResult]

    def factory: Expr[scala.collection.Factory[Item, PossibleSmartResult]]

    def build: PossibleSmartCtor[scala.collection.mutable.Builder[Item, PossibleSmartResult], CollA]
  }

  /** An alias indicating the the type is a collection of some item type, but the exact item type is an existential
    * type.
    *
    * @tparam A
    *   the type of the collection
    *
    * @since 0.3.0
    */
  type IsCollection[A] = Existential[IsCollectionOf[A, *]]
  object IsCollection {
    private val providers = scala.collection.mutable.ListBuffer[Provider]()

    trait Provider {

      def unapply[A](tpe: Type[A]): Option[IsCollection[A]]
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def unapply[A](tpe: Type[A]): Option[IsCollection[A]] =
      providers.view.map(_.unapply(tpe)).collectFirst { case Some(collection) => collection }

    // TODO: provide a support for built-in collections, Arrays, IArrays, etc
  }

  /** Proof that the type is an option of the given item type.
    *
    * Proof needs to provide a way to build the option from its item, and to fold over it.
    *
    * Intended to both:
    *   - handle all built-in options, java's Optional, etc with a single interface
    *   - make it possible to extend the support for custom options coming from other libraries just by providing a std
    *     extension for macro, that would be loaded from the classpath
    *
    * @tparam OptionA
    *   the type of the option with applied item type
    * @tparam Item
    *   the type of the item
    *
    * @since 0.3.0
    */
  trait IsOptionOf[OptionA, Item] {

    def empty: Expr[OptionA]

    def of(value: Expr[Item]): Expr[OptionA]

    def fold[A](option: Expr[OptionA])(onEmpty: => Expr[A], onSome: Expr[Item] => Expr[A]): Expr[A]

    def getOrElse(option: Expr[OptionA])(default: Expr[Item]): Expr[Item]

    def orElse(option: Expr[OptionA])(default: Expr[OptionA]): Expr[OptionA]
  }

  /** An alias indicating the the type is an option of some item type, but the exact item type is an existential type.
    *
    * @tparam A
    *   the type of the option
    *
    * @since 0.3.0
    */
  type IsOption[A] = Existential[IsOptionOf[A, *]]
  object IsOption {
    private val providers = scala.collection.mutable.ListBuffer[Provider]()

    trait Provider {

      def unapply[A](tpe: Type[A]): Option[IsOption[A]]
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def unapply[A](tpe: Type[A]): Option[IsOption[A]] =
      providers.view.map(_.unapply(tpe)).collectFirst { case Some(option) => option }

    // TODO: provide a support for built-in options, java's Optional, etc
  }

  /** Proof that the type is a wrapper of the given inner type.
    *
    * Proof needs to provide a way to unwrap the wrapper to its inner type, and to wrap it back from its inner type.
    *
    * Intended to both:
    *   - handle all proper AnyVals (opaque types?) etc with a single interface
    *   - make it possible to extend the support for new type libraries coming from other libraries just by providing a
    *     std extension for macro, that would be loaded from the classpath
    *
    * @tparam Outer
    *   the type of the wrapper
    * @tparam Inner
    *   the type of the inner type
    *
    * @since 0.3.0
    */
  trait IsWrapperOf[Outer, Inner] {

    val unwrap: Expr[Outer] => Expr[Inner]

    val wrap: PossibleSmartCtor[Inner, Outer]
  }

  /** An alias indicating the the type is a wrapper of some inner type, but the exact inner type is an existential type.
    *
    * @tparam A
    *   the type of the wrapper
    *
    * @since 0.3.0
    */
  type IsWrapper[A] = Existential[IsWrapperOf[A, *]]
  object IsWrapper {
    private val providers = scala.collection.mutable.ListBuffer[Provider]()

    trait Provider {

      def unapply[A](tpe: Type[A]): Option[IsWrapper[A]]
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def unapply[A](tpe: Type[A]): Option[IsWrapper[A]] =
      providers.view.map(_.unapply(tpe)).collectFirst { case Some(wrapper) => wrapper }

    // TODO: provide a support for any-vals (and opaque types?)
  }

  implicit final class EnvironmentStdExtensionsOps(private val environment: Environment.type) {

    /** Loads all standard extensions.
      *
      * @since 0.3.0
      */
    def loadStandardExtensions(): Either[NonEmptyVector[Throwable], Unit] =
      environment.loadMacroExtensions[StandardMacroExtension]
  }
}
