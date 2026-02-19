package hearth
package std

import hearth.fp.data.{NonEmptyList, NonEmptyMap}
import hearth.typed.ImportedCrossTypeImplicit

import scala.collection.immutable.ListMap

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
  trait CtorLikeOf[Input, Output] {

    type Result[A]
    @ImportedCrossTypeImplicit
    val Result: Type.Ctor1[Result]

    val ctor: Expr[Input] => Expr[Result[Output]]
    val method: Option[Method.Returning[Result[Output]]]

    def apply(input: Expr[Input]): Expr[Result[Output]] = ctor(input)
  }
  object CtorLikeOf {

    final case class PlainValue[Input, Output](
        ctor: Expr[Input] => Expr[Output],
        method: Option[Method.Returning[Output]]
    ) extends CtorLikeOf[Input, Output] {

      // $COVERAGE-OFF$
      override type Result[A] = PlainValue.Result[A]
      override val Result: Type.Ctor1[Result] = PlainValue.Result
      override def toString: String = "PlainValue"
      // $COVERAGE-ON$
    }
    object PlainValue {
      type Result[A] = A
      // $COVERAGE-OFF$
      val Result: Type.Ctor1[Result] = new Type.Ctor1[Result] {
        def apply[A: Type]: Type[A] = Type[A]
        def unapply[A](A: Type[A]): Option[??] = Some(A.as_??)
        override def asUntyped: UntypedType = Type.identityCtor1Untyped
      }
      // $COVERAGE-ON$
    }

    final case class EitherStringOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[String, Output]],
        method: Option[Method.Returning[Either[String, Output]]]
    ) extends CtorLikeOf[Input, Output] {

      // $COVERAGE-OFF$
      override type Result[A] = EitherStringOrValue.Result[A]
      override val Result: Type.Ctor1[Result] = EitherStringOrValue.Result
      override def toString: String = "EitherStringOrValue"
      // $COVERAGE-ON$
    }
    object EitherStringOrValue {
      type Result[A] = Either[String, A]
      val Result: Type.Ctor1[Result] = Type.Ctor2.of[Either].setA[String](using Type.of[String])
    }

    final case class EitherIterableStringOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[Iterable[String], Output]],
        method: Option[Method.Returning[Either[Iterable[String], Output]]]
    ) extends CtorLikeOf[Input, Output] {

      // $COVERAGE-OFF$
      override type Result[A] = EitherIterableStringOrValue.Result[A]
      override val Result: Type.Ctor1[Result] = EitherIterableStringOrValue.Result
      override def toString: String = "EitherIterableStringOrValue"
      // $COVERAGE-ON$
    }
    object EitherIterableStringOrValue {
      type Result[A] = Either[Iterable[String], A]
      val Result: Type.Ctor1[Result] = Type.Ctor2.of[Either].setA[Iterable[String]](using Type.of[Iterable[String]])
    }

    final case class EitherThrowableOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[Throwable, Output]],
        method: Option[Method.Returning[Either[Throwable, Output]]]
    ) extends CtorLikeOf[Input, Output] {

      // $COVERAGE-OFF$
      override type Result[A] = EitherThrowableOrValue.Result[A]
      override val Result: Type.Ctor1[Result] = EitherThrowableOrValue.Result
      override def toString: String = "EitherThrowableOrValue"
      // $COVERAGE-ON$
    }
    object EitherThrowableOrValue {
      type Result[A] = Either[Throwable, A]
      val Result: Type.Ctor1[Result] = Type.Ctor2.of[Either].setA[Throwable](using Type.of[Throwable])
    }

    final case class EitherIterableThrowableOrValue[Input, Output](
        ctor: Expr[Input] => Expr[Either[Iterable[Throwable], Output]],
        method: Option[Method.Returning[Either[Iterable[Throwable], Output]]]
    ) extends CtorLikeOf[Input, Output] {

      // $COVERAGE-OFF$
      override type Result[A] = EitherIterableThrowableOrValue.Result[A]
      override val Result: Type.Ctor1[Result] = EitherIterableThrowableOrValue.Result
      override def toString: String = "EitherIterableThrowableOrValue"
      // $COVERAGE-ON$
    }
    object EitherIterableThrowableOrValue {
      type Result[A] = Either[Iterable[Throwable], A]
      val Result: Type.Ctor1[Result] =
        Type.Ctor2.of[Either].setA[Iterable[Throwable]](using Type.of[Iterable[Throwable]])
    }
  }

  type CtorLike[A] = Existential[CtorLikeOf[*, A]]

  type CtorLikes[A] = NonEmptyList[CtorLike[A]]
  object CtorLikes {
    private val providers = scala.collection.mutable.ListBuffer[Provider]()

    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    trait Provider {

      def name: String
      def unapply[A](tpe: Type[A]): ProviderResult[CtorLikes[A]]

      final protected def skipped(reason: String): ProviderResult[Nothing] =
        ProviderResult.skipped(name, reason)
      final protected def failed(error: Throwable): ProviderResult[Nothing] =
        ProviderResult.failed(name, error)
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def parse[A](tpe: Type[A]): ProviderResult[CtorLikes[A]] = {
      var matched: Option[CtorLikes[A]] = None
      var skippedReasons = ListMap.empty[String, Either[Throwable, String]]
      providers.foreach { provider =>
        provider.unapply(tpe) match {
          case ProviderResult.Matched(value) =>
            matched = matched match {
              case Some(existing) => Some(existing ++ value)
              case None           => Some(value)
            }
          case ProviderResult.Skipped(reasons) =>
            skippedReasons ++= reasons.iterator
        }
      }
      matched match {
        case Some(ctorLikes) => ProviderResult.Matched(ctorLikes)
        case None            =>
          NonEmptyMap.fromListMap(skippedReasons) match {
            case Some(nem) => ProviderResult.Skipped(nem)
            case None      => ProviderResult.skipped("CtorLikes", "No providers registered")
          }
      }
    }

    def unapply[A](tpe: Type[A]): Option[CtorLikes[A]] = parse(tpe) match {
      case ProviderResult.Matched(value) =>
        lastUnapplyFailure = null
        Some(value)
      case ProviderResult.Skipped(reasons) =>
        lastUnapplyFailure = reasons
        None
    }

    /** Builder trait for creating CtorLike from an existential input type. Used instead of polymorphic function types
      * for Scala 2 compatibility.
      */
    trait CtorBuilder[Output, Result] {
      def apply[Input: Type](
          ctor: Expr[Input] => Expr[Result],
          method: Method.Returning[Result]
      ): CtorLikeOf[Input, Output]
    }

    /** Extracts from type `Output` all possible smart constructors that can be used to build an instance of `Output`
      * from an instance of `Input`.
      *
      * @tparam Output
      *   the type construct, maybe with a smart constructor
      * @tparam Result
      *   the type of the result
      * @param buildCtor
      *   a function that builds a smart constructor from an input type and a result type
      * @return
      *   a list of existential smart constructors
      */
    @scala.annotation.nowarn
    def extractCtorLikesResult[Output: Type, Result: Type](
        buildCtor: CtorBuilder[Output, Result]
    ): List[Existential[CtorLikeOf[*, Output]]] = {
      // Constructors of the type itself
      val plainCtors: List[Method.NoInstance[?]] =
        if (Type[Output] <:< Type[Result]) Type[Output].constructors.asInstanceOf[List[Method.NoInstance[?]]]
        else Nil

      // NoInstance methods from the type (for case objects, etc.)
      val noInstanceMethods: List[Method.NoInstance[?]] = Type[Output].methods.collect {
        case method if method.value.isInstanceOf[Method.NoInstance[?]] =>
          method.value.asInstanceOf[Method.NoInstance[?]]
      }

      // Process constructors and noInstance methods
      (plainCtors ++ noInstanceMethods).collect {
        case mi: Method.NoInstance[?] if mi.isUnary && mi.Returned <:< Type[Result] =>
          val method = mi.asInstanceOf[Method.NoInstance[Result]]
          val (paramName, param) = mi.parameters.flatten.head
          import param.tpe.Underlying as Input
          def applyMiInput(input: Expr[Input]): Expr[Result] =
            method(Map(paramName -> input.as_??)) match {
              case Right(ex) => ex
              case Left(err) => hearthAssertionFailed(err.toString)
            }
          Existential[CtorLikeOf[*, Output], Input](buildCtor[Input](applyMiInput, method.asReturning))
      }
    }
  }

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

    def asIterable(value: Expr[CollA]): Expr[Iterable[Item]]

    type CtorResult
    @ImportedCrossTypeImplicit
    implicit val CtorResult: Type[CtorResult]

    def factory: Expr[scala.collection.Factory[Item, CtorResult]]

    def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], CollA]
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

    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    trait Provider {

      def name: String
      def unapply[A](tpe: Type[A]): ProviderResult[IsCollection[A]]

      final protected def skipped(reason: String): ProviderResult[Nothing] =
        ProviderResult.skipped(name, reason)
      final protected def failed(error: Throwable): ProviderResult[Nothing] =
        ProviderResult.failed(name, error)
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] = {
      var skippedReasons = ListMap.empty[String, Either[Throwable, String]]
      val it = providers.iterator
      while (it.hasNext) {
        val provider = it.next()
        provider.unapply(tpe) match {
          case matched: ProviderResult.Matched[IsCollection[A] @unchecked] => return matched
          case ProviderResult.Skipped(reasons)                             => skippedReasons ++= reasons.iterator
        }
      }
      NonEmptyMap.fromListMap(skippedReasons) match {
        case Some(nem) => ProviderResult.Skipped(nem)
        case None      => ProviderResult.skipped("IsCollection", "No providers registered")
      }
    }

    def unapply[A](tpe: Type[A]): Option[IsCollection[A]] = parse(tpe) match {
      case ProviderResult.Matched(value) =>
        lastUnapplyFailure = null
        Some(value)
      case ProviderResult.Skipped(reasons) =>
        lastUnapplyFailure = reasons
        None
    }
  }

  /** Proof that the type is a map of the given key and value types.
    *
    * Proof needs to provide a way to build the map from its pairs of keys and values, and to iterate over its pairs.
    *
    * Intended to both:
    *   - handle all built-in Maps with a single interface
    *   - make it possible to extend the support for custom maps coming from other libraries just by providing a std
    *     extension for macro, that would be loaded from the classpath
    *
    * @tparam MapKV
    *   the type of the map with applied key and value type
    * @tparam Pair
    *   the type of the pair of key and value
    *
    * @since 0.3.0
    */
  trait IsMapOf[MapKV, Pair] extends IsCollectionOf[MapKV, Pair] {

    type Key
    @ImportedCrossTypeImplicit
    implicit val Key: Type[Key]

    type Value
    @ImportedCrossTypeImplicit
    implicit val Value: Type[Value]

    def key(pair: Expr[Pair]): Expr[Key]
    def value(pair: Expr[Pair]): Expr[Value]
    def pair(key: Expr[Key], value: Expr[Value]): Expr[Pair]
  }

  /** An alias indicating the the type is a map of some key and value types, but the exact key and value types are an
    * existential type.
    *
    * @tparam A
    *   the type of the map
    *
    * @since 0.3.0
    */
  type IsMap[A] = Existential[IsMapOf[A, *]]
  object IsMap {

    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    def parse[A](tpe: Type[A]): ProviderResult[IsMap[A]] =
      IsCollection.parse(tpe) match {
        case ProviderResult.Matched(isCollection) if isCollection.value.isInstanceOf[IsMapOf[?, ?]] =>
          ProviderResult.Matched(isCollection.asInstanceOf[IsMap[A]])
        case ProviderResult.Matched(_) =>
          ProviderResult.skipped("IsMap", s"${tpe.prettyPrint} is a collection but not a Map")
        case s: ProviderResult.Skipped => s
      }

    def unapply[A](tpe: Type[A]): Option[IsMap[A]] = parse(tpe) match {
      case ProviderResult.Matched(value) =>
        lastUnapplyFailure = null
        Some(value)
      case ProviderResult.Skipped(reasons) =>
        lastUnapplyFailure = reasons
        None
    }
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

    def fold[A: Type](option: Expr[OptionA])(onEmpty: Expr[A], onSome: Expr[Item] => Expr[A]): Expr[A]

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

    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    trait Provider {

      def name: String
      def unapply[A](tpe: Type[A]): ProviderResult[IsOption[A]]

      final protected def skipped(reason: String): ProviderResult[Nothing] =
        ProviderResult.skipped(name, reason)
      final protected def failed(error: Throwable): ProviderResult[Nothing] =
        ProviderResult.failed(name, error)
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def parse[A](tpe: Type[A]): ProviderResult[IsOption[A]] = {
      var skippedReasons = ListMap.empty[String, Either[Throwable, String]]
      val it = providers.iterator
      while (it.hasNext) {
        val provider = it.next()
        provider.unapply(tpe) match {
          case matched: ProviderResult.Matched[IsOption[A] @unchecked] => return matched
          case ProviderResult.Skipped(reasons)                         => skippedReasons ++= reasons.iterator
        }
      }
      NonEmptyMap.fromListMap(skippedReasons) match {
        case Some(nem) => ProviderResult.Skipped(nem)
        case None      => ProviderResult.skipped("IsOption", "No providers registered")
      }
    }

    def unapply[A](tpe: Type[A]): Option[IsOption[A]] = parse(tpe) match {
      case ProviderResult.Matched(value) =>
        lastUnapplyFailure = null
        Some(value)
      case ProviderResult.Skipped(reasons) =>
        lastUnapplyFailure = reasons
        None
    }
  }

  /** Proof that the type is an either of the given left and right types.
    *
    * Proof needs to provide a way to build the either from its left or right, and to fold over it.
    *
    * Intended to both:
    *   - handle all built-in Eithers, Try, etc with a single interface
    *   - make it possible to extend the support for custom eithers coming from other libraries just by providing a std
    *     extension for macro, that would be loaded from the classpath
    *
    * @tparam EitherLR
    *   the type of the either with applied left and right type
    * @tparam LeftValue
    *   the type of the left value
    * @tparam RightValue
    *   the type of the right value
    *
    * @since 0.3.0
    */
  trait IsEitherOf[EitherLR, LeftValue, RightValue] {

    def left(leftValue: Expr[LeftValue]): Expr[EitherLR]

    def right(rightValue: Expr[RightValue]): Expr[EitherLR]

    def fold[A: Type](
        either: Expr[EitherLR]
    )(onLeft: Expr[LeftValue] => Expr[A], onRight: Expr[RightValue] => Expr[A]): Expr[A]

    def getOrElse(either: Expr[EitherLR])(default: Expr[RightValue]): Expr[RightValue]

    def orElse(either: Expr[EitherLR])(default: Expr[EitherLR]): Expr[EitherLR]
  }

  /** Specialization for Existential type for IsEitherOf that provides the left and right types as existential types.
    *
    * @tparam EitherLR
    *   the type of the either with applied left and right type
    *
    * @since 0.3.0
    */
  trait IsEither[EitherLR] {

    type LeftValue
    @ImportedCrossTypeImplicit
    implicit val LeftValue: Type[LeftValue]

    type RightValue
    @ImportedCrossTypeImplicit
    implicit val RightValue: Type[RightValue]

    def value: IsEitherOf[EitherLR, LeftValue, RightValue]
  }
  object IsEither {
    private val providers = scala.collection.mutable.ListBuffer[Provider]()

    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    trait Provider {

      def name: String
      def unapply[A](tpe: Type[A]): ProviderResult[IsEither[A]]

      final protected def skipped(reason: String): ProviderResult[Nothing] =
        ProviderResult.skipped(name, reason)
      final protected def failed(error: Throwable): ProviderResult[Nothing] =
        ProviderResult.failed(name, error)
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def parse[A](tpe: Type[A]): ProviderResult[IsEither[A]] = {
      var skippedReasons = ListMap.empty[String, Either[Throwable, String]]
      val it = providers.iterator
      while (it.hasNext) {
        val provider = it.next()
        provider.unapply(tpe) match {
          case matched: ProviderResult.Matched[IsEither[A] @unchecked] => return matched
          case ProviderResult.Skipped(reasons)                         => skippedReasons ++= reasons.iterator
        }
      }
      NonEmptyMap.fromListMap(skippedReasons) match {
        case Some(nem) => ProviderResult.Skipped(nem)
        case None      => ProviderResult.skipped("IsEither", "No providers registered")
      }
    }

    def unapply[A](tpe: Type[A]): Option[IsEither[A]] = parse(tpe) match {
      case ProviderResult.Matched(value) =>
        lastUnapplyFailure = null
        Some(value)
      case ProviderResult.Skipped(reasons) =>
        lastUnapplyFailure = reasons
        None
    }
  }

  /** Proof that the type is a value type of the given inner type.
    *
    * Proof needs to provide a way to unwrap the value type to its inner type, and to wrap it back from its inner type.
    *
    * Intended to both:
    *   - handle all proper AnyVals (opaque types?) etc with a single interface
    *   - make it possible to extend the support for new type libraries coming from other libraries just by providing a
    *     std extension for macro, that would be loaded from the classpath
    *
    * @tparam Outer
    *   the type of the value type
    * @tparam Inner
    *   the type of the inner type
    *
    * @since 0.3.0
    */
  trait IsValueTypeOf[Outer, Inner] {

    val unwrap: Expr[Outer] => Expr[Inner]

    val wrap: CtorLikeOf[Inner, Outer]

    def ctors: CtorLikes[Outer]
  }

  /** An alias indicating the the type is a value type of some inner type, but the exact inner type is an existential
    * type.
    *
    * @tparam A
    *   the type of the value type
    *
    * @since 0.3.0
    */
  type IsValueType[A] = Existential[IsValueTypeOf[A, *]]
  object IsValueType {
    private val providers = scala.collection.mutable.ListBuffer[Provider]()

    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    trait Provider {

      def name: String
      def unapply[A](tpe: Type[A]): ProviderResult[IsValueType[A]]

      final protected def skipped(reason: String): ProviderResult[Nothing] =
        ProviderResult.skipped(name, reason)
      final protected def failed(error: Throwable): ProviderResult[Nothing] =
        ProviderResult.failed(name, error)
    }

    def registerProvider(provider: Provider): Unit =
      providers += provider

    def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] = {
      var skippedReasons = ListMap.empty[String, Either[Throwable, String]]
      val it = providers.iterator
      while (it.hasNext) {
        val provider = it.next()
        provider.unapply(tpe) match {
          case matched: ProviderResult.Matched[IsValueType[A] @unchecked] => return matched
          case ProviderResult.Skipped(reasons)                            => skippedReasons ++= reasons.iterator
        }
      }
      NonEmptyMap.fromListMap(skippedReasons) match {
        case Some(nem) => ProviderResult.Skipped(nem)
        case None      => ProviderResult.skipped("IsValueType", "No providers registered")
      }
    }

    def unapply[A](tpe: Type[A]): Option[IsValueType[A]] = parse(tpe) match {
      case ProviderResult.Matched(value) =>
        lastUnapplyFailure = null
        Some(value)
      case ProviderResult.Skipped(reasons) =>
        lastUnapplyFailure = reasons
        None
    }
  }

  implicit final class EnvironmentStdExtensionsOps(private val environment: Environment.type) {

    /** Loads all standard extensions.
      *
      * @since 0.3.0
      */
    def loadStandardExtensions(): ExtensionLoadingResult[StandardMacroExtension] =
      environment.loadMacroExtensions[StandardMacroExtension]
  }
}
