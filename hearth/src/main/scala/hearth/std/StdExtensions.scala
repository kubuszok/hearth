package hearth
package std

import hearth.fp.data.{NonEmptyList, NonEmptyMap}
import hearth.typed.ImportedCrossTypeImplicit

import scala.collection.immutable.ListMap

trait StdExtensions { this: MacroCommons =>

  /** Base trait for companion objects that aggregate results from registered providers.
    *
    * Provides common infrastructure: a mutable list of providers, `registerProvider`, abstract `parse`, concrete
    * `unapply` (for pattern matching) that delegates to `parse` and stores failure reasons in `lastUnapplyFailure`.
    *
    * @since 0.3.0
    *
    * @tparam Provided
    *   the type constructor that providers produce (e.g. `IsCollection`, `IsOption`)
    */
  trait ProvidedCompanion[Provided[_]] {
    protected val providers: scala.collection.mutable.ListBuffer[Provider] =
      scala.collection.mutable.ListBuffer[Provider]()

    /** The aggregated skip/error reasons from the most recent [[unapply]] that did not match (`null` after a match), so
      * a failing pattern match can be diagnosed. Keyed by provider name; see [[ProviderResult.Skipped]].
      *
      * @since 0.3.0
      */
    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    /** A single registered provider: given a `Type[A]`, decides whether it recognises `A` and, if so, produces the
      * `Provided[A]` proof.
      *
      * `parse` must return [[ProviderResult.Matched]] only for a type it can actually build (do not `Matched` a type
      * whose exprs you cannot emit); when declining it should return `skipped(reason)` so the aggregated failure
      * reasons stay informative, and `failed(error)` when it recognised the shape but errored while building.
      *
      * A provider registered against a shape companion (`IsCollection`/`IsOption`/`IsEither`/`IsValueType`/`CtorLikes`)
      * does '''not''' need its own bottom-type guard: those companions short-circuit bottom types (`Nothing`/`Null`)
      * before dispatching to providers (see [[isBottomType]]). A provider invoked directly, or a new engine reusing
      * this pattern, must replicate that guard itself.
      *
      * @since 0.3.0
      */
    trait Provider {

      def name: String
      def parse[A](tpe: Type[A]): ProviderResult[Provided[A]]

      final protected def skipped(reason: String): ProviderResult[Nothing] =
        ProviderResult.skipped(name, reason)
      final protected def failed(error: Throwable): ProviderResult[Nothing] =
        ProviderResult.failed(name, error)
    }

    /** Registers a [[Provider]] with this companion; typically called from a [[StandardMacroExtension.extend]] body so
      * the provider joins the pool tried by [[parse]]. Later registrations are tried after earlier ones.
      *
      * @since 0.3.0
      */
    def registerProvider(provider: Provider): Unit =
      providers += provider

    /** Runs the registered providers against `A` and aggregates the outcome: [[ProviderResult.Matched]] from the first
      * provider that recognises `A`, otherwise a [[ProviderResult.Skipped]] combining every provider's skip/error
      * reasons. Bottom types are skipped up front (see [[isBottomType]]).
      *
      * @since 0.3.0
      */
    def parse[A: Type]: ProviderResult[Provided[A]]

    /** Extractor form of [[parse]] for pattern matching: `Some(proof)` on a match, `None` otherwise (with the reasons
      * stashed in [[lastUnapplyFailure]]).
      *
      * @since 0.3.0
      */
    def unapply[A](tpe: Type[A]): Option[Provided[A]] = parse(using tpe) match {
      case ProviderResult.Matched(value) =>
        lastUnapplyFailure = null
        Some(value)
      case ProviderResult.Skipped(reasons) =>
        lastUnapplyFailure = reasons
        None
    }

    /** Whether `A` is a bottom type (`Nothing` or `Null`).
      *
      * Bottom types conform to EVERY type via `<:<`, so a provider that selects by `<:<` matches them and then crashes
      * while eagerly building exprs it cannot build (e.g. upcasting `java.util.Optional[Nothing]` to `Null`). No
      * provider can produce a value of a bottom type, so every shape companion's `parse` skips them up front rather
      * than crashing - which is why a registered [[Provider]] need not guard for them at those entry points. See #319.
      *
      * @since 0.3.0
      */
    final protected def isBottomType[A: Type]: Boolean =
      Type[A] <:< Type.of[Null] || Type[A] <:< Type.of[Nothing]
    // A `def` (not a `val`): a new trait `val`/`var` adds an abstract accessor to the compiled interface, breaking
    // binary compatibility; a `final def` compiles to a provided default method and does not. See issue #319.
    final protected def bottomTypeSkipReason: String =
      "bottom types (Nothing/Null) conform to every type but no provider can build a value of them"

    // Backing field is `private` so it stays out of the compiled interface (see the note above); the public accessor is
    // a `final def`. Read-only is also better API - callers should only inspect the provenance, not set it.
    private var lastMatchProvenanceValue: Option[ProviderProvenance] = None

    /** Provenance of the provider that produced the most recent successful match through [[parse]]/[[unapply]], or
      * `None` when the last call did not match. Populated by [[firstMatch]] (and thus by every companion whose `parse`
      * delegates to it). Lets callers tell built-in results apart from specific extensions. See issue #329.
      */
    final def lastMatchProvenance: Option[ProviderProvenance] = lastMatchProvenanceValue

    final protected def recordMatch(provider: Provider): Unit =
      lastMatchProvenanceValue = Some(ProviderProvenance(provider.name, provider.getClass.getName))

    /** Standard "first provider that matches wins, otherwise aggregate the skip reasons" loop, shared by the companions
      * with that shape (IsCollection/IsOption/IsEither/IsValueType). Records [[lastMatchProvenance]] on a match (#329).
      */
    final protected def firstMatch[A: Type](companionName: String): ProviderResult[Provided[A]] = {
      lastMatchProvenanceValue = None
      var skippedReasons = ListMap.empty[String, Either[Throwable, String]]
      val it = providers.iterator
      while (it.hasNext) {
        val provider = it.next()
        provider.parse(Type[A]) match {
          case matched: ProviderResult.Matched[Provided[A] @unchecked] =>
            recordMatch(provider)
            return matched
          case ProviderResult.Skipped(reasons) => skippedReasons ++= reasons.iterator
        }
      }
      NonEmptyMap.fromListMap(skippedReasons) match {
        case Some(nem) => ProviderResult.Skipped(nem)
        case None      => ProviderResult.skipped(companionName, "No providers registered")
      }
    }

    // Memoizes the (expensive) provider scan by the queried type, so re-parsing the same type within one macro
    // expansion runs the providers at most once. The provenance that `firstMatch` records as a side effect is
    // stored alongside the result and replayed on every call (hit or miss), so `lastMatchProvenance` stays correct.
    private type CachedFirstMatch[A] = (ProviderResult[Provided[A]], Option[ProviderProvenance])
    private lazy val firstMatchCache = new Type.Cache[CachedFirstMatch]

    /** [[firstMatch]] memoized by the queried type; use in `parse` instead of `firstMatch` to avoid re-scanning
      * providers for a type already seen in this expansion. Reproduces `firstMatch`'s [[lastMatchProvenance]] side
      * effect on a cache hit.
      */
    final protected def firstMatchCached[A: Type](companionName: String): ProviderResult[Provided[A]] = {
      val (result, provenance) = firstMatchCache.getOrPut(Type[A]) {
        val r = firstMatch[A](companionName)
        (r, lastMatchProvenanceValue)
      }
      lastMatchProvenanceValue = provenance
      result
    }
  }

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
    * @since 0.3.0
    *
    * @tparam Input
    *   the type of the input
    * @tparam Output
    *   the type of the output
    */
  trait CtorLikeOf[Input, Output] {

    type Result[A]
    @ImportedCrossTypeImplicit
    val Result: Type.Ctor1[Result]

    val ctor: Expr[Input] => Expr[Result[Output]]
    val method: Option[Method]

    def apply(input: Expr[Input]): Expr[Result[Output]] = ctor(input)
  }
  object CtorLikeOf {

    final case class PlainValue[Input, Output](
        ctor: Expr[Input] => Expr[Output],
        method: Option[Method]
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
        method: Option[Method]
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
        method: Option[Method]
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
        method: Option[Method]
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
        method: Option[Method]
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

  /** Shape companion aggregating every registered smart-constructor provider for a type. Unlike the other companions,
    * its [[parse]] '''concatenates''' the [[CtorLikeOf]]s from all matching providers rather than stopping at the first
    * match, so a type may be built through several constructors.
    *
    * @since 0.3.0
    */
  object CtorLikes extends ProvidedCompanion[CtorLikes] {

    override def parse[A: Type]: ProviderResult[CtorLikes[A]] = if (isBottomType[A])
      ProviderResult.skipped("CtorLikes", bottomTypeSkipReason)
    else {
      var matched: Option[CtorLikes[A]] = None
      var skippedReasons = ListMap.empty[String, Either[Throwable, String]]
      providers.foreach { provider =>
        provider.parse(Type[A]) match {
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

    /** Builder trait for creating CtorLike from an existential input type. Used instead of polymorphic function types
      * for Scala 2 compatibility.
      */
    trait CtorBuilder[Output, Result] {
      def apply[Input: Type](
          ctor: Expr[Input] => Expr[Result],
          method: Method
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
      val plainCtors: List[Method] =
        if (Type[Output] <:< Type[Result]) Type[Output].constructors
        else Nil

      // Non-instance methods from the type (for case objects, etc.)
      val noInstanceMethods: List[Method] = Type[Output].methods.filter {
        case _: Method.OnInstance => false
        case _                    => true
      }

      // Process constructors and non-instance methods
      (plainCtors ++ noInstanceMethods).collect {
        case method if method.isUnary && method.knownReturning.exists { rt =>
              import rt.Underlying as R
              Type[R] <:< Type[Result]
            } =>
          val (paramName, param) = method.parameters.flatten.head
          import param.tpe.Underlying as Input
          def applyMiInput(input: Expr[Input]): Expr[Result] =
            method match {
              case av: Method.ApplyValues =>
                av(Map(paramName -> input.as_??)) match {
                  case r: Method.Result[?] =>
                    r.build() match {
                      case Right(ex) => ex.asInstanceOf[Expr[Result]]
                      case Left(err) => hearthAssertionFailed(err.toString)
                    }
                  case other => hearthAssertionFailed(s"Expected Result after ApplyValues, got $other")
                }
              case other => hearthAssertionFailed(s"Expected ApplyValues for unary method, got $other")
            }
          Existential[CtorLikeOf[*, Output], Input](buildCtor[Input](applyMiInput, method))
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
    * @since 0.3.0
    *
    * @tparam CollA
    *   the type of the collection with applied item type
    * @tparam Item
    *   the type of the item
    */
  trait IsCollectionOf[CollA, Item] {

    def asIterable(value: Expr[CollA]): Expr[Iterable[Item]]

    /** Emits code that runs `f` on every element of the collection.
      *
      * The default splices the per-item body directly into a `while`/iterator loop (a "zero-closure" traversal) instead
      * of allocating a `Function1` for `Iterable.foreach`, because that closure often fails to scalarize at megamorphic
      * call sites; the loop form is never worse and frequently faster. A provider with a cheaper traversal (e.g. arrays
      * by index) should '''override''' this - the only contract is that `f` is applied to each element exactly once, in
      * iteration order, and its resulting `Expr[Unit]` bodies are spliced without allocating a closure per element.
      *
      * @since 0.3.0
      *
      * @param value
      *   the collection expression to traverse
      * @param f
      *   builds the per-element body from an element expression
      */
    def foreach(value: Expr[CollA])(f: Expr[Item] => Expr[Unit]): Expr[Unit] = {
      val iterableExpr = asIterable(value)
      Type.Ctor1.of[Iterable].unapply(Expr.typeOf(iterableExpr)) match {
        case Some(item) =>
          import item.Underlying as I
          val castIterableExpr = iterableExpr.asInstanceOf[Expr[Iterable[I]]]
          val fI = f.asInstanceOf[Expr[I] => Expr[Unit]]
          // Zero-closure default: walk the iterator with a `while` loop and splice the per-item body directly into
          // the loop, rather than allocating a `Function1` for `Iterable.foreach`. The closure form often fails to
          // scalarize at megamorphic call sites (e.g. map entry encoding), so the `while` form is both never worse
          // and frequently faster. Providers with a cheaper traversal (e.g. arrays by index) still override this.
          Expr.quote {
            val it = Expr.splice(castIterableExpr).iterator
            while (it.hasNext) {
              val elem = it.next()
              Expr.splice(fI(Expr.quote(elem)))
            }
          }
        case None =>
          hearthRequirementFailed(
            s"foreach default: asIterable did not return Iterable[_], got ${Expr.typeOf(iterableExpr).prettyPrint}"
          )
      }
    }

    /** If this collection is actually a map, returns its map proof (whose `Pair` is this `Item`); `None` for a plain
      * collection. Lets a caller that already holds an `IsCollectionOf` discover map-ness without a second
      * `IsCollection.parse` and without an erasure-unsound `isInstanceOf[IsMapOf[?, ?]]`.
      *
      * @since 0.3.1
      */
    def asMap: Option[IsMapOf[CollA, Item]] = None

    type CtorResult
    @ImportedCrossTypeImplicit
    implicit val CtorResult: Type[CtorResult]

    def factory: Expr[scala.collection.Factory[Item, CtorResult]]

    def build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], CollA]
  }

  /** An alias indicating the type is a collection of some item type, but the exact item type is an existential type.
    *
    * @since 0.3.0
    *
    * @tparam A
    *   the type of the collection
    */
  type IsCollection[A] = Existential[IsCollectionOf[A, *]]

  /** Shape companion recognising collection-shaped types; register a [[Provider]] here to teach it a custom collection.
    *
    * @since 0.3.0
    */
  object IsCollection extends ProvidedCompanion[IsCollection] {

    override def parse[A: Type]: ProviderResult[IsCollection[A]] = if (isBottomType[A])
      ProviderResult.skipped("IsCollection", bottomTypeSkipReason)
    else firstMatchCached[A]("IsCollection")
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
    * @since 0.3.0
    *
    * @tparam MapKV
    *   the type of the map with applied key and value type
    * @tparam Pair
    *   the type of the pair of key and value
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

    override def asMap: Option[IsMapOf[MapKV, Pair]] = Some(this)
  }

  /** An alias indicating the type is a map of some key and value types, but the exact key and value types are an
    * existential type.
    *
    * @since 0.3.0
    *
    * @tparam A
    *   the type of the map
    */
  type IsMap[A] = Existential[IsMapOf[A, *]]

  /** Shape companion recognising map-shaped types. It decodes through [[IsCollection]] and keeps only collections that
    * are actually maps (whose [[IsCollectionOf.asMap]] is defined), so map providers register on `IsCollection`.
    *
    * @since 0.3.0
    */
  object IsMap {

    var lastUnapplyFailure: NonEmptyMap[String, Either[Throwable, String]] = _

    /** Provenance of the provider behind the most recent successful match - mirrors
      * [[IsCollection.lastMatchProvenance]] since `IsMap` decodes through the collection providers. See issue #329.
      */
    def lastMatchProvenance: Option[ProviderProvenance] = IsCollection.lastMatchProvenance

    def parse[A: Type]: ProviderResult[IsMap[A]] =
      IsCollection.parse[A] match {
        case ProviderResult.Matched(isCollection) if isCollection.value.asMap.isDefined =>
          ProviderResult.Matched(isCollection.asInstanceOf[IsMap[A]])
        case ProviderResult.Matched(_) =>
          ProviderResult.skipped("IsMap", s"${Type[A].prettyPrint} is a collection but not a Map")
        case s: ProviderResult.Skipped => s
      }

    def unapply[A](tpe: Type[A]): Option[IsMap[A]] = parse(using tpe) match {
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
    * @since 0.3.0
    *
    * @tparam OptionA
    *   the type of the option with applied item type
    * @tparam Item
    *   the type of the item
    */
  trait IsOptionOf[OptionA, Item] {

    def empty: Expr[OptionA]

    def of(value: Expr[Item]): Expr[OptionA]

    def fold[A: Type](option: Expr[OptionA])(onEmpty: Expr[A], onSome: Expr[Item] => Expr[A]): Expr[A]

    def getOrElse(option: Expr[OptionA])(default: Expr[Item]): Expr[Item]

    def orElse(option: Expr[OptionA])(default: Expr[OptionA]): Expr[OptionA]
  }

  /** An alias indicating the type is an option of some item type, but the exact item type is an existential type.
    *
    * @since 0.3.0
    *
    * @tparam A
    *   the type of the option
    */
  type IsOption[A] = Existential[IsOptionOf[A, *]]

  /** Shape companion recognising option-shaped types; register a [[Provider]] here to teach it a custom option.
    *
    * @since 0.3.0
    */
  object IsOption extends ProvidedCompanion[IsOption] {

    override def parse[A: Type]: ProviderResult[IsOption[A]] = if (isBottomType[A])
      ProviderResult.skipped("IsOption", bottomTypeSkipReason)
    else firstMatchCached[A]("IsOption")
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
    * @since 0.3.0
    *
    * @tparam EitherLR
    *   the type of the either with applied left and right type
    * @tparam LeftValue
    *   the type of the left value
    * @tparam RightValue
    *   the type of the right value
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
    * @since 0.3.0
    *
    * @tparam EitherLR
    *   the type of the either with applied left and right type
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

  /** Shape companion recognising either-shaped types; register a [[Provider]] here to teach it a custom either.
    *
    * @since 0.3.0
    */
  object IsEither extends ProvidedCompanion[IsEither] {

    override def parse[A: Type]: ProviderResult[IsEither[A]] = if (isBottomType[A])
      ProviderResult.skipped("IsEither", bottomTypeSkipReason)
    else firstMatchCached[A]("IsEither")
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
    * @since 0.3.0
    *
    * @tparam Outer
    *   the type of the value type
    * @tparam Inner
    *   the type of the inner type
    */
  trait IsValueTypeOf[Outer, Inner] {

    val unwrap: Expr[Outer] => Expr[Inner]

    val wrap: CtorLikeOf[Inner, Outer]

    def ctors: CtorLikes[Outer]
  }

  /** An alias indicating the type is a value type of some inner type, but the exact inner type is an existential type.
    *
    * @since 0.3.0
    *
    * @tparam A
    *   the type of the value type
    */
  type IsValueType[A] = Existential[IsValueTypeOf[A, *]]

  /** Shape companion recognising value/wrapper types; register a [[Provider]] here to teach it a custom value type.
    *
    * @since 0.3.0
    */
  object IsValueType extends ProvidedCompanion[IsValueType] {

    override def parse[A: Type]: ProviderResult[IsValueType[A]] = if (isBottomType[A])
      ProviderResult.skipped("IsValueType", bottomTypeSkipReason)
    else firstMatchCached[A]("IsValueType")
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
