package hearth
package typed

import hearth.fp.Id
import hearth.fp.data.*
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

trait Types extends TypeConstructors with TypesCrossQuotes with TypesCompat { this: MacroCommons =>

  /** Platform-specific type representation (`c.WeakTypeTag[A]` in 2, `scala.quoted.Type[A]` in 3).
    *
    * Typed [[Type]] and [[UntypedType]] exist because some macro operations do not require the full knowledge about the
    * type (because they operate on Symbols, and we would have to convert from the typed representation to Symbols to
    * use them), and some require the full knowledge about the type (because e.g. type parameters from the class have to
    * be applied to its methods and their arguments/returned values).
    *
    * The implementation will use the right underlying representation to perform the operations, and where possible
    * convert between typed and untyped representations, but the distinction would be useful in cases where some
    * operations are available to only one of them. Then the user could convert between them in the context where the
    * missing information is available.
    *
    * Note that existential type [[??]], is not an [[UntypedType]] - it's a typed representation, where the macro during
    * **its execution** would know the exact type BUT it's inconvenient for us to use generics to represent that exact
    * type during compilation of the macro itself (not its expansion).
    *
    * @since 0.1.0
    */
  type Type[A]

  /** Phantom type constructor of kind `* -> *`, standing in for a type constructor that was discovered during macro
    * execution rather than written literally in the macro's sources (see [[Type.decompose1]]).
    *
    * It is never instantiated nor extended — only its role as a placeholder label matters: a `Type[AnyK1[X]]` returned
    * by APIs using this placeholder is backed by the platform representation of the real constructor (e.g. `G[X]`), so
    * printing, comparisons and implicit summoning all see the real type, even though static typing shows `AnyK1`.
    *
    * See [[Type.decompose1]] for the end-to-end recipe (discover a constructor `G` at expansion, then summon a type
    * class `TC[G]` for it via `Type.CtorK1.of[TC].apply(using gCtor)`).
    *
    * @since 0.4.0
    */
  sealed trait AnyK1[A]

  /** Phantom type constructor of kind `(*, *) -> *`, standing in for a type constructor that was discovered during
    * macro execution rather than written literally in the macro's sources (see [[Type.decompose2]]).
    *
    * It is never instantiated nor extended — only its role as a placeholder label matters: a `Type[AnyK2[X, Y]]`
    * returned by APIs using this placeholder is backed by the platform representation of the real constructor (e.g.
    * `G[X, Y]`), so printing, comparisons and implicit summoning all see the real type, even though static typing shows
    * `AnyK2`.
    *
    * @since 0.4.0
    */
  sealed trait AnyK2[A, B]

  val Type: TypeModule
  trait TypeModule extends Ctors with TypeCrossQuotes with TypeCompat { this: Type.type =>

    /** Summons `Type` instance */
    final def apply[A](implicit A: Type[A]): Type[A] = A

    final def apply[A](value: A)(implicit codec: TypeCodec[A]): Type[A] = codec.toType(value)
    final def unapply[A](tpe: Type[A])(implicit codec: TypeCodec[A]): Option[A] = codec.fromType(tpe).map(_.value)

    def shortName[A: Type]: String
    final def fqcn[A: Type]: String = plainPrint[A].takeWhile(_ != '[')
    def plainPrint[A: Type]: String
    def prettyPrint[A: Type]: String

    /** Builds a runtime expression that prints the type name, substituting overridden type arguments with runtime
      * values. Non-overridden parts produce output identical to [[plainPrint]].
      *
      * @since 0.3.0
      */
    def runtimePlainPrint[A: Type](overrideForType: ?? => Option[Expr[String]]): Expr[String]

    /** Like [[runtimePlainPrint]] but the non-overridden parts use ANSI-colored output (like [[prettyPrint]]).
      *
      * @since 0.3.0
      */
    def runtimePrettyPrint[A: Type](overrideForType: ?? => Option[Expr[String]]): Expr[String]

    /** Like [[runtimePlainPrint]] but the non-overridden parts use short names (like [[shortName]]).
      *
      * @since 0.3.0
      */
    def runtimeShortPrint[A: Type](overrideForType: ?? => Option[Expr[String]]): Expr[String]

    /** Resolves the runtime `java.lang.Class` of `A` when it is available on the classpath.
      *
      * This can only work if the type is available in the classpath, so it's not a good idea to use it for e.g. types
      * from the current project (which are not yet compiled and thus have no loadable class).
      *
      * Total and safe for callers: it returns `None` (never throws) whenever the class cannot be resolved - a
      * user-project type, an abstract type, a type parameter, etc. The one internal assertion in the underlying
      * `UntypedType.toClass` fires only for a genuine maintainer gap (a type flagged built-in without a matching
      * branch), never for anything a user can express; arrays and `IArray` answer with `None` rather than throwing
      * (issue #333).
      *
      * Caveat: a re-materialised parameterised `classOf` literal does not survive Scala 2 re-typechecking - e.g.
      * splicing `classOf[EnumSet[?]]` fails with "takes type parameters" (issue #321), so prefer the raw constructor's
      * class over a parameterised one.
      *
      * @see
      *   [[getRuntimeClass]] for the same lookup as an extension method on `Type[A]`
      *
      * @since 0.1.0
      *
      * @return
      *   the loaded `java.lang.Class[A]`, or `None` when the type is not resolvable on the classpath
      */
    final def classOfType[A: Type]: Option[java.lang.Class[A]] =
      UntypedType.toClass(UntypedType.fromTyped[A]).map(_.asInstanceOf[java.lang.Class[A]])

    /** Attempts: top-level object, object in object, etc.
      *
      * We can use the resulting stream to find first class name that e.g. is an actual class, or is a class
      * representing a module singleton.
      *
      * Based on https://github.com/MateuszKubuszok/MacroTypeclass ideas.
      */
    final def possibleClassesOfType[A: Type]: Array[String] = {
      val plain = plainPrint[A].takeWhile(_ != '[')
      def iter(n: String): Array[String] =
        scala.collection.Iterator
          .iterate(n)(_.reverse.replaceFirst("[.]", "\\$").reverse)
          .take(n.count(_ == '.') + 1)
          .toArray
          .reverse
      // JVM-encode the simple name (the segment after the last '.') so a symbolic name like `::` becomes
      // `$colon$colon` and resolves via `Class.forName` (a decoded `scala.collection.immutable.::` never would).
      // `NameTransformer.encode` is the identity for ordinary alphanumeric names, so this only ADDS candidates for
      // symbolic types and never changes resolution for anything already handled.
      def encodeSimpleName(fqn: String): String = {
        val idx = fqn.lastIndexOf('.')
        if (idx < 0) scala.reflect.NameTransformer.encode(fqn)
        else fqn.substring(0, idx + 1) + scala.reflect.NameTransformer.encode(fqn.substring(idx + 1))
      }
      // Decoded candidates first (existing behaviour), then the symbol-encoded ones as an additive fallback.
      def candidatesOf(base: String): Array[String] = {
        val encoded = encodeSimpleName(base)
        if (encoded == base) iter(base) else iter(base) ++ iter(encoded)
      }
      // When we see ".type", don't call isObject (avoids cyclic dependency in Scala 3). Produce candidates for both:
      // Scala object ("foo.Bar.type" -> "foo.Bar$") and Java enum/singleton ("java.lang.Thread.State.type" -> "java.lang.Thread.State").
      if (plain.endsWith(".type")) {
        val base = plain.dropRight(".type".length)
        (candidatesOf(base + '$') ++ candidatesOf(base)).distinct
      } else candidatesOf(plain).distinct
    }

    final def position[A: Type]: Option[Position] = UntypedType.fromTyped[A].position

    final def companionObject[A: Type]: Option[Expr_??] =
      UntypedType.fromTyped[A].companionObject.map { case (tpe, expr) =>
        val A0 = tpe.asTyped[A]
        val expr0 = expr.asTyped[A](using A0)
        Existential(expr0)(using A0)
      }

    /** Direct subtypes of a sealed hierarchy or enum `A`, keyed by the subtype's SIMPLE name, or `None` when `A` is not
      * decomposable.
      *
      * '''The map is keyed by the subtype's SIMPLE name.''' Same-named subtypes declared in different scopes (e.g.
      * `object Color { case object Green }` alongside a top-level `case object Green`) collapse into a single entry,
      * hiding the ambiguity (issue #309) - use [[directChildrenList]] when you must observe every subtype. '''Do not
      * rely on the key format''': it is retained as-is for backward compatibility only. In 0.5.0 the key changes to the
      * subtype's FULL name (without type parameters).
      *
      * @since 0.1.0
      */
    final def directChildren[A: Type]: Option[ListMap[String, ??<:[A]]] =
      UntypedType.fromTyped[A].directChildren.map(m => ListMap.from(m.view.mapValues(_.asTyped[A].as_??<:[A])))

    /** Like [[directChildren]], but returns an ordered `List` preserving duplicate simple names and extraction order,
      * so same-named subtypes in different scopes are all observable (and their ambiguity detectable). See issue #309.
      *
      * Because [[directChildren]] collapses same-simple-name subtypes - and will change its key to the subtype's FULL
      * name in 0.5.0 - this list form is the stable way to observe every subtype regardless of the map's key format.
      *
      * @since 0.4.1
      */
    final def directChildrenList[A: Type]: Option[List[(String, ??<:[A])]] =
      UntypedType
        .fromTyped[A]
        .directChildrenList
        .map(_.map { case (name, subtype) =>
          name -> subtype.asTyped[A].as_??<:[A]
        })

    /** Recursively flattened leaf subtypes of a sealed hierarchy or `enum` `A` as a `NonEmptyMap`, or `None` when `A`
      * is not exhaustively decomposable.
      *
      * Unlike [[directChildren]] (one level only), this recurses through nested sealed parents down to the concrete
      * leaves (`case class`es, `case object`s, `enum` cases, enumeration values), so the result is the exhaustive set
      * of value shapes a match on `A` must handle. Stable singletons and enumeration values are treated as leaves and
      * not recursed into.
      *
      * @see
      *   [[directChildren]] for a single level, [[directChildrenList]] for the ambiguity-preserving list form
      *
      * @since 0.1.0
      */
    final def exhaustiveChildren[A: Type]: Option[NonEmptyMap[String, ??<:[A]]] =
      UntypedType.fromTyped[A].exhaustiveChildren.map(m => m.map { case (k, v) => (k, v.asTyped[A].as_??<:[A]) })

    final def annotations[A: Type]: List[Expr_??] = {
      val untyped = UntypedType.fromTyped[A]
      untyped.annotations.zip(untyped.annotationTypes).map { case (expr, tpe) =>
        UntypedExpr.as_??(expr, tpe)
      }
    }

    /** Annotations on type `A` whose type is a subtype of `Ann`, with each expression typed as `Expr[Ann]`.
      *
      * Subtype (`<:<`) matching is used (rather than `=:=`) so that a whole annotation hierarchy can be matched by its
      * common base type. The constructor arguments of a matched annotation can be read with
      * [[Annotations.constructorArguments]].
      *
      * Example - finding case-class fields that carry a marker annotation (e.g. `@sensitiveData`):
      *
      * {{{
      * val sensitiveFields = caseClass.primaryConstructor.totalParameters.flatten.collect {
      *   case (name, param) if param.hasAnnotationOfType[sensitiveData] => name
      * }
      * }}}
      *
      * @since 0.4.0
      */
    final def annotationsOfType[A: Type, Ann: Type]: List[Expr[Ann]] = Annotations.filterOfType[Ann](annotations[A])

    /** Annotations in the TYPE POSITION of `A` - the `@Ann` in `X @Ann` (an `AnnotatedType`), as opposed to
      * [[annotations]] which reads annotations on the type's SYMBOL. Typically used to inspect annotations placed AFTER
      * the type of a case-class field (`name: String @Action("anonymize")`). Stacked annotations are all returned. See
      * issue #306.
      *
      * @since 0.4.1
      */
    final def typeAnnotations[A: Type]: List[Expr_??] = {
      val untyped = UntypedType.fromTyped[A]
      untyped.typeAnnotations.zip(untyped.typeAnnotationTypes).map { case (expr, tpe) =>
        UntypedExpr.as_??(expr, tpe)
      }
    }

    /** Type-position annotations on `A` (see [[typeAnnotations]]) whose type is a subtype of `Ann`, each typed as
      * `Expr[Ann]`. `<:<` matching (like [[annotationsOfType]]), so an annotation hierarchy can be matched by its base.
      *
      * @since 0.4.1
      */
    final def typeAnnotationsOfType[A: Type, Ann: Type]: List[Expr[Ann]] =
      Annotations.filterOfType[Ann](typeAnnotations[A])

    /** Whether type `A` has at least one annotation whose type is a subtype of `Ann`.
      *
      * @since 0.4.0
      */
    final def hasAnnotationOfType[A: Type, Ann: Type]: Boolean = annotations[A].exists(_.Underlying <:< Type[Ann])

    /** The 8 JVM value types that box to `java.lang.Object`: `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`,
      * `Double`, `Char`.
      *
      * '''`Unit` is intentionally NOT in this list''' - it does not box the way the 8 value types do. Beware the
      * resulting asymmetry: [[isPrimitive]] returns `true` for `Unit` (for parity with scalac's `Symbol.isPrimitive`,
      * issue #310) even though `Unit` is absent here. See [[isPrimitive]] for the full contract and [[jvmBuiltInTypes]]
      * for the broader built-in set.
      *
      * @since 0.1.0
      */
    // `lazy` (also below): materializing these `Type.of` lists on module init costs every macro expansion for lists
    // that most expansions never read.
    final lazy val primitiveTypes: List[??] = List(
      Type.of[Boolean].as_??,
      Type.of[Byte].as_??,
      Type.of[Short].as_??,
      Type.of[Int].as_??,
      Type.of[Long].as_??,
      Type.of[Float].as_??,
      Type.of[Double].as_??,
      Type.of[Char].as_??
    )

    /** Whether `A` is a JVM primitive.
      *
      * `true` for any of the 8 value types in [[primitiveTypes]] '''and additionally for `Unit`''' - so
      * `isPrimitive[Unit]` is `true` even though `Unit` is deliberately absent from [[primitiveTypes]]. This asymmetry
      * exists for parity with scalac's `Symbol.isPrimitive` (issue #310). See `UntypedType.isPrimitive` for the full
      * contract.
      *
      * @since 0.1.0
      */
    final def isPrimitive[A: Type]: Boolean = UntypedType.fromTyped[A].isPrimitive
    final def isArray[A: Type]: Boolean = UntypedType.fromTyped[A].isArray
    final def isIArray[A: Type]: Boolean = UntypedType.fromTyped[A].isIArray

    /** The listable JVM built-in types: every type in [[primitiveTypes]] plus `Unit`, `String` and `java.lang.Class`.
      *
      * This list is NOT exhaustive for [[isJvmBuiltIn]]: the built-in set is open-ended. Arrays (`isArray`/`isIArray`)
      * and every `java.lang.*` type ([[isInJavaLangPackage]]) also count as built-in, but they are unbounded families
      * so they are detected structurally rather than enumerated here. See [[isJvmBuiltIn]] for the full contract.
      *
      * @since 0.1.0
      */
    final lazy val jvmBuiltInTypes: List[??] = primitiveTypes ++ List(
      Type.of[String].as_??,
      Type.of[Unit].as_??,
      Type.of[java.lang.Class[?]].as_??
    )

    /** Whether `A` is a JVM built-in type.
      *
      * `true` for anything in [[jvmBuiltInTypes]] (primitives, `Unit`, `String`, `java.lang.Class`) '''or''' any array
      * ([[isArray]]/[[isIArray]]) '''or''' any `java.lang.*` type ([[isInJavaLangPackage]]). The array and
      * `java.lang.*` families are unbounded, so this is broader than the listable [[jvmBuiltInTypes]]. See
      * `UntypedType.isJvmBuiltIn` for the full contract.
      *
      * @since 0.1.0
      */
    final def isJvmBuiltIn[A: Type]: Boolean = UntypedType.fromTyped[A].isJvmBuiltIn
    final def isInJavaLangPackage[A: Type]: Boolean = UntypedType.fromTyped[A].isInJavaLangPackage

    final lazy val typeSystemSpecialTypes: List[??] = List(
      Type.of[Any].as_??,
      Type.of[AnyRef].as_??,
      Type.of[AnyVal].as_??,
      Type.of[Null].as_??,
      Type.of[Nothing].asInstanceOf[Type[Any]].as_?? // Type[Nothing] sees no extension methods
    )
    final def isTypeSystemSpecial[A: Type]: Boolean = UntypedType.fromTyped[A].isTypeSystemSpecial
    final def isOpaqueType[A: Type]: Boolean = UntypedType.fromTyped[A].isOpaqueType

    /** Underlying type of an opaque type, as seen from outside its defining scope (Scala 3-only).
      *
      * Resolves the right-hand side of `opaque type X = Y` even through:
      *   - bounds (`opaque type X <: Int = Int` resolves to `Int`, not the bound),
      *   - chains of opaque types (`opaque type Outer = Inner` where `Inner` is itself opaque resolves to the innermost
      *     underlying type),
      *   - applied parameterized opaques (`opaque type Wrapper[A] = List[A]` applied as `Wrapper[Int]` resolves to
      *     `List[Int]`),
      *   - plain aliases to opaque types (`type Alias = X` resolves to `X`'s underlying type).
      *
      * Returns `None` when the type is not an opaque type, or when the underlying type cannot be determined. Always
      * returns `None` on Scala 2 (which has no opaque types).
      *
      * @since 0.4.0
      */
    final def opaqueUnderlyingType[A: Type]: Option[??] =
      UntypedType.fromTyped[A].opaqueUnderlyingType.map(_.as_??)
    final def isTuple[A: Type]: Boolean = UntypedType.fromTyped[A].isTuple
    final def isNamedTuple[A: Type]: Boolean = UntypedType.fromTyped[A].isNamedTuple
    final def isUnionType[A: Type]: Boolean = UntypedType.fromTyped[A].isUnionType

    /** Whether the member `B` of the union type `A` has to be discriminated with a user-provided
      * `scala.reflect.TypeTest[A, B]` rather than a runtime class test (Scala 3-only).
      *
      * `true` for non-singleton union members whose runtime class either cannot be determined (abstract types, type
      * parameters) or is not disjoint from another member's runtime class (same/related erasure, e.g. `List[Int]` and
      * `List[String]`). Always `false` on Scala 2 (which has no union types).
      *
      * @since 0.4.0
      */
    final def unionMemberRequiresTypeTest[A: Type, B: Type]: Boolean =
      UntypedType.fromTyped[A].unionMemberRequiresTypeTest(UntypedType.fromTyped[B])

    /** Human-readable reason why [[directChildren]] refused to decompose the union type `A` (Scala 3-only).
      *
      * Currently provided only when some members are not runtime-distinguishable and no implicit
      * `scala.reflect.TypeTest[A, Member]` was found for them - the message names the missing instances. Returns `None`
      * when the type is not a refused union, or when the refusal has no actionable fix (e.g. members with static
      * subtype relationships). Always `None` on Scala 2 (which has no union types).
      *
      * @since 0.4.0
      */
    final def unionRefusalReason[A: Type]: Option[String] = UntypedType.fromTyped[A].unionRefusalReason

    final def isAbstract[A: Type]: Boolean = UntypedType.fromTyped[A].isAbstract
    final def isFinal[A: Type]: Boolean = UntypedType.fromTyped[A].isFinal
    final def isTrait[A: Type]: Boolean = UntypedType.fromTyped[A].isTrait

    final def isClass[A: Type]: Boolean = UntypedType.fromTyped[A].isClass
    final def notJvmBuiltInClass[A: Type]: Boolean = isClass[A] && !isJvmBuiltIn[A]
    final def isPlainOldJavaObject[A: Type]: Boolean =
      notJvmBuiltInClass[A] && !(isAbstract[A] || isSealed[A] || isJavaEnum[A] || isJavaEnumValue[A] || isEnumeration[
        A
      ])
    final def isJavaBean[A: Type]: Boolean = isJavaBean[A](Everywhere)
    final def isJavaBean[A: Type](visibility: Accessible): Boolean =
      !isObject[A] && isPlainOldJavaObject[A] && Type[A].defaultConstructor.exists(_.isAvailable(visibility))

    final def isSealed[A: Type]: Boolean = UntypedType.fromTyped[A].isSealed
    final def isJavaEnum[A: Type]: Boolean = UntypedType.fromTyped[A].isJavaEnum
    final def isJavaEnumValue[A: Type]: Boolean = UntypedType.fromTyped[A].isJavaEnumValue

    /** Whether `A` is a member of a `scala.Enumeration` (the pre-`enum` Scala 2 `Enumeration#Value` pattern).
      *
      * This is NOT a Scala 3 `enum` (whose cases are decomposable via [[directChildren]]/[[isCaseVal]]) and NOT a Java
      * `enum` (see [[isJavaEnum]]). The three "enum" notions are easy to confuse (issue #311).
      *
      * @see
      *   [[isJavaEnum]], [[isSealed]]
      *
      * @since 0.1.0
      */
    final def isEnumeration[A: Type]: Boolean = UntypedType.fromTyped[A].isEnumeration

    /** Whether `A` carries the raw `case` flag (a `case class`, `case object`, or `enum` case).
      *
      * This is the low-level flag; the derived predicates [[isCaseClass]], [[isCaseObject]] and [[isCaseVal]] combine
      * it with [[isClass]]/[[isObject]]/[[isVal]] to distinguish the three shapes.
      *
      * @since 0.1.0
      */
    final def isCase[A: Type]: Boolean = UntypedType.fromTyped[A].isCase

    /** Whether `A` is a module singleton (`object`).
      *
      * Note that a parameterless Scala 3 `enum` case is a `val`, NOT an `object` - see [[isVal]] and [[isCaseVal]]
      * (issue #311).
      *
      * @since 0.1.0
      */
    final def isObject[A: Type]: Boolean = UntypedType.fromTyped[A].isObject

    /** Whether `A` is a `val` singleton, in particular a parameterless Scala 3 `enum` case (which is represented as a
      * `val`, not an `object` - issue #311).
      *
      * @see
      *   [[isObject]], [[isCaseVal]]
      *
      * @since 0.1.0
      */
    final def isVal[A: Type]: Boolean = UntypedType.fromTyped[A].isVal

    final def isCaseClass[A: Type]: Boolean = UntypedType.fromTyped[A].isCaseClass
    final def isCaseObject[A: Type]: Boolean = UntypedType.fromTyped[A].isCaseObject

    /** Whether `A` is a parameterless case value - a `case object` or a parameterless Scala 3 `enum` case.
      *
      * Distinct from [[isCaseObject]]: a Scala 3 parameterless `enum` case is a `val`, not an `object`, so it is an
      * `isCaseVal` but not an `isCaseObject` (issue #311 - such cases were previously missed).
      *
      * @see
      *   [[isCaseObject]], [[isCaseClass]]
      *
      * @since 0.4.0
      */
    final def isCaseVal[A: Type]: Boolean = UntypedType.fromTyped[A].isCaseVal

    final def isAvailable[A: Type](scope: Accessible): Boolean = UntypedType.fromTyped[A].isAvailable(scope)

    /** Direct parents (the declared supertypes) of `A`, e.g. `List(AnyRef, Serializable, ...)`.
      *
      * These are only the immediate supertypes as written; use [[baseClasses]] for the full transitive linearization.
      * On Scala 3 this is computed via Java reflection to avoid the `TypeRepr#baseClasses` extension-method shadowing
      * that would otherwise capture the call (issue #328).
      *
      * @see
      *   [[baseClasses]]
      *
      * @since 0.4.1
      *
      * @return
      *   the direct supertypes of `A`, each as an existential `??`
      */
    final def parents[A: Type]: List[??] = UntypedType.fromTyped[A].parents.map(_.as_??)

    /** The full linearized set of base classes of `A` - transitive, and including `A` itself.
      *
      * Contrast [[parents]], which returns only the direct supertypes. On Scala 3, code that also imports
      * `quotes.reflect` should prefer the `UntypedType#baseClassTypes` extension to sidestep the `TypeRepr#baseClasses`
      * shadowing (issue #328).
      *
      * @see
      *   [[parents]]
      *
      * @since 0.4.1
      *
      * @return
      *   every base class of `A` (including `A`), each as an existential `??`
      */
    final def baseClasses[A: Type]: List[??] = UntypedType.fromTyped[A].baseClasses.map(_.as_??)

    final def isSubtypeOf[A: Type, B: Type]: Boolean = UntypedType.fromTyped[A] <:< UntypedType.fromTyped[B]
    final def isSameAs[A: Type, B: Type]: Boolean = UntypedType.fromTyped[A] =:= UntypedType.fromTyped[B]

    /** Type arguments of an applied type, e.g. `List(Type[Int])` for `List[Int]`, `Nil` for non-applied types.
      *
      * Typed counterpart of `UntypedType.typeArguments`.
      *
      * @since 0.4.0
      */
    final def typeArguments[A: Type]: List[??] = UntypedType.typeArguments(UntypedType.fromTyped[A]).map(_.as_??)

    /** Checks whether 2 types are built from the same type constructor (compared by type symbol, after dealiasing),
      * ignoring their type arguments, e.g. `List[Int]` and `List[String]` agree, but `List[Int]` and `Vector[Int]` do
      * not.
      *
      * Typed counterpart of `UntypedType.sameTypeConstructorAs`. To compare against a constructor obtained from
      * `Type.CtorN`, use `ctor.sameTypeConstructorAs(other)` instead.
      *
      * @since 0.4.0
      */
    final def hasSameTypeConstructor[A: Type, B: Type]: Boolean =
      UntypedType.sameTypeConstructorAs(UntypedType.fromTyped[A], UntypedType.fromTyped[B])

    /** Decomposes an applied type `G[X]` (where the constructor `G` is NOT known statically) into its type constructor
      * and its type argument, e.g. turns `Type[List[Int]]` into `Type.Ctor1` of `List` and `Type[Int]` (as `??`).
      *
      * The constructor is returned as a fully functional `Type.Ctor1` (created via `Type.Ctor1.fromUntyped`), so it can
      * be re-applied to other types (`ctor[B]`), pattern-matched against other applied types (`ctor.unapply`), compared
      * (`ctor.sameTypeConstructorAs`), or passed where `Type.Ctor1` is expected (e.g. `Type.CtorK1#apply` to build
      * `Type[HKT[G]]` and summon a type class for the discovered constructor). Since `G` has no statically known name,
      * the phantom label [[AnyK1]] is used in its place - the underlying platform representation is the real
      * constructor.
      *
      * Returns `None` for non-applied types and for types applied to a number of arguments different than 1 (the type
      * is dealiased first, so e.g. `type StringMap[V] = Map[String, V]` decomposes as `Map[String, V]` would - into 2
      * arguments - rather than as a unary constructor).
      *
      * End-to-end recipe - discover a constructor `G` (e.g. the `G` in a field `g: G[X]`) at expansion, then summon a
      * type class `MyTC[G]` for it:
      *
      * {{{
      * // `A` is the type of some field like `g: G[X]`, where `G` is unknown when the macro is authored.
      * Type.decompose1[A] match {
      *   case Some((gCtor, _)) => // gCtor: Ctor1[AnyK1], backed by the real `G`
      *     implicit val tcOfG: Type[MyTC[AnyK1]] = Type.CtorK1.of[MyTC].apply(using gCtor)
      *     Expr.summonImplicit[MyTC[AnyK1]] // finds the real `MyTC[G]` instance
      *   case None => // `A` is not a unary applied type
      * }
      * }}}
      *
      * [[AnyK1]] is only a static label: the underlying platform type of `gCtor` is the real `G`, so implicit search
      * actually sees `MyTC[G]`. `gCtor` can also be re-applied (`gCtor[String]`), compared
      * (`gCtor.sameTypeConstructorAs(...)`) or pattern-matched.
      *
      * @since 0.4.0
      */
    final def decompose1[A: Type]: Option[(Ctor1[AnyK1], ??)] = {
      val dealiased = UntypedType.dealias(UntypedType.fromTyped[A])
      UntypedType.typeArguments(dealiased) match {
        case arg :: Nil => Some((Ctor1.fromUntyped[AnyK1](UntypedType.typeConstructor(dealiased)), arg.as_??))
        case _          => None
      }
    }

    /** Decomposes an applied type `G[X, Y]` (where the constructor `G` is NOT known statically) into its type
      * constructor and its type arguments, e.g. turns `Type[Map[String, Int]]` into `Type.Ctor2` of `Map` and
      * `(Type[String], Type[Int])` (as `??`s).
      *
      * Binary-constructor counterpart of [[decompose1]] - see its documentation for the details of the contract. Since
      * `G` has no statically known name, the phantom label [[AnyK2]] is used in its place.
      *
      * Returns `None` for non-applied types and for types applied to a number of arguments different than 2.
      *
      * @since 0.4.0
      */
    final def decompose2[A: Type]: Option[(Ctor2[AnyK2], (??, ??))] = {
      val dealiased = UntypedType.dealias(UntypedType.fromTyped[A])
      UntypedType.typeArguments(dealiased) match {
        case arg1 :: arg2 :: Nil =>
          Some((Ctor2.fromUntyped[AnyK2](UntypedType.typeConstructor(dealiased)), (arg1.as_??, arg2.as_??)))
        case _ => None
      }
    }

    // Literal types

    def NullCodec: TypeCodec[Null]
    def UnitCodec: TypeCodec[Unit]
    def BooleanCodec: TypeCodec[Boolean]
    def ByteCodec: TypeCodec[Byte]
    def ShortCodec: TypeCodec[Short]
    def IntCodec: TypeCodec[Int]
    def LongCodec: TypeCodec[Long]
    def FloatCodec: TypeCodec[Float]
    def DoubleCodec: TypeCodec[Double]
    def CharCodec: TypeCodec[Char]
    def StringCodec: TypeCodec[String]

    final def ClassCodec[A: Type]: TypeCodec[java.lang.Class[A]] = new TypeCodec[java.lang.Class[A]] {
      override def toType[B <: java.lang.Class[A]](value: B): Type[B] =
        Type.of[java.lang.Class[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[java.lang.Class[A], Id]] =
        Type.classOfType[A].map { clazz =>
          Existential.UpperBounded[java.lang.Class[A], Id, java.lang.Class[A]](clazz)(using
            B.asInstanceOf[Type[java.lang.Class[A]]]
          )
        }
    }
    final def ClassTagCodec[A: Type]: TypeCodec[scala.reflect.ClassTag[A]] =
      new TypeCodec[scala.reflect.ClassTag[A]] {
        override def toType[B <: scala.reflect.ClassTag[A]](value: B): Type[B] =
          Type.of[scala.reflect.ClassTag[A]].asInstanceOf[Type[B]]
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[scala.reflect.ClassTag[A], Id]] =
          Type.classOfType[A].map { clazz =>
            Existential.UpperBounded[scala.reflect.ClassTag[A], Id, scala.reflect.ClassTag[A]](
              scala.reflect.ClassTag(clazz)
            )(using B.asInstanceOf[Type[scala.reflect.ClassTag[A]]])
          }
      }

    // Tuple codecs
    def Tuple1Codec[T1: TypeCodec]: TypeCodec[Tuple1[T1]]
    def Tuple2Codec[T1: TypeCodec, T2: TypeCodec]: TypeCodec[(T1, T2)]
    def Tuple3Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec]: TypeCodec[(T1, T2, T3)]
    def Tuple4Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec]: TypeCodec[(T1, T2, T3, T4)]
    def Tuple5Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5)]
    def Tuple6Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec, T6: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5, T6)]
    def Tuple7Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7)]
    def Tuple8Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8)]
    def Tuple9Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]
    def Tuple10Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)]
    def Tuple11Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]
    def Tuple12Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)]
    def Tuple13Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)]
    def Tuple14Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]
    def Tuple15Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)]
    def Tuple16Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)]
    def Tuple17Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)]
    def Tuple18Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)]
    def Tuple19Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)]
    def Tuple20Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]
    def Tuple21Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)]
    def Tuple22Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec,
        T22: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)]

    // TODO: specialize for primitive types
    final def ArrayCodec[A: Type: TypeCodec]: TypeCodec[Array[A]] = new TypeCodec[Array[A]] {
      private lazy val ArrayCtor = Type.Ctor1.of[Array]
      override def toType[B <: Array[A]](value: B): Type[B] = ArrayCtor[A].asInstanceOf[Type[B]]
      // $COVERAGE-OFF$ Ctor1.unapply does not work for TypeCodec roundtrips yet
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Array[A], Id]] =
        B match {
          case ArrayCtor(elem) =>
            import elem.Underlying as Elem
            for {
              ct <- Type.classOfType[A].map(scala.reflect.ClassTag[A](_))
              v <- TypeCodec[A].fromType(Type[Elem])
            } yield {
              implicit val classTag: scala.reflect.ClassTag[A] = ct
              Existential.UpperBounded[Array[A], Id, Array[A]](
                Array(v.value.asInstanceOf[A])
              )(using B.asInstanceOf[Type[Array[A]]])
            }
          case _ => None
        }
      // $COVERAGE-ON$
    }
    final def SeqCodec[A: Type: TypeCodec]: TypeCodec[Seq[A]] = new TypeCodec[Seq[A]] {
      private lazy val SeqCtor = Type.Ctor1.of[Seq]
      override def toType[B <: Seq[A]](value: B): Type[B] = SeqCtor[A].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Seq[A], Id]] =
        if (B =:= Type.of[Nil.type])
          Some(Existential.UpperBounded[Seq[A], Id, Seq[A]](Nil)(using B.asInstanceOf[Type[Seq[A]]]))
        // $COVERAGE-OFF$ Ctor1.unapply does not work for TypeCodec roundtrips yet
        else
          B match {
            case SeqCtor(elem) =>
              import elem.Underlying as Elem
              TypeCodec[A].fromType(Type[Elem]).map { v =>
                Existential.UpperBounded[Seq[A], Id, Seq[A]](
                  Seq(v.value.asInstanceOf[A])
                )(using B.asInstanceOf[Type[Seq[A]]])
              }
            case _ => None
          }
      // $COVERAGE-ON$
    }
    final def ListCodec[A: Type: TypeCodec]: TypeCodec[List[A]] = new TypeCodec[List[A]] {
      private lazy val ListCtor = Type.Ctor1.of[List]
      override def toType[B <: List[A]](value: B): Type[B] = ListCtor[A].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[List[A], Id]] =
        if (B =:= Type.of[Nil.type])
          Some(Existential.UpperBounded[List[A], Id, List[A]](Nil)(using B.asInstanceOf[Type[List[A]]]))
        // $COVERAGE-OFF$ Ctor1.unapply does not work for TypeCodec roundtrips yet
        else
          B match {
            case ListCtor(elem) =>
              import elem.Underlying as Elem
              TypeCodec[A].fromType(Type[Elem]).map { v =>
                Existential.UpperBounded[List[A], Id, List[A]](
                  List(v.value.asInstanceOf[A])
                )(using B.asInstanceOf[Type[List[A]]])
              }
            case _ => None
          }
      // $COVERAGE-ON$
    }
    final lazy val NilCodec: TypeCodec[Nil.type] = new TypeCodec[Nil.type] {
      override def toType[B <: Nil.type](value: B): Type[B] = Type.of[Nil.type].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Nil.type, Id]] =
        if (B =:= Type.of[Nil.type])
          Some(Existential.UpperBounded[Nil.type, Id, Nil.type](Nil)(using B.asInstanceOf[Type[Nil.type]]))
        else None
    }
    final def VectorCodec[A: Type: TypeCodec]: TypeCodec[Vector[A]] = new TypeCodec[Vector[A]] {
      private lazy val VectorCtor = Type.Ctor1.of[Vector]
      override def toType[B <: Vector[A]](value: B): Type[B] = VectorCtor[A].asInstanceOf[Type[B]]
      // $COVERAGE-OFF$ Ctor1.unapply does not work for TypeCodec roundtrips yet
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Vector[A], Id]] =
        B match {
          case VectorCtor(elem) =>
            import elem.Underlying as Elem
            TypeCodec[A].fromType(Type[Elem]).map { v =>
              Existential.UpperBounded[Vector[A], Id, Vector[A]](
                Vector(v.value.asInstanceOf[A])
              )(using B.asInstanceOf[Type[Vector[A]]])
            }
          case _ => None
        }
      // $COVERAGE-ON$
    }
    final def MapCodec[K: Type: TypeCodec, V: Type: TypeCodec]: TypeCodec[Map[K, V]] = new TypeCodec[Map[K, V]] {
      private lazy val MapCtor = Type.Ctor2.of[Map]
      override def toType[B <: Map[K, V]](value: B): Type[B] = MapCtor[K, V].asInstanceOf[Type[B]]
      // $COVERAGE-OFF$ Ctor2.unapply does not work for TypeCodec roundtrips yet
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Map[K, V], Id]] =
        B match {
          case MapCtor(k, v) =>
            import k.Underlying as K0
            import v.Underlying as V0
            for {
              dk <- TypeCodec[K].fromType(Type[K0])
              dv <- TypeCodec[V].fromType(Type[V0])
            } yield Existential.UpperBounded[Map[K, V], Id, Map[K, V]](
              Map(dk.value.asInstanceOf[K] -> dv.value.asInstanceOf[V])
            )(using B.asInstanceOf[Type[Map[K, V]]])
          case _ => None
        }
      // $COVERAGE-ON$
    }
    final def SetCodec[A: Type: TypeCodec]: TypeCodec[Set[A]] = new TypeCodec[Set[A]] {
      private lazy val SetCtor = Type.Ctor1.of[Set]
      override def toType[B <: Set[A]](value: B): Type[B] = SetCtor[A].asInstanceOf[Type[B]]
      // $COVERAGE-OFF$ Ctor1.unapply does not work for TypeCodec roundtrips yet
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Set[A], Id]] =
        B match {
          case SetCtor(elem) =>
            import elem.Underlying as Elem
            TypeCodec[A].fromType(Type[Elem]).map { v =>
              Existential.UpperBounded[Set[A], Id, Set[A]](
                Set(v.value.asInstanceOf[A])
              )(using B.asInstanceOf[Type[Set[A]]])
            }
          case _ => None
        }
      // $COVERAGE-ON$
    }
    def OptionCodec[A: TypeCodec]: TypeCodec[Option[A]]
    def SomeCodec[A: TypeCodec]: TypeCodec[Some[A]]
    def NoneCodec: TypeCodec[None.type]
    def EitherCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Either[L, R]]
    def LeftCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Left[L, R]]
    def RightCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Right[L, R]]

    private object ModuleCodecImpl extends TypeCodec[Any] {

      def toType[A](value: A): Type[A] = UntypedType.fromClass(value.getClass).asTyped[A]

      def fromType[A](tpe: Type[A]): Option[Existential.UpperBounded[Any, Id]] =
        possibleClassesOfType(tpe).collectFirst { case ModuleSingleton(value) =>
          Existential.UpperBounded[Any, Id, A](value.asInstanceOf[A])(using tpe)
        }

      /** Matches if name represents a module singleton existing in the classpath. */
      private object ModuleSingleton {
        def unapply(className: String): Option[Any] = try
          Option(java.lang.Class.forName(className).getField("MODULE$").get(null))
        catch {
          case _: Throwable => None
        }
      }
    }
    final def ModuleCodec[ModuleSingleton <: Singleton]: TypeCodec[ModuleSingleton] =
      ModuleCodecImpl.asInstanceOf[TypeCodec[ModuleSingleton]]

    /** A lazily-computed [[Type]] that is SAFE to keep in per-expansion helper objects.
      *
      * A plain `lazy val tpe: Type[A] = Type.of[A]` is NOT: its first touch can happen inside an `Expr.splice` (which,
      * on Scala 3, evaluates under a fresh nested `Quotes`), tying the `Type` - and every expr later built from it - to
      * that splice's scope; reusing it from a sibling or outer splice then aborts under `-Xcheck-macros` with
      * "Expression created in a splice was used outside of that splice". Eager `val`s dodge this only by accident of
      * WHEN the enclosing object initializes - and pay the full materialization cost (a pickled-quote unpickling per
      * `Type.of` on Scala 3) on every macro expansion, used or not.
      *
      * `Type.Lazy` computes its thunk with the MACRO-ENTRY context pinned, so the result is scoped exactly like an
      * eagerly-initialized `val` (valid throughout the whole expansion, inside any splice), while the cost is only paid
      * if the type is actually used. The computed value is memoized per macro expansion (re-computed if the holder
      * outlives the expansion, e.g. when kept in a global object).
      *
      * Usage - replace an eager helper `val`:
      * {{{
      * val StringType: Type[String]      = Type.of[String]        // eager: costs every expansion
      * val StringType: Type.Lazy[String] = Type.Lazy(Type.of[String]) // deferred: costs only when used
      * }}}
      * Use-sites stay unchanged - a `Type.Lazy[A]` converts to `Type[A]` implicitly (see `lazyTypeToType`).
      *
      * Not thread-safe; a macro expansion is single-threaded.
      *
      * @since 0.4.1
      */
    final class Lazy[A] private[Types] (compute: () => Type[A]) {
      private var cachedFor: AnyRef = null
      private var cached: Type[A] = null.asInstanceOf[Type[A]]

      def value: Type[A] = {
        val key = CrossQuotes.macroEntryContextKey
        if ((cached == null) || (cachedFor ne key)) {
          cached = CrossQuotes.withMacroEntryContext(compute())
          cachedFor = key
        }
        cached
      }
    }
    object Lazy {

      /** Creates a [[Lazy]] - `compute` is by-name and will run (at most once per expansion) with the macro-entry
        * context pinned.
        *
        * @since 0.4.1
        */
      def apply[A](compute: => Type[A]): Lazy[A] = new Lazy(() => compute)
    }

    /** A mutable, macro-expansion-scoped memo keyed by `Type`, storing one `F[Result]` per distinct type.
      *
      * Intended for caching the results of expensive per-type computations (class-view parsing, std-extension provider
      * scans, etc.) so they run at most once per type within a single macro expansion.
      *
      * Keys are compared with [[=:=]] (semantic type equality), '''not''' `==`: a `Type[A]` has no value-based
      * `equals`, so on Scala 3 `==` is reference identity and would miss almost every hit. A miss simply recomputes, so
      * a stale, partial, or leaky cache can never yield a wrong result — only less speed-up.
      *
      * Lookups are not a linear scan: entries are hash-bucketed by a cheap discriminator (the dealiased type's symbol,
      * see [[hearth.untyped.UntypedTypes# UntypedTypeModule.cacheBucketKey]]), so the `=:=` comparison only runs
      * against the few same-symbol candidates. This mirrors `isSameAs`'s fast negative (differing dealiased symbols ⟹
      * not `=:=`), so bucketing can only turn an exotic would-be hit into a harmless miss.
      *
      * Entries are additionally partitioned by the active Cross-Quotes scope (`CrossQuotes.ctx` — the ACTIVE `Quotes`
      * on Scala 3, a constant on Scala 2): cached values routinely embed materialized `Expr`s (std provider views,
      * summoned implicits), and an `Expr` created during one `Expr.splice` evaluation must not be handed out during
      * another (`-Xcheck-macros` aborts with a ScopeException). Within one scope memoization is unaffected; a new scope
      * recomputes instead of leaking foreign-scope trees.
      *
      * Not thread-safe; a macro expansion is single-threaded.
      *
      * @since 0.4.1
      */
    final class Cache[F[_]] {
      // (cross-quotes scope token, cheap bucket key) -> entries checked with =:=.
      private val impl =
        new java.util.HashMap[(AnyRef, AnyRef), scala.collection.mutable.ListBuffer[Cache.Entry[F]]]

      private def bucketKey[Result](key: Type[Result]): (AnyRef, AnyRef) =
        (CrossQuotes.ctx[AnyRef], UntypedType.cacheBucketKey(UntypedType.fromTyped(using key)))

      /** Returns the cached `F[Result]` for a type `=:=` `key`, or `None`. */
      def get[Result](key: Type[Result]): Option[F[Result]] = {
        val bucket = impl.get(bucketKey(key))
        if (bucket == null) None
        else bucket.collectFirst { case e if e.key =:= key => e.value.asInstanceOf[F[Result]] }
      }

      /** Stores `value` under `key`. Does not deduplicate — call [[getOrPut]] to avoid recomputation. */
      def put[Result](key: Type[Result], value: F[Result]): Unit = {
        val bucket =
          impl.computeIfAbsent(bucketKey(key), _ => scala.collection.mutable.ListBuffer.empty[Cache.Entry[F]])
        bucket += Cache.Entry[F, Result](key, value)
        ()
      }

      /** Returns the cached `F[Result]` for a type `=:=` `key`, computing and storing `value` on a miss. */
      def getOrPut[Result](key: Type[Result])(value: => F[Result]): F[Result] =
        get(key).getOrElse {
          val result = value
          put(key, result)
          result
        }
    }
    object Cache {

      /** A single `(key, value)` entry, hiding the concrete `Result` type behind an abstract type member. */
      sealed trait Entry[F[_]] {
        type Result
        def key: Type[Result]
        def value: F[Result]
      }
      object Entry {
        def apply[F[_], R](k: Type[R], v: F[R]): Entry[F] = new Entry[F] {
          type Result = R
          val key: Type[R] = k
          val value: F[R] = v
        }
      }
    }
  }

  /** Unwraps a [[Type.Lazy]] wherever a [[Type]] is expected, so converting an eager helper `val` to `Type.Lazy`
    * requires no use-site changes (implicit `Type` positions still need the value assigned to an `implicit val`,
    * exactly as with the eager form).
    *
    * @since 0.4.1
    */
  implicit final def lazyTypeToType[A](lazyType: Type.Lazy[A]): Type[A] = lazyType.value

  implicit final class TypeMethods[A](private val tpe: Type[A]) {

    def shortName: String = Type.shortName(using tpe)
    def fqcn: String = Type.fqcn(using tpe)
    def plainPrint: String = Type.plainPrint(using tpe)
    def prettyPrint: String = Type.prettyPrint(using tpe)

    def runtimePlainPrint(overrideForType: ?? => Option[Expr[String]]): Expr[String] =
      Type.runtimePlainPrint(overrideForType)(using tpe)
    def runtimePrettyPrint(overrideForType: ?? => Option[Expr[String]]): Expr[String] =
      Type.runtimePrettyPrint(overrideForType)(using tpe)
    def runtimeShortPrint(overrideForType: ?? => Option[Expr[String]]): Expr[String] =
      Type.runtimeShortPrint(overrideForType)(using tpe)

    def position: Option[Position] = Type.position(using tpe)

    def getRuntimeClass: Option[java.lang.Class[A]] = Type.classOfType(using tpe)

    def primaryConstructor: Option[Method { type Instance = A }] =
      Method.primaryConstructorOf(using tpe).map(_.asInstanceOf[Method { type Instance = A }])
    def defaultConstructor: Option[Method { type Instance = A }] = constructors.find(_.isNullary)
    def constructors: List[Method { type Instance = A }] =
      Method.constructorsOf(using tpe).map(_.asInstanceOf[Method { type Instance = A }])

    /** All methods of `A` in a stable, deterministic order - see [[Method.methodsOf]]. Costlier than
      * [[unsortedMethods]]; prefer that unless you rely on ordering.
      */
    def methods: List[Method { type Instance = A }] =
      Method.methodsOf(using tpe).map(_.asInstanceOf[Method { type Instance = A }])

    /** All methods of `A` in raw discovery order, without the expensive position-resolving sort - see
      * [[Method.unsortedMethodsOf]]. Recommended when the result is only searched/filtered by name.
      */
    def unsortedMethods: List[Method { type Instance = A }] =
      Method.unsortedMethodsOf(using tpe).map(_.asInstanceOf[Method { type Instance = A }])

    /** Only the methods named `name`, converting just the matches - see [[Method.unsortedMethodsNamed]]. */
    def unsortedMethodsNamed(name: String): List[Method { type Instance = A }] =
      Method.unsortedMethodsNamed(name)(using tpe).map(_.asInstanceOf[Method { type Instance = A }])

    def companionObject: Option[Expr_??] = Type.companionObject(using tpe)

    def directChildren: Option[ListMap[String, ??<:[A]]] = Type.directChildren(using tpe)
    def exhaustiveChildren: Option[NonEmptyMap[String, ??<:[A]]] = Type.exhaustiveChildren(using tpe)

    def annotations: List[Expr_??] = Type.annotations(using tpe)
    def annotationsOfType[Ann: Type]: List[Expr[Ann]] = Type.annotationsOfType[A, Ann](using tpe, Type[Ann])
    def typeAnnotations: List[Expr_??] = Type.typeAnnotations(using tpe)
    def typeAnnotationsOfType[Ann: Type]: List[Expr[Ann]] = Type.typeAnnotationsOfType[A, Ann](using tpe, Type[Ann])
    def hasAnnotationOfType[Ann: Type]: Boolean = Type.hasAnnotationOfType[A, Ann](using tpe, Type[Ann])

    def summonExpr: SummoningResult[A] = Expr.summonImplicit(using tpe)
    def summonExprIgnoring(excluded: UntypedMethod*): SummoningResult[A] =
      Expr.summonImplicitIgnoring(excluded*)(using tpe)

    def isPrimitive: Boolean = Type.isPrimitive(using tpe)
    def isArray: Boolean = Type.isArray(using tpe)
    def isIArray: Boolean = Type.isIArray(using tpe)
    def isInJavaLangPackage: Boolean = Type.isInJavaLangPackage(using tpe)
    def isJvmBuiltIn: Boolean = Type.isJvmBuiltIn(using tpe)
    def isTypeSystemSpecial: Boolean = Type.isTypeSystemSpecial(using tpe)
    def isOpaqueType: Boolean = Type.isOpaqueType(using tpe)
    def opaqueUnderlyingType: Option[??] = Type.opaqueUnderlyingType(using tpe)
    def isTuple: Boolean = Type.isTuple(using tpe)
    def isNamedTuple: Boolean = Type.isNamedTuple(using tpe)
    def isUnionType: Boolean = Type.isUnionType(using tpe)
    def unionMemberRequiresTypeTest[B: Type]: Boolean = Type.unionMemberRequiresTypeTest[A, B](using tpe, Type[B])
    def unionRefusalReason: Option[String] = Type.unionRefusalReason(using tpe)

    def isAbstract: Boolean = Type.isAbstract(using tpe)
    def isFinal: Boolean = Type.isFinal(using tpe)
    def isTrait: Boolean = Type.isTrait(using tpe)

    def isClass: Boolean = Type.isClass(using tpe)
    def notJvmBuiltInClass: Boolean = Type.notJvmBuiltInClass(using tpe)
    def isPlainOldJavaObject: Boolean = Type.isPlainOldJavaObject(using tpe)
    def isJavaBean: Boolean = Type.isJavaBean(using tpe)
    def isJavaBean(visibility: Accessible): Boolean = Type.isJavaBean(visibility)(using tpe)

    def isSealed: Boolean = Type.isSealed(using tpe)
    def isJavaEnum: Boolean = Type.isJavaEnum(using tpe)
    def isJavaEnumValue: Boolean = Type.isJavaEnumValue(using tpe)
    def isEnumeration: Boolean = Type.isEnumeration(using tpe)

    def isCase: Boolean = Type.isCase(using tpe)
    def isObject: Boolean = Type.isObject(using tpe)
    def isVal: Boolean = Type.isVal(using tpe)

    def isCaseClass: Boolean = Type.isCaseClass(using tpe)
    def isCaseObject: Boolean = Type.isCaseObject(using tpe)
    def isCaseVal: Boolean = Type.isCaseVal(using tpe)

    def isAvailable(scope: Accessible): Boolean = Type.isAvailable[A](scope)(using tpe)

    def parents: List[??] = Type.parents(using tpe)
    def baseClasses: List[??] = Type.baseClasses(using tpe)

    def <:<[B](tpe2: Type[B]): Boolean = Type.isSubtypeOf(using tpe, tpe2)
    def =:=[B](tpe2: Type[B]): Boolean = Type.isSameAs(using tpe, tpe2)

    def asUntyped: UntypedType = UntypedType.fromTyped(using tpe)

    def as_?? : ?? = Existential[Type, A](tpe)(using tpe)
    def as_??>:[L <: A]: ??>:[L] = Existential.LowerBounded[L, Type, A](tpe)(using tpe)
    def as_??<:[U >: A]: ??<:[U] = Existential.UpperBounded[U, Type, A](tpe)(using tpe)
    def as_<:??<:[L <: A, U >: A]: L <:??<: U = Existential.Bounded[L, U, Type, A](tpe)(using tpe)
  }

  // Aliases to make the (very common) existential types shorter

  final type ?? = Existential[Type]
  final type ??>:[L] = Existential.LowerBounded[L, Type]
  final type ??<:[U] = Existential.UpperBounded[U, Type]
  final type <:??<:[L, U >: L] = Existential.Bounded[L, U, Type]

  implicit final def ExistentialTypeMethods(tpe: ??): BoundedExistentialTypeMethods[Nothing, Any] =
    new BoundedExistentialTypeMethods[Nothing, Any](tpe)
  implicit final def LowerBoundedExistentialTypeMethods[L](tpe: ??>:[L]): BoundedExistentialTypeMethods[L, Any] =
    new BoundedExistentialTypeMethods[L, Any](tpe)
  implicit final def UpperBoundedExistentialTypeMethods[U](tpe: ??<:[U]): BoundedExistentialTypeMethods[Nothing, U] =
    new BoundedExistentialTypeMethods[Nothing, U](tpe)
  implicit final class BoundedExistentialTypeMethods[L, U >: L](private val tpe: L <:??<: U) {

    def shortName: String = Type.shortName(using tpe.Underlying)
    def fqcn: String = Type.fqcn(using tpe.Underlying)
    def plainPrint: String = Type.plainPrint(using tpe.Underlying)
    def prettyPrint: String = Type.prettyPrint(using tpe.Underlying)

    def runtimePlainPrint(overrideForType: ?? => Option[Expr[String]]): Expr[String] =
      Type.runtimePlainPrint(overrideForType)(using tpe.Underlying)
    def runtimePrettyPrint(overrideForType: ?? => Option[Expr[String]]): Expr[String] =
      Type.runtimePrettyPrint(overrideForType)(using tpe.Underlying)
    def runtimeShortPrint(overrideForType: ?? => Option[Expr[String]]): Expr[String] =
      Type.runtimeShortPrint(overrideForType)(using tpe.Underlying)

    def asUntyped: UntypedType = UntypedType.fromTyped(using tpe.Underlying)
  }

  /** Generalizes over the idea of conversion between singleton/literal type values and their type representation.
    *
    * @since 0.1.0
    */
  trait TypeCodec[U] {

    def toType[A <: U](value: A): Type[A]
    def fromType[A](tpe: Type[A]): Option[Existential.UpperBounded[U, Id]]
  }
  object TypeCodec extends TypeCodecCompat {

    def apply[A](implicit codec: TypeCodec[A]): TypeCodec[A] = codec

    implicit lazy val NullCodec: TypeCodec[Null] = Type.NullCodec
    implicit lazy val UnitCodec: TypeCodec[Unit] = Type.UnitCodec
    implicit lazy val BooleanCodec: TypeCodec[Boolean] = Type.BooleanCodec
    implicit lazy val ByteCodec: TypeCodec[Byte] = Type.ByteCodec
    implicit lazy val ShortCodec: TypeCodec[Short] = Type.ShortCodec
    implicit lazy val IntCodec: TypeCodec[Int] = Type.IntCodec
    implicit lazy val LongCodec: TypeCodec[Long] = Type.LongCodec
    implicit lazy val FloatCodec: TypeCodec[Float] = Type.FloatCodec
    implicit lazy val DoubleCodec: TypeCodec[Double] = Type.DoubleCodec
    implicit lazy val CharCodec: TypeCodec[Char] = Type.CharCodec
    implicit lazy val StringCodec: TypeCodec[String] = Type.StringCodec

    implicit def ClassCodec[A: Type]: TypeCodec[java.lang.Class[A]] = Type.ClassCodec[A]
    implicit def ClassTagCodec[A: Type]: TypeCodec[scala.reflect.ClassTag[A]] = Type.ClassTagCodec[A]

    implicit def ArrayCodec[A: Type: TypeCodec]: TypeCodec[Array[A]] = Type.ArrayCodec[A]
    implicit def SeqCodec[A: Type: TypeCodec]: TypeCodec[Seq[A]] = Type.SeqCodec[A]
    implicit def ListCodec[A: Type: TypeCodec]: TypeCodec[List[A]] = Type.ListCodec[A]
    implicit lazy val NilCodec: TypeCodec[Nil.type] = Type.NilCodec
    implicit def VectorCodec[A: Type: TypeCodec]: TypeCodec[Vector[A]] = Type.VectorCodec[A]
    implicit def MapCodec[K: Type: TypeCodec, V: Type: TypeCodec]: TypeCodec[Map[K, V]] = Type.MapCodec[K, V]
    implicit def SetCodec[A: Type: TypeCodec]: TypeCodec[Set[A]] = Type.SetCodec[A]
    implicit def OptionCodec[A: TypeCodec]: TypeCodec[Option[A]] = Type.OptionCodec[A]
    implicit def SomeCodec[A: TypeCodec]: TypeCodec[Some[A]] = Type.SomeCodec[A]
    implicit lazy val NoneCodec: TypeCodec[None.type] = Type.NoneCodec
    implicit def EitherCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Either[L, R]] = Type.EitherCodec[L, R]
    implicit def LeftCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Left[L, R]] = Type.LeftCodec[L, R]
    implicit def RightCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Right[L, R]] = Type.RightCodec[L, R]

    implicit def Tuple1Codec[T1: TypeCodec]: TypeCodec[Tuple1[T1]] = Type.Tuple1Codec[T1]
    implicit def Tuple2Codec[T1: TypeCodec, T2: TypeCodec]: TypeCodec[(T1, T2)] = Type.Tuple2Codec[T1, T2]
    implicit def Tuple3Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec]: TypeCodec[(T1, T2, T3)] =
      Type.Tuple3Codec[T1, T2, T3]
    implicit def Tuple4Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec]: TypeCodec[(T1, T2, T3, T4)] =
      Type.Tuple4Codec[T1, T2, T3, T4]
    implicit def Tuple5Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5)] =
      Type.Tuple5Codec[T1, T2, T3, T4, T5]
    implicit def Tuple6Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec, T6: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5, T6)] =
      Type.Tuple6Codec[T1, T2, T3, T4, T5, T6]
    implicit def Tuple7Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7)] =
      Type.Tuple7Codec[T1, T2, T3, T4, T5, T6, T7]
    implicit def Tuple8Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8)] =
      Type.Tuple8Codec[T1, T2, T3, T4, T5, T6, T7, T8]
    implicit def Tuple9Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
      Type.Tuple9Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    implicit def Tuple10Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
      Type.Tuple10Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    implicit def Tuple11Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
      Type.Tuple11Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    implicit def Tuple12Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
      Type.Tuple12Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    implicit def Tuple13Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
      Type.Tuple13Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    implicit def Tuple14Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
      Type.Tuple14Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    implicit def Tuple15Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
      Type.Tuple15Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    implicit def Tuple16Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
      Type.Tuple16Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
    implicit def Tuple17Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
      Type.Tuple17Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    implicit def Tuple18Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
      Type.Tuple18Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    implicit def Tuple19Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
      Type.Tuple19Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    implicit def Tuple20Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
      Type.Tuple20Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    implicit def Tuple21Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec
    ]: TypeCodec[
      (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)
    ] =
      Type.Tuple21Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    implicit def Tuple22Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec,
        T22: TypeCodec
    ]: TypeCodec[
      (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
    ] =
      Type.Tuple22Codec[
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
        T18,
        T19,
        T20,
        T21,
        T22
      ]

    implicit def ModuleCodec[ModuleSingleton <: Singleton]: TypeCodec[ModuleSingleton] =
      Type.ModuleCodec[ModuleSingleton]
  }
}
