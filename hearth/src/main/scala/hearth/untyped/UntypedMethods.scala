package hearth
package untyped

import scala.collection.immutable.ListMap

trait UntypedMethods { this: MacroCommons =>

  /** Defines how we should call the method.
    *
    * As an user you shouldn't be concerned with this, but it is important when generating [[Expr]] of a method call,
    * since AST is different for constructors and normal methods calls, and we don't want to require passing an instance
    * if something is a static method or package object method.
    *
    * Internal utilities like [[UntypedMethod.unsafeApply]] can use this to generate the right AST, but it is
    * recommended to use safe `apply` on pattern-matched typed [[Method]] instead.
    *
    * @since 0.1.0
    */
  sealed trait Invocation extends Product with Serializable
  object Invocation {
    sealed trait WithoutInstance extends Invocation

    case object Constructor extends WithoutInstance
    final case class OnModule(module: UntypedExpr) extends WithoutInstance
    case object OnInstance extends Invocation
  }

  /** Platform-specific untyped type parameter representation (a type parameter `Symbol` on both platforms).
    *
    * Follows the same abstract type + module pattern as [[UntypedType]] and [[UntypedExpr]].
    *
    * @since 0.4.0
    */
  type UntypedTypeParameter

  val UntypedTypeParameter: UntypedTypeParameterModule
  trait UntypedTypeParameterModule { this: UntypedTypeParameter.type =>
    def name(param: UntypedTypeParameter): String
    def upperBound(param: UntypedTypeParameter): UntypedType
    def lowerBound(param: UntypedTypeParameter): UntypedType
    def upperBoundPrint(param: UntypedTypeParameter): String =
      scala.util.Try(UntypedType.plainPrint(upperBound(param))).getOrElse(upperBound(param).toString)
    def lowerBoundPrint(param: UntypedTypeParameter): String =
      scala.util.Try(UntypedType.plainPrint(lowerBound(param))).getOrElse(lowerBound(param).toString)
  }

  /** Type parameters grouped as they appear in the method signature (e.g. `[A, B][C]` → `List(List(A, B), List(C))`).
    *
    * @since 0.4.0
    */
  type UntypedTypeParameters = List[List[UntypedTypeParameter]]

  /** Map from type parameters to their resolved types, used when applying type arguments.
    *
    * @since 0.4.0
    */
  type UntypedTypeArguments = Map[UntypedTypeParameter, ??]

  /** Describes what one step of the method builder chain expects.
    *
    * Computed by platform code from the method's type signature. Not user-facing — users work with the typed
    * [[MethodExpectation]] in the typed layer.
    *
    * @since 0.4.0
    */
  sealed trait UntypedMethodExpectation extends Product with Serializable
  object UntypedMethodExpectation {
    case object NeedsInstance extends UntypedMethodExpectation
    final case class NeedsTypes(typeParams: UntypedTypeParameters) extends UntypedMethodExpectation
    final case class NeedsValues(params: UntypedParameters) extends UntypedMethodExpectation
  }

  /** Platform-specific untyped parameter representation (`c.universe.TermSymbol` in 2, `quotes.reflect.Symbol` in 3)
    * together with some helper data, an [[UntypedMethod]] to which it belongs, or its index in the method's parameters
    * (useful if the parameter has a default value).
    *
    * Does not resolve the type of the parameter, yet, because it might depend on the instance's type parameters,
    * method's type parameters, etc.
    *
    * @since 0.1.0
    */
  type UntypedParameter <: UntypedParameterMethods

  val UntypedParameter: UntypedParameterModule
  trait UntypedParameterModule { this: UntypedParameter.type =>

    final def fromTyped(param: Parameter): UntypedParameter = param.asUntyped
  }

  trait UntypedParameterMethods { this: UntypedParameter =>

    def name: String
    def index: Int
    def position: Option[Position]
    def annotations: List[UntypedExpr]

    /** The types of this parameter's [[annotations]], captured separately so the typed layer can re-tag each annotation
      * expression with its type.
      *
      * Exists for the Scala 2 annotation-type-preservation fix: `c.untypecheck(ann.tree)` (required before splicing an
      * annotation tree) strips its type, so the type is recorded here beforehand. See CLAUDE.md "Annotation type
      * preservation".
      *
      * @since 0.4.0
      */
    def annotationTypes: List[UntypedType]

    def isByName: Boolean
    def isVararg: Boolean
    def isImplicit: Boolean
    def hasDefault: Boolean

    /** For a by-name parameter (`a: => A`), the underlying type `A`; `None` for any non-by-name parameter.
      *
      * Normalizes a platform difference: on Scala 2 a by-name parameter's type is the applied wrapper `<byname>[A]` (so
      * `A` is recoverable as its sole type argument), while on Scala 3 it is a `ByNameType(A)` which is NOT an applied
      * type (so `typeArguments` returns `Nil`). This surfaces `A` uniformly across platforms.
      *
      * @since 0.4.0
      */
    def byNameUnderlying: Option[UntypedType]

    final def default(instanceTpe: UntypedType): Option[UntypedMethod] = UntypedMethod.defaultValue(instanceTpe)(this)
  }

  /** Ordered map of [[UntypedParameter]]s by their name.
    *
    * Parameters are grouped just like the parameters list they represent.
    *
    * @since 0.1.0
    */
  type UntypedParameters = List[ListMap[String, UntypedParameter]]

  val UntypedParameters: UntypedParametersModule
  trait UntypedParametersModule { this: UntypedParameters.type =>

    final def fromTyped(typed: Parameters): UntypedParameters = typed.map { inner =>
      ListMap.from(inner.view.mapValues(_.asUntyped))
    }

    def toTyped[Instance: Type](untyped: UntypedParameters): Parameters
  }

  implicit final class UntypedParametersMethods(private val parameters: UntypedParameters) {

    def asTyped[Instance: Type]: Parameters = UntypedParameters.toTyped(parameters)
  }

  /** Map of argument values by their names.
    *
    * This map is flat because arguments would be matched by their name, so the order in which we have them here is
    * irrelevent, the important part is that all non-optional arguments should be present and have the right type.
    *
    * While it does not not verify if all arguments are present and if they have the expected type, it is expected in
    * [[UntypedMethod.unsafeApply]] that [[UntypedExpr]]s represent the values of the correct type.
    *
    * @since 0.1.0
    */
  type UntypedArguments = Map[String, UntypedExpr]
  object UntypedArguments {

    final def fromTyped(typed: Arguments): UntypedArguments = typed.view.mapValues(_.value.asUntyped).toMap // wtf?
    final def toTyped(untyped: UntypedArguments): Arguments = untyped.view.mapValues(_.as_??).toMap
  }

  implicit final class UntypedArgumentsMethods(private val arguments: UntypedArguments) {

    def asTyped[Instance: Type]: Arguments = UntypedArguments.toTyped(arguments)

    def adaptToParams(
        instanceTpe: UntypedType,
        instance: Option[UntypedExpr],
        method: UntypedMethod
    ): List[List[UntypedExpr]] =
      method.parameters.map { params =>
        params.view.map { case (paramName, untyped) =>
          def defaultValue = untyped.default(instanceTpe).map { method =>
            // Default value is called on the same instance as it's method, and without any arguments
            method.unsafeApply(instanceTpe)(instance, Map.empty)
          }
          val provided = arguments.get(paramName).orElse(defaultValue).getOrElse {
            // $COVERAGE-OFF$
            hearthRequirementFailed(
              s"""Expected that ${instanceTpe.prettyPrint}'s ${method.name} parameter `$paramName` would be provided or have a default value.
                 |Ensure that all arguments are provided or have a default value.""".stripMargin
            )
            // $COVERAGE-ON$
          }
          // Vararg (repeated) parameters expect an Expr[Seq[A]] argument - splice it as `seq: _*`.
          if (untyped.isVararg) adaptVarargArgument(provided) else provided
        }.toList
      }
  }

  /** Platform-specific: wraps an `Expr[Seq[A]]` argument so that it can be spliced into a vararg (repeated) parameter
    * position - the equivalent of writing `method(seq: _*)` (Scala 2) / `method(seq*)` (Scala 3) in source code.
    *
    * Used by [[UntypedArgumentsMethods.adaptToParams]] when the corresponding [[UntypedParameter]] `isVararg`.
    *
    * @since 0.4.0
    */
  protected def adaptVarargArgument(expr: UntypedExpr): UntypedExpr

  /** Platform-specific method representation (`c.universe.MethodSymbol` in 2, `quotes.reflect.Symbol` in 3).
    *
    * @since 0.1.0
    */
  type UntypedMethod <: UntypedMethodMethods

  val UntypedMethod: UntypedMethodModule
  trait UntypedMethodModule { this: UntypedMethod.type =>

    final def fromTyped(method: Method): UntypedMethod = method.asUntyped
    def toTyped[Instance: Type](untyped: UntypedMethod): Method

    /** The untyped discovery primitives behind the typed [[Method.primaryConstructorOf]]/[[Method.constructorsOf]]/
      * [[Method.methodsOf]] entry points and the [[AnonymousInstance]] method classification: respectively the primary
      * constructor of `instanceTpe` (if any), all of its constructors, and all of its methods.
      *
      * @since 0.1.0
      */
    def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod]
    def constructors(instanceTpe: UntypedType): List[UntypedMethod]
    def methods(instanceTpe: UntypedType): List[UntypedMethod]

    def defaultValue(instanceTpe: UntypedType)(param: UntypedParameter): Option[UntypedMethod]

    def enclosing: Option[UntypedMethod]

    /** We want to keep sorting consistent between Scala 2 and 3, and make it reasonable for both.
      *
      * The chanllenges are:
      *   - we want to keep methods defined in the type before all inherited or synthetic methods
      *   - we want to keep declared methods in their source order... but [[Position]] can be empty
      *   - inherited methods can come from multiple parental classes, sorting them primarily by position doesn't make
      *     sense (especially if some of these parents might give us empty [[Position]]s)
      *   - so for non-declared methods we sort by name, but for declared methods we sort by position
      *
      * Constructor arguments come first, ordered by their position in the primary constructor. Then declared
      * non-constructor-argument methods, ordered by source position. Then inherited/synthetic methods, ordered by name
      * using [[hearth.fp.NaturalLanguageOrdering]] (which handles _1, _2, ..., _10, _11 correctly).
      */
    final protected def sortMethods(methods: List[UntypedMethod]): List[UntypedMethod] = {
      val (ctorArgs, rest) = methods.partition(_.isConstructorArgument)
      val sortedCtorArgs = ctorArgs.sortBy(_.constructorArgumentIndex.getOrElse(Int.MaxValue))

      val (declared, others) = rest.partitionMap { method =>
        method.position match {
          case Some(position) if method.isDeclared => Left(position -> method)
          case _                                   => Right(method)
        }
      }
      val sortedDeclared = declared.sortBy(_._1).map(_._2)
      val sortedOthers = others.sortBy(_.name)(hearth.fp.NaturalLanguageOrdering.caseInsensitive)

      sortedCtorArgs ++ sortedDeclared ++ sortedOthers
    }

    // Defaults methods' positions are 1-indexed. They are named `methodName$default$indexOfParameter`.

    final protected def defaultValueMethodName(methodName: String, idx: Int): String = methodName + "$default$" + idx

    final protected val possibleConstructorNames: List[String] = List(
      "<init>", // Ctor of non-case class (no `apply`) has `<init>$default$idx` default (on Scala 2, unencoded).
      "apply", // Ctor of case class on Scala 2 has `apply$default$idx` default (= `apply` method).
      "$lessinit$greater" // Ctor of case class on Scala 3 has `$lessinit$greater$default$idx` (no `apply` but encoded).
    )
  }

  trait UntypedMethodMethods { this: UntypedMethod =>

    /** Resolves this untyped method against instance type `Instance`, producing a typed [[Method]] builder chain (its
      * parameter/return types resolved against `Instance`).
      *
      * @since 0.1.0
      */
    def asTyped[Instance: Type]: Method = UntypedMethod.toTyped(this)

    def invocation: Invocation

    def hasTypeParameters: Boolean
    def typeParameters: UntypedTypeParameters
    def parameters: UntypedParameters

    def methodExpectations(instanceTpe: UntypedType): List[UntypedMethodExpectation]

    /** Low-level, unchecked method application: builds the call AST directly, WITHOUT validating that all arguments are
      * present or well-typed. Prefer the safe typed API - `apply` on a pattern-matched [[Method]] variant, or
      * [[Method.fold]]/[[Method.foldF]] - which enforce the builder-chain contract.
      *
      * @since 0.1.0
      */
    def unsafeApply(instanceTpe: UntypedType)(instance: Option[UntypedExpr], arguments: UntypedArguments): UntypedExpr
    def unsafeApplyWithTypes(instanceTpe: UntypedType)(
        typeArgs: UntypedTypeArguments,
        instance: Option[UntypedExpr],
        arguments: UntypedArguments
    ): UntypedExpr
    final def unsafeApplyNoInstance(instanceTpe: UntypedType)(arguments: UntypedArguments): UntypedExpr =
      unsafeApply(instanceTpe)(None, arguments)
    final def unsafeApplyInstance(
        instanceTpe: UntypedType
    )(instance: UntypedExpr, arguments: UntypedArguments): UntypedExpr =
      unsafeApply(instanceTpe)(Some(instance), arguments)

    def name: String
    def position: Option[Position]

    def annotations: List[UntypedExpr]

    /** The types of this method's [[annotations]], captured separately so the typed layer can re-tag each annotation
      * expression with its type; needed for the Scala 2 annotation-type-preservation fix (see CLAUDE.md "Annotation
      * type preservation").
      *
      * @since 0.4.0
      */
    def annotationTypes: List[UntypedType]

    def isConstructor: Boolean

    def isConstructorArgument: Boolean
    def constructorArgumentIndex: Option[Int]

    /** Whether this method is a case-class field accessor.
      *
      * Singletons (case objects, parameterless Scala 3 enum cases) are NOT case-class fields - they are routed to
      * [[SingletonValue]] instead (hearth#311).
      *
      * @since 0.1.0
      */
    def isCaseField: Boolean

    /** Whether this member is a `val` / stable value member.
      *
      * This is not limited to concrete backing fields: it also holds for stable *deferred* (abstract) accessors and for
      * inherited constructor-`val` fields (hearth#326, hearth#327). Consequently an inherited public `val` surfaces as
      * `isVal && isInherited && isAvailable(...)`, rather than as the subclass's private param-accessor plumbing.
      *
      * @since 0.1.0
      */
    def isVal: Boolean
    def isVar: Boolean
    def isLazy: Boolean
    def isDef: Boolean

    /** Whether this member is declared directly on the instance type (as opposed to inherited or synthetic).
      *
      * `isInherited` is derived as `!isDeclared && !isSynthetic`.
      *
      * @since 0.1.0
      */
    def isDeclared: Boolean

    /** Whether this member was synthesized by the compiler (e.g. an accessor or `copy`/`apply` helper).
      *
      * @since 0.1.0
      */
    def isSynthetic: Boolean
    final def isInherited: Boolean = !isDeclared && !isSynthetic
    def isImplicit: Boolean

    def isFinal: Boolean
    def isAbstract: Boolean

    /** Whether this method overrides a member of a supertype.
      *
      * Cross-platform divergence: for `java.lang.Object` methods this is `true` on Scala 2 (they override members
      * inherited from `Any`) but `false` on Scala 3 (`Object` is the root class). Cross-platform derivation code must
      * not branch on this predicate for `Object` members.
      *
      * @since 0.1.0
      */
    def isOverride: Boolean

    /** Whether this member is (unqualified) `private`.
      *
      * Qualified visibility is normalized: `private[pkg]` yields `isPrivate == false` with
      * `privateWithin == Some("pkg")`. To detect qualified-private members, check [[privateWithin]] rather than
      * assuming `isPrivate` covers them.
      *
      * @since 0.1.0
      */
    def isPrivate: Boolean

    /** Whether this member is (unqualified) `protected`.
      *
      * As with [[isPrivate]], qualified visibility is normalized: `protected[pkg]` yields `isProtected == false` with
      * `protectedWithin == Some("pkg")`.
      *
      * @since 0.1.0
      */
    def isProtected: Boolean

    /** The enclosing package/type name of a qualified-private member (`private[pkg]` -> `Some("pkg")`), else `None`.
      *
      * @see
      *   [[isPrivate]] for the normalization contract
      *
      * @since 0.1.0
      */
    def privateWithin: Option[String]

    /** The enclosing package/type name of a qualified-protected member (`protected[pkg]` -> `Some("pkg")`), else
      * `None`.
      *
      * @see
      *   [[isProtected]] for the normalization contract
      *
      * @since 0.1.0
      */
    def protectedWithin: Option[String]

    /** Whether this member is reachable under the given [[Accessible]] scope.
      *
      * @since 0.1.0
      *
      * @param scope
      *   the accessibility scope to check against
      * @return
      *   `true` if the member is available in that scope
      */
    def isAvailable(scope: Accessible): Boolean

    def paramTypePrints(instanceTpe: UntypedType): (List[List[(String, String)]], String)
    def signatureSegments(instanceTpe: UntypedType): List[String]

    @scala.annotation.nowarn("msg=unused explicit parameter")
    def paramTypePrints(
        instanceTpe: UntypedType,
        hl: hearth.treeprinter.SyntaxHighlight
    ): (List[List[(String, String)]], String) =
      paramTypePrints(instanceTpe)
    @scala.annotation.nowarn("msg=unused explicit parameter")
    def signatureSegments(instanceTpe: UntypedType, hl: hearth.treeprinter.SyntaxHighlight): List[String] =
      signatureSegments(instanceTpe)

    /** Renders the method's full signature (modifiers/keywords, type and value parameter clauses, return type), colored
      * per the given [[hearth.treeprinter.SyntaxHighlight]].
      *
      * The shared engine behind [[plainPrint]] (uncolored, a stable comparison key) and [[prettyPrint]] (ANSI-colored
      * for display) - the same printing-trio pattern used by the `Expr`/`Type` printers.
      *
      * @since 0.4.0
      *
      * @param instanceTpe
      *   the type the method is rendered as a member of
      * @param hl
      *   the syntax highlighter controlling coloring (`plain` for none, `ANSI` for color)
      */
    @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
    final def renderSignature(instanceTpe: UntypedType, hl: hearth.treeprinter.SyntaxHighlight): String = {
      val sb = new StringBuilder
      val typeName = {
        val raw = instanceTpe.plainPrint
        if (hl.TypeDefColor.isEmpty) raw else hl.highlightTypeDef(raw)
      }
      val (_, returnType) = paramTypePrints(instanceTpe, hl)

      if (isConstructor) {
        sb.append(hl.highlightKeyword("new")).append(" ").append(typeName)
      } else {
        sb.append(typeName).append(": ")
        if (isPrivate) {
          sb.append(hl.highlightKeyword("private"))
          privateWithin.foreach(s => sb.append("[").append(hl.highlightTypeDef(s)).append("]"))
          sb.append(" ")
        } else if (isProtected) {
          sb.append(hl.highlightKeyword("protected"))
          protectedWithin.foreach(s => sb.append("[").append(hl.highlightTypeDef(s)).append("]"))
          sb.append(" ")
        } else {
          privateWithin.foreach(s =>
            sb.append(hl.highlightKeyword("private")).append("[").append(hl.highlightTypeDef(s)).append("] ")
          )
          protectedWithin.foreach(s =>
            sb.append(hl.highlightKeyword("protected")).append("[").append(hl.highlightTypeDef(s)).append("] ")
          )
        }
        if (isImplicit) sb.append(hl.highlightKeyword("implicit")).append(" ")
        if (isFinal) sb.append(hl.highlightKeyword("final")).append(" ")
        if (isAbstract && isOverride) sb.append(hl.highlightKeyword("abstract")).append(" ")
        if (isOverride) sb.append(hl.highlightKeyword("override")).append(" ")
        if (isVal) sb.append(hl.highlightKeyword("val")).append(" ")
        else if (isVar) sb.append(hl.highlightKeyword("var")).append(" ")
        else if (isLazy)
          sb.append(hl.highlightKeyword("lazy")).append(" ").append(hl.highlightKeyword("val")).append(" ")
        else sb.append(hl.highlightKeyword("def")).append(" ")
        sb.append(name)
      }

      signatureSegments(instanceTpe, hl).foreach(seg => sb.append(seg))

      if (!isConstructor && returnType.nonEmpty) {
        val _ = sb.append(": ").append(returnType)
      }

      sb.result()
    }

    final def plainPrint(instanceTpe: UntypedType): String =
      renderSignature(instanceTpe, hearth.treeprinter.SyntaxHighlight.plain)

    final def prettyPrint(instanceTpe: UntypedType): String =
      renderSignature(instanceTpe, hearth.treeprinter.SyntaxHighlight.ANSI)
  }

  implicit final lazy val UntypedMethodOrdering: Ordering[UntypedMethod] = {
    // Stabilize order in case of https://github.com/scala/scala3/issues/21672 (does not solve the warnings!)
    implicit val nameOrdering: Ordering[String] = hearth.fp.NaturalLanguageOrdering.caseInsensitive
    Ordering.by[UntypedMethod, (Option[Int], Option[Position], String)](m =>
      (m.constructorArgumentIndex, m.position, m.name)
    )
  }
}
