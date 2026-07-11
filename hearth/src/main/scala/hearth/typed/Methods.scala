package hearth
package typed

import scala.collection.immutable.ListMap

trait Methods { this: MacroCommons =>

  /** Represents a type parameter with resolved bounds.
    *
    * @since 0.4.0
    */
  final class TypeParameter(
      val asUntyped: UntypedTypeParameter,
      val upperBound: ??,
      val lowerBound: ??
  ) {
    final lazy val name: String = UntypedTypeParameter.name(asUntyped)
  }

  /** Type parameters grouped as they appear in the method signature.
    *
    * @since 0.4.0
    */
  type TypeParameters = List[List[TypeParameter]]

  /** Represents a method parameter with resolved type.
    *
    * Allows easier access to:
    *   - name
    *   - default value
    *   - annotations
    *   - resolved type
    *   - checking if parameter is by-name
    *   - checking if parameter is vararg (repeated)
    *   - checking if parameter is implicit
    *
    * For vararg parameters [[tpe]] is normalized to `scala.collection.immutable.Seq[A]` on both platforms (instead of
    * the platform-specific repeated-parameter marker type `A*`), and the argument passed for such parameter should be
    * an `Expr[Seq[A]]` - it will be spliced into the call as `seq: _*`.
    *
    * @since 0.1.0
    */
  final class Parameter(
      val asUntyped: UntypedParameter,
      val untypedInstanceType: UntypedType,
      val tpe: ??
  ) {

    lazy val name: String = asUntyped.name
    lazy val index: Int = asUntyped.index
    lazy val position: Option[Position] = asUntyped.position

    lazy val hasDefault: Boolean = asUntyped.hasDefault

    /** The method that computes this parameter's default value, if it has one (`None` otherwise).
      *
      * The returned [[Method]] is a builder chain for the compiler-synthesized default-value method (e.g.
      * `Foo.apply$default$2`). For a generic enclosing class its type arguments are re-applied here, so a default whose
      * type mentions a class type parameter is resolved rather than left abstract.
      *
      * @since 0.4.0
      */
    lazy val defaultValue: Option[Method] = asUntyped.default(untypedInstanceType).map { untyped =>
      val method = untyped.asTyped(using untypedInstanceType.asTyped[Any])
      method match {
        case at: Method.ApplyTypes =>
          val instanceTypeArgs = UntypedType.typeArguments(untypedInstanceType)
          if (instanceTypeArgs.isEmpty) method
          else {
            val allTypeParams = at.typeParams.flatten
            val classTypeArgs: UntypedTypeArguments = allTypeParams
              .take(instanceTypeArgs.size)
              .zip(instanceTypeArgs)
              .map { case (param, arg) => param -> arg.as_?? }
              .toMap
            at.apply(classTypeArgs)
          }
        case other => other
      }
    }

    lazy val annotations: List[Expr_??] = asUntyped.annotations.zip(asUntyped.annotationTypes).map { case (expr, tpe) =>
      UntypedExpr.as_??(expr, tpe)
    }

    /** Annotations on this parameter whose type is a subtype of `Ann`, with each expression typed as `Expr[Ann]`.
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
    def annotationsOfType[Ann: Type]: List[Expr[Ann]] = Annotations.filterOfType[Ann](annotations)

    /** Whether this parameter has at least one annotation whose type is a subtype of `Ann`.
      *
      * @since 0.4.0
      */
    def hasAnnotationOfType[Ann: Type]: Boolean = annotations.exists(_.Underlying <:< Type[Ann])

    lazy val isByName: Boolean = asUntyped.isByName

    /** Whether the parameter is a vararg/repeated parameter (`xs: A*`).
      *
      * For such parameters [[tpe]] is normalized to `scala.collection.immutable.Seq[A]` on both platforms, and the
      * argument provided when calling the method (via [[Method.ApplyValues]], `fold`, etc.) should be an `Expr[Seq[A]]` -
      * it will be spliced into the call as `seq: _*`.
      *
      * @since 0.4.0
      */
    lazy val isVararg: Boolean = asUntyped.isVararg
    lazy val isImplicit: Boolean = asUntyped.isImplicit

    /** For a by-name parameter (`a: => A`), the underlying type `A` (typed); `None` for any non-by-name parameter.
      *
      * Useful for wiring/DI macros that need to satisfy a by-name parameter with a strict value of `A`: unlike [[tpe]]
      * (which is the by-name wrapper, represented differently on Scala 2 and 3), this recovers `A` uniformly across
      * platforms — on Scala 3 the parameter type is a `ByNameType(A)` whose `A` is not reachable via
      * `Type.typeArguments`.
      *
      * @since 0.4.0
      */
    lazy val byNameUnderlying: Option[??] = asUntyped.byNameUnderlying.map(UntypedType.as_??)
  }

  /** Ordered map of [[Parameter]]s by their name.
    *
    * Parameters are grouped just like the parameters list they represent.
    *
    * It is assumed that all type-parameters were already applied and all [[Parameter]]s have their types resolved.
    *
    * @since 0.1.0
    */
  type Parameters = List[ListMap[String, Parameter]]

  /** Map of argument values by their names.
    *
    * This map is flat because arguments would be matched by their name, so the order in which we have them here is
    * irrelevent, the important part is that all non-optional arguments should be present and have the right type.
    *
    * @since 0.1.0
    */
  type Arguments = Map[String, Expr_??]

  /** Describes what one step of the method builder chain expects (typed, user-facing).
    *
    * Exposed on every [[Method]] via `expectations`. Users can pattern-match on this to classify methods by their chain
    * shape (e.g. find all generic methods, all path-dependent methods, etc.). The order of the steps mirrors the
    * method's source clauses:
    *   - `NeedsInstance` — the receiver of a non-static, non-constructor method,
    *   - `NeedsTypes` — one type-parameter clause,
    *   - `NeedsValues` — one value-parameter clause.
    *
    * Because Scala 3 allows parameter-clause interleaving (`def f[A](a: A)[B](b: B)`) while Scala 2 does not, the
    * number and interleaving of `NeedsTypes`/`NeedsValues` steps can differ between platforms for the same logical
    * method - do not hard-code a fixed step layout.
    *
    * @since 0.4.0
    */
  sealed trait MethodExpectation extends Product with Serializable
  object MethodExpectation {
    case object NeedsInstance extends MethodExpectation
    final case class NeedsTypes(typeParameters: TypeParameters) extends MethodExpectation
    final case class NeedsValues(parameters: Parameters) extends MethodExpectation
  }

  /** Represents a method as a builder chain, where each variant partially applies one layer of arguments and returns
    * the next [[Method]] in the chain.
    *
    * A `Method` is NOT directly callable; it is a step in a builder chain that you advance one layer of arguments at a
    * time until you reach the terminal [[Method.Result]], whose `build()` validates the accumulated arguments and
    * produces the call expression:
    *
    * {{{
    * OnInstance -> ApplyTypes -> ApplyValues -> Result.build(): Either[String, Expr[Returned]]
    * }}}
    *
    * Do not assume a fixed shape: not every step is present (a nullary no-arg method may be a [[Method.Result]]
    * immediately; a constructor has no [[Method.OnInstance]]; Scala 3 clause interleaving can produce several
    * [[Method.ApplyTypes]]/[[Method.ApplyValues]] steps - see [[MethodExpectation]]). Pattern match to determine what
    * the next step expects, calling that variant's `apply(...)` to obtain the next [[Method]]:
    *   - [[Method.OnInstance]] — needs an instance expression
    *   - [[Method.ApplyTypes]] — needs type arguments
    *   - [[Method.ApplyValues]] — needs value arguments
    *   - [[Method.Result]] — terminal, call `build()` to validate and produce the expression
    *
    * Rather than driving the recursion by hand, prefer [[fold]] (or [[foldF]] for effectful callbacks), which walks the
    * whole chain in one pass, asking you to supply each layer of arguments:
    *
    * {{{
    * // Call `instance.methodName(args...)`, resolving each step of the chain in one pass:
    * val result: Either[String, Expr_??] = method.fold(
    *   onInstance = _  => instance.as_??,                 // supply the receiver
    *   onTypes    = at => Map.empty,                      // supply type args (keyed by at.typeParams)
    *   onValues   = av => av.parameters.flatten.map {     // supply value args by name
    *     case (name, param) => name -> argFor(param)
    *   }.toMap
    * )
    * result match {
    *   case Right(expr) => // expr: Expr_?? of the method's return type
    *   case Left(err)   => // application failed (missing/ill-typed args)
    * }
    * }}}
    *
    * Obtain `Method`s for a type via the discovery entry points [[Method.primaryConstructorOf]],
    * [[Method.constructorsOf]] and [[Method.methodsOf]] (or the higher-level [[Class]] views).
    *
    * @see
    *   [[fold]] and [[foldF]] for driving the chain
    *
    * @since 0.4.0
    */
  sealed trait Method {
    val asUntyped: UntypedMethod
    val untypedInstanceType: UntypedType

    type Instance
    @ImportedCrossTypeImplicit
    implicit val Instance: Type[Instance]

    /** Per-step description of what this method still needs applied; see [[MethodExpectation]].
      *
      * @since 0.4.0
      */
    val expectations: List[MethodExpectation]

    /** All value parameters of the whole method, across every clause.
      *
      * This is stable across the builder chain (it does not shrink as arguments are applied), so it is what you
      * enumerate to build the full argument map. Contrast with [[parameters]], which only lists the parameters the
      * CURRENT step still expects.
      *
      * @since 0.4.0
      */
    def totalParameters: Parameters

    /** The value parameters that the CURRENT step still expects.
      *
      * Empty on every step except a [[Method.ApplyValues]] step, where it holds exactly the parameters of the clause
      * about to be applied. Use [[totalParameters]] when you need every parameter of the whole method regardless of the
      * current chain position.
      *
      * @since 0.4.0
      */
    def parameters: Parameters

    /** The result type, when statically known (`None` until enough of the chain is applied to determine it).
      *
      * @since 0.4.0
      */
    def knownReturning: Option[??]

    /** Total number of value parameters across all clauses.
      *
      * @since 0.4.0
      */
    final lazy val arity: Int = totalParameters.flatten.size
    final def isNAry(n: Int): Boolean = arity == n
    final lazy val isNullary: Boolean = isNAry(0)
    final lazy val isUnary: Boolean = isNAry(1)
    final lazy val isBinary: Boolean = isNAry(2)

    final lazy val name: String = asUntyped.name
    final lazy val position: Option[Position] = asUntyped.position

    final lazy val annotations: List[Expr_??] =
      asUntyped.annotations.zip(asUntyped.annotationTypes).map { case (expr, tpe) =>
        UntypedExpr.as_??(expr, tpe)
      }

    /** Annotations on this method whose type is a subtype of `Ann`, with each expression typed as `Expr[Ann]`.
      *
      * Subtype (`<:<`) matching is used (rather than `=:=`) so that a whole annotation hierarchy can be matched by its
      * common base type. The constructor arguments of a matched annotation can be read with
      * [[Annotations.constructorArguments]].
      *
      * @since 0.4.0
      */
    final def annotationsOfType[Ann: Type]: List[Expr[Ann]] = Annotations.filterOfType[Ann](annotations)

    /** Whether this method has at least one annotation whose type is a subtype of `Ann`.
      *
      * @since 0.4.0
      */
    final def hasAnnotationOfType[Ann: Type]: Boolean = annotations.exists(_.Underlying <:< Type[Ann])

    final lazy val isConstructor: Boolean = asUntyped.isConstructor

    /** Kind/origin predicates forwarding [[UntypedMethod]]; see that trait for the exact cross-platform contract.
      *
      * In particular [[isVal]] follows [[UntypedMethodMethods.isVal]], which also treats stable deferred (abstract)
      * accessors and inherited constructor-`val` fields as vals.
      *
      * @see
      *   [[UntypedMethodMethods.isVal]]
      *
      * @since 0.1.0
      */
    final lazy val isVal: Boolean = asUntyped.isVal
    final lazy val isVar: Boolean = asUntyped.isVar
    final lazy val isLazy: Boolean = asUntyped.isLazy
    final lazy val isDef: Boolean = asUntyped.isDef
    final lazy val isImplicit: Boolean = asUntyped.isImplicit
    final lazy val isDeclared: Boolean = asUntyped.isDeclared
    final lazy val isSynthetic: Boolean = asUntyped.isSynthetic
    final lazy val isInherited: Boolean = asUntyped.isInherited

    final lazy val isFinal: Boolean = asUntyped.isFinal
    final lazy val isAbstract: Boolean = asUntyped.isAbstract

    /** Whether this method overrides a member of a supertype; forwards [[UntypedMethodMethods.isOverride]].
      *
      * Cross-platform divergence: for `java.lang.Object` methods this is `true` on Scala 2 (they override members
      * inherited from `Any`) but `false` on Scala 3 (`Object` is the root class). Cross-platform derivation code must
      * not branch on this predicate for `Object` members.
      *
      * @see
      *   [[UntypedMethodMethods.isOverride]]
      *
      * @since 0.1.0
      */
    final lazy val isOverride: Boolean = asUntyped.isOverride

    /** Whether this member is (unqualified) `private`; forwards [[UntypedMethodMethods.isPrivate]].
      *
      * Qualified visibility is normalized: `private[pkg]` yields `isPrivate == false` with
      * `privateWithin == Some("pkg")` (and likewise `protected[pkg]` for [[isProtected]]/[[protectedWithin]]). To
      * detect qualified-private members, check [[privateWithin]] rather than assuming `isPrivate` covers them.
      *
      * @see
      *   [[UntypedMethodMethods.isPrivate]]
      *
      * @since 0.1.0
      */
    final lazy val isPrivate: Boolean = asUntyped.isPrivate
    final lazy val isProtected: Boolean = asUntyped.isProtected
    final lazy val privateWithin: Option[String] = asUntyped.privateWithin
    final lazy val protectedWithin: Option[String] = asUntyped.protectedWithin

    /** Whether this member is reachable under the given [[Accessible]] scope.
      *
      * @since 0.1.0
      *
      * @param scope
      *   the accessibility scope to check against
      * @return
      *   `true` if the member is available in that scope
      */
    final def isAvailable(scope: Accessible): Boolean = asUntyped.isAvailable(scope)

    final lazy val isConstructorArgument: Boolean = asUntyped.isConstructorArgument

    /** Whether this method is a case-class field accessor.
      *
      * Singletons (case objects, parameterless Scala 3 enum cases) are NOT case-class fields - they are routed to
      * [[SingletonValue]] instead.
      *
      * @see
      *   [[UntypedMethodMethods.isCaseField]]
      *
      * @since 0.1.0
      */
    final lazy val isCaseField: Boolean = asUntyped.isCaseField

    /** Whether this method is a Scala accessor getter.
      *
      * Heuristic: a `val`/`var`/`lazy val` member ([[isVal]]/[[isVar]]/[[isLazy]]) that is not itself a setter. The
      * companion [[isScalaSetter]] recognizes the compiler's `x_=` setter (a unary method whose name ends in `_=`), and
      * [[isScalaAccessor]] is the union of the two.
      *
      * @since 0.4.0
      */
    final lazy val isScalaGetter: Boolean = (isVal || isVar || isLazy) && !isScalaSetter
    final lazy val isScalaSetter: Boolean = (isUnary && name.endsWith("_="))
    final lazy val isScalaAccessor: Boolean = isScalaGetter || isScalaSetter

    /** The field name behind a Scala accessor: the member name for a getter, or the name with the trailing `_=`
      * stripped for a setter; `None` if this is not a Scala accessor.
      *
      * @since 0.4.0
      */
    final lazy val scalaAccessorName: Option[String] =
      if (isScalaGetter) Some(name) else if (isScalaSetter) Some(name.dropRight(2)) else None

    /** Whether this method is a JavaBean getter.
      *
      * JavaBean naming heuristic: a nullary instance method named `getX` returning a non-`Unit` type, or named `isX`
      * returning `Boolean`. The companion [[isJavaSetter]] recognizes a unary instance `setX` method returning `Unit`,
      * and [[isJavaAccessor]] is the union of the two.
      *
      * @since 0.4.0
      */
    final lazy val isJavaGetter: Boolean = {
      // Name checks are hoisted before `knownReturning` so that the (deferred) return type is only resolved for
      // methods that actually look like accessors. The JavaBeans convention requires an UPPER-CASE character right
      // after the `get`/`is` prefix (`getName`, not `getters`; `isActive`, not `island`), otherwise the "property"
      // name would be a fragment of a plain word.
      lazy val getLike = name.startsWith("get") && name.length > 3 && name.charAt(3).isUpper
      lazy val isLike = name.startsWith("is") && name.length > 2 && name.charAt(2).isUpper
      asUntyped.invocation == Invocation.OnInstance && isNullary && (getLike || isLike) &&
      knownReturning.exists { rt =>
        import rt.Underlying as R
        (getLike && !(R <:< Type.of[Unit])) || (isLike && (R <:< Type.of[Boolean]))
      }
    }
    final lazy val isJavaSetter: Boolean =
      asUntyped.invocation == Invocation.OnInstance && isUnary && name.startsWith("set") && name.length > 3 &&
        name.charAt(3).isUpper && // JavaBeans convention: `setValue`, not `settle`
        knownReturning.exists { rt =>
          import rt.Underlying as R
          R <:< Type.of[Unit]
        }
    final lazy val isJavaAccessor: Boolean = isJavaGetter || isJavaSetter

    /** The property name behind a JavaBean accessor: the `get`/`is`/`set` prefix is dropped and the remainder
      * decapitalized (`getFooBar` -> `fooBar`); `None` if this is not a JavaBean accessor.
      *
      * @since 0.4.0
      */
    final lazy val javaAccessorName: Option[String] =
      if (isJavaGetter) Some {
        val n = if (name.startsWith("is")) name.drop(2) else name.drop(3)
        n.head.toLower.toString + n.tail
      }
      else if (isJavaSetter) Some {
        val n = name.drop(3)
        n.head.toLower.toString + n.tail
      }
      else None

    final lazy val isAccessor: Boolean = isScalaAccessor || isJavaAccessor

    final lazy val accessorName: Option[String] = scalaAccessorName.orElse(javaAccessorName)

    private[hearth] def appliedState: Method.AppliedState

    /** Applies every step of the builder chain in one pass, calling the supplied callback for each instance/type/value
      * clause, and returns the built expression or an error.
      *
      * This hides the manual `pattern match -> apply -> recurse -> Result.build()` recursion (see the [[Method]]
      * overview for the worked example). Each callback is invoked once per matching step, in chain order; steps that a
      * given method does not have are simply not visited.
      *
      * @see
      *   [[foldF]] for effectful callbacks
      *
      * @since 0.4.0
      *
      * @param onInstance
      *   supplies the receiver for a [[Method.OnInstance]] step
      * @param onTypes
      *   supplies the type arguments for a [[Method.ApplyTypes]] step (keyed by its `typeParams`)
      * @param onValues
      *   supplies the value arguments for a [[Method.ApplyValues]] step (keyed by parameter name)
      * @return
      *   the built call expression, or `Left` describing why application failed (missing/ill-typed args)
      */
    final def fold(
        onInstance: Method.OnInstance => Expr_??,
        onTypes: Method.ApplyTypes => UntypedTypeArguments,
        onValues: Method.ApplyValues => Arguments
    ): Either[String, Expr_??] = {
      @scala.annotation.tailrec
      def loop(current: Method): Either[String, Expr_??] = current match {
        case oi: Method.OnInstance =>
          val expr = onInstance(oi)
          loop(oi.applyUntyped(expr.value.asUntyped))
        case at: Method.ApplyTypes =>
          loop(at.apply(onTypes(at)))
        case av: Method.ApplyValues =>
          loop(av.apply(onValues(av)))
        case r: Method.Result[?] =>
          import r.Returned
          r.build().map(_.as_??)
      }
      loop(this)
    }

    /** Like [[fold]] but each callback is effectful in `F` (via [[hearth.fp.DirectStyle]]); short-circuits on the
      * effect.
      *
      * @see
      *   [[fold]] for the pure variant
      *
      * @since 0.4.0
      *
      * @tparam F
      *   the effect type the callbacks run in
      * @param onInstance
      *   supplies (in `F`) the receiver for a [[Method.OnInstance]] step
      * @param onTypes
      *   supplies (in `F`) the type arguments for a [[Method.ApplyTypes]] step
      * @param onValues
      *   supplies (in `F`) the value arguments for a [[Method.ApplyValues]] step
      * @return
      *   `F` of the built call expression, or `Left` describing why application failed
      */
    final def foldF[F[_]](
        onInstance: Method.OnInstance => F[Expr_??],
        onTypes: Method.ApplyTypes => F[UntypedTypeArguments],
        onValues: Method.ApplyValues => F[Arguments]
    )(implicit F: hearth.fp.DirectStyle[F]): F[Either[String, Expr_??]] = F.scoped { run =>
      @scala.annotation.tailrec
      def loop(current: Method): Either[String, Expr_??] = current match {
        case oi: Method.OnInstance =>
          val expr = run(onInstance(oi))
          loop(oi.applyUntyped(expr.value.asUntyped))
        case at: Method.ApplyTypes =>
          loop(at.apply(run(onTypes(at))))
        case av: Method.ApplyValues =>
          loop(av.apply(run(onValues(av))))
        case r: Method.Result[?] =>
          import r.Returned
          r.build().map(_.as_??)
      }
      loop(this)
    }

    final def plainPrint: String = asUntyped.plainPrint(untypedInstanceType)
    final def prettyPrint: String = asUntyped.prettyPrint(untypedInstanceType)

    @scala.annotation.nowarn("msg=unused value")
    final override def toString: String = {
      val signature = asUntyped.plainPrint(untypedInstanceType)
      val state = appliedState
      if (state.steps.isEmpty) signature
      else {
        val parts = new scala.collection.mutable.ListBuffer[String]

        state.steps.foreach {
          case Method.AppliedStep.Instance(expr) =>
            val exprStr = scala.util.Try(expr.as_??.plainPrint).getOrElse(expr.toString)
            parts += s"on $exprStr"
          case _ => ()
        }

        val appliedBuf = new StringBuilder
        state.steps.foreach {
          case Method.AppliedStep.Types(args) if args.nonEmpty =>
            appliedBuf.append(
              args
                .map { case (tp, tpe) =>
                  s"${UntypedTypeParameter.name(tp)} = ${tpe.plainPrint}"
                }
                .mkString("[", ", ", "]")
            )
          case Method.AppliedStep.Values(args, paramOrder) if args.nonEmpty =>
            val ordered = paramOrder.flatMap(n => args.get(n).map(n -> _))
            appliedBuf.append(
              ordered
                .map { case (n, e) =>
                  val eStr = scala.util.Try(e.as_??.plainPrint).getOrElse(e.toString)
                  s"$n = $eStr"
                }
                .mkString("(", ", ", ")")
            )
          case _ => ()
        }
        if (appliedBuf.nonEmpty) parts += s"applied ${appliedBuf.result()}"

        if (!state.hadKnownReturning) {
          knownReturning.foreach { rt =>
            val _ = parts += s"returning ${rt.plainPrint}"
          }
        }

        if (parts.isEmpty) signature
        else s"$signature (${parts.mkString(", ")})"
      }
    }

    override def equals(that: Any): Boolean = that match {
      case that: Method =>
        asUntyped == that.asUntyped && (untypedInstanceType =:= that.untypedInstanceType)
      case _ => false
    }
    override def hashCode: Int = asUntyped.hashCode
  }
  object Method {

    // Memoize the (per-expansion) untyped->typed conversion of a type's whole method list by the queried type. Resolving
    // a typed `Method` (`UntypedMethod.asTyped`) resolves the full signature (parameter types, substitutions,
    // expectations) and is not cheap; a caller that looks methods up per field (e.g. one getter per product field)
    // otherwise re-converts EVERY method on every call - O(fields x methods) per type. Keyed by `=:=` via Type.Cache
    // (per-expansion lifetime); a miss only recomputes, so a stale/partial cache can never yield a wrong result.
    private type CachedMethods[A] = List[Method]
    private lazy val methodsOfCache = new Type.Cache[CachedMethods]
    private lazy val unsortedMethodsOfCache = new Type.Cache[CachedMethods]

    sealed private[hearth] trait AppliedStep extends Product with Serializable
    private[hearth] object AppliedStep {
      final case class Instance(expr: UntypedExpr) extends AppliedStep
      final case class Types(args: UntypedTypeArguments) extends AppliedStep
      final case class Values(args: UntypedArguments, parameterOrder: List[String]) extends AppliedStep
    }

    final private[hearth] case class AppliedState(
        steps: List[AppliedStep] = Nil,
        hadKnownReturning: Boolean = false
    )

    /** Returns the primary constructor of `A` as a builder chain, if any.
      *
      * The recommended entry point for constructing an `A`; see [[Method]] for how to drive the returned chain.
      *
      * @since 0.4.0
      *
      * @tparam A
      *   the type whose primary constructor to resolve
      * @return
      *   the primary constructor as a [[Method]], or `None` if `A` has none
      */
    def primaryConstructorOf[A: Type]: Option[Method] =
      UntypedType.fromTyped[A].primaryConstructor.map(_.asTyped[A])

    /** Returns all constructors of `A`, each as a builder chain.
      *
      * @since 0.4.0
      *
      * @tparam A
      *   the type whose constructors to resolve
      * @return
      *   every constructor of `A` as a [[Method]]
      */
    def constructorsOf[A: Type]: List[Method] =
      UntypedType.fromTyped[A].constructors.map(_.asTyped[A])

    /** Returns all methods (declared, inherited and synthetic) of `A`, each as a builder chain resolved against `A`, in
      * a STABLE, deterministic order (see [[UntypedMethod.methods]]).
      *
      * The ordering is comparatively expensive (it resolves each declared method's source position). '''Prefer
      * [[unsortedMethodsOf]] unless you rely on the order''' - most callers only search by name.
      *
      * @since 0.4.0
      *
      * @tparam A
      *   the instance type the methods are resolved against
      * @return
      *   every method visible on `A` as a [[Method]], in deterministic order
      */
    def methodsOf[A: Type]: List[Method] =
      methodsOfCache.getOrPut(Type[A])(UntypedMethod.sortMethodsBy(untypedMethodsOf[A])(identity).map(_.asTyped[A]))

    /** Like [[methodsOf]] but in raw discovery order, WITHOUT the expensive position-resolving sort (see
      * [[UntypedMethod.unsortedMethods]]).
      *
      * Cheaper than [[methodsOf]]; the order is unspecified. '''Recommended''' whenever the result is only searched or
      * filtered by name (`find`/`collectFirst`/`filter`/`exists`) rather than consumed as a deterministic sequence.
      *
      * @since 0.4.1
      */
    def unsortedMethodsOf[A: Type]: List[Method] =
      unsortedMethodsOfCache.getOrPut(Type[A])(untypedMethodsOf[A].map(_.asTyped[A]))

    /** Only the methods of `A` named `name`, in raw discovery order — semantically
      * `unsortedMethodsOf[A].filter(_.name == name)`, but the name filter runs on the UNTYPED methods (a cheap symbol
      * name compare), so only the matching methods pay the expensive typed conversion (parameter and return type
      * resolution). Prefer this over [[methodsOf]]/[[unsortedMethodsOf]] + `filter`/`find` when looking methods up by
      * name (e.g. resolving one getter per field): a type with ~25 methods then converts ~1 instead of all.
      *
      * The untyped method list and each name's converted methods are memoized per type within the expansion.
      *
      * @since 0.4.1
      */
    def unsortedMethodsNamed[A: Type](name: String): List[Method] = {
      val lookup = namedLookupCache.getOrPut(Type[A])(new NamedLookup(untypedMethodsOf[A]))
      lookup.byName.getOrElseUpdate(name, lookup.untyped.iterator.filter(_.name == name).map(_.asTyped[A]).toList)
    }

    /** Sorts methods into the same STABLE order as [[methodsOf]] (constructor arguments by ctor position, then declared
      * methods by source position, then inherited/synthetic by natural-language name order).
      *
      * The sort key of each method is computed purely from that method, so `sort(xs.filter(p)) == sort(xs).filter(p)` —
      * the intended pattern is to FILTER the cheap [[unsortedMethodsOf]] listing first and sort only the small subset
      * that survived, paying the expensive position resolution per kept method instead of per listed method.
      *
      * @since 0.4.1
      */
    def sort[M <: Method](methods: List[M]): List[M] = sortBy(methods)(m => m)

    /** [[sort]] generalized over any element that can produce a [[Method]] key (e.g. name-method pairs).
      *
      * @since 0.4.1
      */
    def sortBy[M](methods: List[M])(toMethod: M => Method): List[M] =
      UntypedMethod.sortMethodsBy(methods)(m => toMethod(m).asUntyped)

    // The raw (unsorted) UNTYPED listing is the shared input of methodsOf/unsortedMethodsOf/unsortedMethodsNamed;
    // memoizing it separately means a type listed through more than one of those entry points walks its symbol
    // members only once per expansion.
    private type UntypedMethodsFor[A] = List[UntypedMethod]
    private lazy val untypedMethodsOfCache = new Type.Cache[UntypedMethodsFor]
    private def untypedMethodsOf[A: Type]: List[UntypedMethod] =
      untypedMethodsOfCache.getOrPut(Type[A])(UntypedType.fromTyped[A].unsortedMethods)

    final private class NamedLookup(val untyped: List[UntypedMethod]) {
      val byName = scala.collection.mutable.Map.empty[String, List[Method]]
    }
    private type NamedLookupFor[A] = NamedLookup
    private lazy val namedLookupCache = new Type.Cache[NamedLookupFor]

    private[hearth] def buildChain(
        asUntyped: UntypedMethod,
        untypedInstanceType: UntypedType,
        instanceEvidence: ??,
        expectations: List[MethodExpectation],
        totalParameters: Parameters,
        // Thunked so that converting a method does not pay for resolving its return type (memberType + widening);
        // only chain steps that are actually asked for `knownReturning` (or the terminal `Result`) force it. The
        // platform should memoize the thunk (close over a `lazy val`) so multiple steps share one resolution.
        returnType: Option[() => ??],
        buildExpr: (Option[UntypedExpr], UntypedTypeArguments, UntypedArguments) => Either[String, UntypedExpr],
        pathDepResolvers: Map[Int, UntypedArguments => Parameters],
        // [hearth#331] Given ALL type arguments applied so far, re-resolve the (still-static) expectations so that a
        // value/implicit clause following a type-parameter clause has the applied type arguments substituted into its
        // parameter types (`(implicit ev: Sync[F])` -> `(implicit ev: Sync[SomeEffect])`). The platform builds this;
        // the default is identity (used when there are no unresolved method type parameters).
        resolveExpectationsForTypeArgs: UntypedTypeArguments => List[MethodExpectation]
    ): Method = {
      val initialState = AppliedState(hadKnownReturning = returnType.isDefined)

      def buildFromStep(
          expectations: List[MethodExpectation],
          stepIndex: Int,
          accInstance: Option[UntypedExpr],
          accTypeArgs: UntypedTypeArguments,
          accArgs: UntypedArguments,
          appliedState: AppliedState
      ): Method =
        if (stepIndex >= expectations.length) {
          val effectiveReturnType = returnType.map(_.apply()).orElse {
            if (accTypeArgs.nonEmpty)
              buildExpr(accInstance, accTypeArgs, accArgs).toOption
                .map(expr => UntypedExpr.as_??(expr).Underlying.as_??)
            else None
          }
          effectiveReturnType match {
            case Some(rt) =>
              import rt.Underlying as Returned
              new Method.Result[Returned](
                asUntyped = asUntyped,
                untypedInstanceType = untypedInstanceType,
                expectations = expectations,
                totalParameters = totalParameters,
                validateAndBuild = () => buildExpr(accInstance, accTypeArgs, accArgs)
              )(instanceEvidence, Type[Returned], appliedState)
            case None =>
              new Method.Result[Nothing](
                asUntyped = asUntyped,
                untypedInstanceType = untypedInstanceType,
                expectations = expectations,
                totalParameters = totalParameters,
                validateAndBuild = () => buildExpr(accInstance, accTypeArgs, accArgs)
              )(instanceEvidence, Type.of[Nothing].asInstanceOf[Type[Nothing]], appliedState)
          }
        } else
          expectations(stepIndex) match {
            case MethodExpectation.NeedsInstance =>
              new Method.OnInstance(
                asUntyped = asUntyped,
                untypedInstanceType = untypedInstanceType,
                expectations = expectations,
                totalParameters = totalParameters,
                knownReturning0 = returnType,
                next = { instanceExpr =>
                  val nextState = appliedState.copy(steps = appliedState.steps :+ AppliedStep.Instance(instanceExpr))
                  buildFromStep(expectations, stepIndex + 1, Some(instanceExpr), accTypeArgs, accArgs, nextState)
                }
              )(instanceEvidence, appliedState)
            case MethodExpectation.NeedsTypes(_) =>
              new Method.ApplyTypes(
                asUntyped = asUntyped,
                untypedInstanceType = untypedInstanceType,
                expectations = expectations,
                totalParameters = totalParameters,
                typeParams = asUntyped.typeParameters,
                next = { typeArgs =>
                  val allTypeArgs = accTypeArgs ++ typeArgs
                  val nextState = appliedState.copy(steps = appliedState.steps :+ AppliedStep.Types(typeArgs))
                  // [hearth#331] Substitute the just-applied type arguments into the parameter types of any subsequent
                  // value/implicit clauses, so `onValues` sees e.g. `Sync[SomeEffect]` rather than an empty clause.
                  val resolvedExpectations = resolveExpectationsForTypeArgs(allTypeArgs)
                  buildFromStep(resolvedExpectations, stepIndex + 1, accInstance, allTypeArgs, accArgs, nextState)
                }
              )(instanceEvidence, appliedState)
            case MethodExpectation.NeedsValues(params) =>
              new Method.ApplyValues(
                asUntyped = asUntyped,
                untypedInstanceType = untypedInstanceType,
                expectations = expectations,
                totalParameters = totalParameters,
                knownReturning0 = returnType,
                parameters = params,
                next = { newArgs =>
                  val nextAccArgs = accArgs ++ newArgs
                  val paramOrder = params.flatMap(_.keys.toList)
                  val nextState = appliedState.copy(
                    steps = appliedState.steps :+ AppliedStep.Values(newArgs, paramOrder)
                  )
                  val nextExpectations = pathDepResolvers.get(stepIndex + 1) match {
                    case Some(resolver) =>
                      expectations.updated(
                        stepIndex + 1,
                        MethodExpectation.NeedsValues(resolver(newArgs))
                      )
                    case None => expectations
                  }
                  buildFromStep(nextExpectations, stepIndex + 1, accInstance, accTypeArgs, nextAccArgs, nextState)
                }
              )(instanceEvidence, appliedState)
          }

      buildFromStep(expectations, 0, None, Map.empty, Map.empty, initialState)
    }

    /** Needs an instance expression (the receiver of a non-static, non-constructor method).
      *
      * Call `apply(instance)` with an `Expr[Instance]` (or `applyUntyped` with an [[UntypedExpr]]) to supply the
      * receiver and advance the chain; the [[Instance]] type member is the receiver's type.
      *
      * @since 0.4.0
      */
    final class OnInstance private[Methods] (
        val asUntyped: UntypedMethod,
        val untypedInstanceType: UntypedType,
        val expectations: List[MethodExpectation],
        val totalParameters: Parameters,
        private[Methods] val knownReturning0: Option[() => ??],
        private val next: UntypedExpr => Method
    )(
        private[Methods] val instanceEvidence: ??,
        private[hearth] val appliedState: AppliedState
    ) extends Method {
      type Instance = instanceEvidence.Underlying
      @ImportedCrossTypeImplicit
      implicit val Instance: Type[Instance] = instanceEvidence.Underlying
      def parameters: Parameters = List.empty
      lazy val knownReturning: Option[??] = knownReturning0.map(_.apply())
      def apply(instance: Expr[Instance]): Method = next(instance.asUntyped)
      def applyUntyped(instance: UntypedExpr): Method = next(instance)
    }
    object OnInstance {

      /** An [[OnInstance]] step whose receiver type is `A`.
        *
        * @since 0.4.0
        */
      type Of[A] = OnInstance { type Instance = A }
    }

    /** Needs the type arguments of one type-parameter clause.
      *
      * The [[typeParams]] are the parameters this clause fills; call `apply(typeArgs)` with an [[UntypedTypeArguments]]
      * map keyed by those parameters to advance the chain.
      *
      * @since 0.4.0
      */
    final class ApplyTypes private[Methods] (
        val asUntyped: UntypedMethod,
        val untypedInstanceType: UntypedType,
        val expectations: List[MethodExpectation],
        val totalParameters: Parameters,
        val typeParams: UntypedTypeParameters,
        private val next: UntypedTypeArguments => Method
    )(
        private[Methods] val instanceEvidence: ??,
        private[hearth] val appliedState: AppliedState
    ) extends Method {
      type Instance = instanceEvidence.Underlying
      @ImportedCrossTypeImplicit
      implicit val Instance: Type[Instance] = instanceEvidence.Underlying
      def parameters: Parameters = List.empty
      def knownReturning: Option[??] = None
      def apply(typeArgs: UntypedTypeArguments): Method = next(typeArgs)
    }

    /** Needs the value arguments of one value-parameter clause.
      *
      * The [[parameters]] here are exactly the arguments this clause expects; call `apply(arguments)` with an
      * [[Arguments]] map keyed by parameter name to advance the chain.
      *
      * @since 0.4.0
      */
    final class ApplyValues private[Methods] (
        val asUntyped: UntypedMethod,
        val untypedInstanceType: UntypedType,
        val expectations: List[MethodExpectation],
        val totalParameters: Parameters,
        private[Methods] val knownReturning0: Option[() => ??],
        override val parameters: Parameters,
        private val next: UntypedArguments => Method
    )(
        private[Methods] val instanceEvidence: ??,
        private[hearth] val appliedState: AppliedState
    ) extends Method {
      type Instance = instanceEvidence.Underlying
      @ImportedCrossTypeImplicit
      implicit val Instance: Type[Instance] = instanceEvidence.Underlying
      lazy val knownReturning: Option[??] = knownReturning0.map(_.apply())
      def apply(arguments: Arguments): Method = next(UntypedArguments.fromTyped(arguments))
    }

    /** Terminal step of the builder chain. All arguments have been accumulated; call [[build]] to validate them and
      * produce the call expression.
      *
      * The [[Returned]] type member is the method's result type (possibly `Nothing` when it cannot be determined
      * statically).
      *
      * @since 0.4.0
      */
    final class Result[Returned0] private[Methods] (
        val asUntyped: UntypedMethod,
        val untypedInstanceType: UntypedType,
        val expectations: List[MethodExpectation],
        val totalParameters: Parameters,
        private val validateAndBuild: () => Either[String, UntypedExpr]
    )(
        private[Methods] val instanceEvidence: ??,
        @ImportedCrossTypeImplicit implicit val Returned: Type[Returned0],
        private[hearth] val appliedState: AppliedState
    ) extends Method {
      final type Returned = Returned0
      type Instance = instanceEvidence.Underlying
      @ImportedCrossTypeImplicit
      implicit val Instance: Type[Instance] = instanceEvidence.Underlying
      def parameters: Parameters = List.empty
      def knownReturning: Option[??] = Some(Returned.as_??)

      /** Validates all accumulated arguments and produces the call expression.
        *
        * @since 0.4.0
        *
        * @return
        *   the built expression of type [[Returned]], or `Left` describing why validation/building failed
        */
      def build(): Either[String, Expr[Returned]] =
        validateAndBuild().map(_.asTyped[Returned])
    }
    object Result {

      /** A [[Result]] whose statically known result type is `A`.
        *
        * @since 0.4.0
        */
      type Of[A] = Result[A]
    }
  }

  /** Where a class/method needs to be accessible from (the use case, not the member's own visibility).
    *
    * Passed as the `visibility` parameter to construction and enumeration APIs (e.g. [[CaseClass.construct]],
    * [[CaseClass.copyMethod]], [[CaseClass.caseFieldValuesAt]]) to decide which members are usable. The three cases
    * trade off safety against reach - see [[Everywhere]], [[AtCallSite]] and [[Anywhere]].
    *
    * @since 0.4.0
    */
  sealed trait Accessible extends Product with Serializable

  /** Requires the class/method to be public (reachable from everywhere).
    *
    * The strictest scope: only members with no visibility restriction match.
    *
    * @since 0.4.0
    */
  case object Everywhere extends Accessible

  /** Requires the class/method to be reachable at the macro-expansion point.
    *
    * A member may be `private[pkg]`/`protected` yet still match if the macro expands within a scope that can reach it.
    * This is the safe default for generated code, since the emitted access (e.g. `instance.field`) has to compile at
    * that point.
    *
    * @since 0.4.0
    */
  case object AtCallSite extends Accessible

  /** No accessibility requirement - every class/method matches, regardless of its visibility.
    *
    * Useful when we only want to enumerate members (e.g. all case fields) without filtering out the non-public ones.
    *
    * @since 0.4.0
    */
  case object Anywhere extends Accessible
}
