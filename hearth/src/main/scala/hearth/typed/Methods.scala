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
    * shape (e.g. find all generic methods, all path-dependent methods, etc.).
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
    * Pattern match to determine what the next step expects:
    *   - [[Method.OnInstance]] — needs an instance expression
    *   - [[Method.ApplyTypes]] — needs type arguments
    *   - [[Method.ApplyValues]] — needs value arguments
    *   - [[Method.Result]] — terminal, call `build()` to validate and produce the expression
    *
    * @since 0.4.0
    */
  sealed trait Method {
    val asUntyped: UntypedMethod
    val untypedInstanceType: UntypedType

    type Instance
    @ImportedCrossTypeImplicit
    implicit val Instance: Type[Instance]

    val expectations: List[MethodExpectation]

    def totalParameters: Parameters
    def parameters: Parameters

    def knownReturning: Option[??]

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
    final lazy val isOverride: Boolean = asUntyped.isOverride

    final lazy val isPrivate: Boolean = asUntyped.isPrivate
    final lazy val isProtected: Boolean = asUntyped.isProtected
    final lazy val privateWithin: Option[String] = asUntyped.privateWithin
    final lazy val protectedWithin: Option[String] = asUntyped.protectedWithin
    final def isAvailable(scope: Accessible): Boolean = asUntyped.isAvailable(scope)

    final lazy val isConstructorArgument: Boolean = asUntyped.isConstructorArgument
    final lazy val isCaseField: Boolean = asUntyped.isCaseField

    final lazy val isScalaGetter: Boolean = (isVal || isVar || isLazy) && !isScalaSetter
    final lazy val isScalaSetter: Boolean = (isUnary && name.endsWith("_="))
    final lazy val isScalaAccessor: Boolean = isScalaGetter || isScalaSetter

    final lazy val scalaAccessorName: Option[String] =
      if (isScalaGetter) Some(name) else if (isScalaSetter) Some(name.dropRight(2)) else None

    final lazy val isJavaGetter: Boolean =
      asUntyped.invocation == Invocation.OnInstance && isNullary &&
        knownReturning.exists { rt =>
          import rt.Underlying as R
          (name.startsWith("get") && name.length > 3 && !(R <:< Type.of[Unit])) ||
          (name.startsWith("is") && name.length > 2 && (R <:< Type.of[Boolean]))
        }
    final lazy val isJavaSetter: Boolean =
      asUntyped.invocation == Invocation.OnInstance && isUnary &&
        knownReturning.exists { rt =>
          import rt.Underlying as R
          name.startsWith("set") && name.length > 3 && (R <:< Type.of[Unit])
        }
    final lazy val isJavaAccessor: Boolean = isJavaGetter || isJavaSetter

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

    def primaryConstructorOf[A: Type]: Option[Method] =
      UntypedType.fromTyped[A].primaryConstructor.map(_.asTyped[A])
    def constructorsOf[A: Type]: List[Method] =
      UntypedType.fromTyped[A].constructors.map(_.asTyped[A])
    def methodsOf[A: Type]: List[Method] =
      UntypedType.fromTyped[A].methods.map(_.asTyped[A])

    private[hearth] def buildChain(
        asUntyped: UntypedMethod,
        untypedInstanceType: UntypedType,
        instanceEvidence: ??,
        expectations: List[MethodExpectation],
        totalParameters: Parameters,
        returnType: Option[??],
        buildExpr: (Option[UntypedExpr], UntypedTypeArguments, UntypedArguments) => Either[String, UntypedExpr],
        pathDepResolvers: Map[Int, UntypedArguments => Parameters]
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
          val effectiveReturnType = returnType.orElse {
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
                  val nextState = appliedState.copy(steps = appliedState.steps :+ AppliedStep.Types(typeArgs))
                  buildFromStep(expectations, stepIndex + 1, accInstance, typeArgs, accArgs, nextState)
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

    /** Needs an instance expression. Call `apply(instance)` to advance the chain.
      *
      * @since 0.4.0
      */
    final class OnInstance private[Methods] (
        val asUntyped: UntypedMethod,
        val untypedInstanceType: UntypedType,
        val expectations: List[MethodExpectation],
        val totalParameters: Parameters,
        private[Methods] val knownReturning0: Option[??],
        private val next: UntypedExpr => Method
    )(
        private[Methods] val instanceEvidence: ??,
        private[hearth] val appliedState: AppliedState
    ) extends Method {
      type Instance = instanceEvidence.Underlying
      @ImportedCrossTypeImplicit
      implicit val Instance: Type[Instance] = instanceEvidence.Underlying
      def parameters: Parameters = List.empty
      def knownReturning: Option[??] = knownReturning0
      def apply(instance: Expr[Instance]): Method = next(instance.asUntyped)
      def applyUntyped(instance: UntypedExpr): Method = next(instance)
    }
    object OnInstance {
      type Of[A] = OnInstance { type Instance = A }
    }

    /** Needs type arguments. Call `apply(typeArgs)` to advance the chain.
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

    /** Needs value arguments. Call `apply(arguments)` to advance the chain.
      *
      * @since 0.4.0
      */
    final class ApplyValues private[Methods] (
        val asUntyped: UntypedMethod,
        val untypedInstanceType: UntypedType,
        val expectations: List[MethodExpectation],
        val totalParameters: Parameters,
        private[Methods] val knownReturning0: Option[??],
        override val parameters: Parameters,
        private val next: UntypedArguments => Method
    )(
        private[Methods] val instanceEvidence: ??,
        private[hearth] val appliedState: AppliedState
    ) extends Method {
      type Instance = instanceEvidence.Underlying
      @ImportedCrossTypeImplicit
      implicit val Instance: Type[Instance] = instanceEvidence.Underlying
      def knownReturning: Option[??] = knownReturning0
      def apply(arguments: Arguments): Method = next(UntypedArguments.fromTyped(arguments))
    }

    /** Terminal step. All arguments have been accumulated. Call `build()` to validate and produce the expression.
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
      def build(): Either[String, Expr[Returned]] =
        validateAndBuild().map(_.asTyped[Returned])
    }
    object Result {
      type Of[A] = Result[A]
    }
  }

  /** Where do we need to access the method? (The use case, not the visibility). */
  sealed trait Accessible extends Product with Serializable

  /** Class/Method is public */
  case object Everywhere extends Accessible

  /** Class/Method might package private/protected modifier but we are in the same package */
  case object AtCallSite extends Accessible

  /** No accessibility requirement - every class/method matches, regardless of its visibility.
    *
    * Useful when we only want to enumerate members (e.g. all case fields) without filtering out the non-public ones.
    *
    * @since 0.4.0
    */
  case object Anywhere extends Accessible
}
