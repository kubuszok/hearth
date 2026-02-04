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

    def isByName: Boolean
    def isImplicit: Boolean
    def hasDefault: Boolean

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
          arguments.get(paramName).orElse(defaultValue).getOrElse {
            // $COVERAGE-OFF$
            hearthRequirementFailed(
              s"""Expected that ${instanceTpe.prettyPrint}'s ${method.name} parameter `$paramName` would be provided or have a default value.
                 |Ensure that all arguments are provided or have a default value.""".stripMargin
            )
            // $COVERAGE-ON$
          }
        }.toList
      }
  }

  /** Platform-specific method representation (`c.universe.MethodSymbol` in 2, `quotes.reflect.Symbol` in 3).
    *
    * @since 0.1.0
    */
  type UntypedMethod <: UntypedMethodMethods

  val UntypedMethod: UntypedMethodModule
  trait UntypedMethodModule { this: UntypedMethod.type =>

    final def fromTyped[Instance, Returned](method: Method[Instance, Returned]): UntypedMethod = method.asUntyped
    def toTyped[Instance: Type](untyped: UntypedMethod): Method.Of[Instance]

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
      *   - TODO: decide how we should order inherited methods with overloaded names
      */
    final protected def sortMethods(methods: List[UntypedMethod]): List[UntypedMethod] = {
      val (declared, others) = methods.partitionMap { method =>
        method.position match {
          case Some(position) if method.isDeclared => Left(position -> method)
          case _                                   => Right(method.name -> method)
        }
      }
      // TODO: Use smarter name sorting  (e.g. one that would keep _1, _2, ..., _10, _11, ...)
      // TODO: Check if declared is empty :/, if it is, and we have a Scala class.
      //       Probably we should filter by declared and keep the original order?
      declared.sortBy(_._1).map(_._2) ++ others.sortBy(_._1).map(_._2)
    }

    // Defaults methods' positions are 1-indexed. They are named `methodName$default$indexOfParameter`.

    final protected def defaultValueMethodName(methodName: String, idx: Int): String = methodName + "$default$" + idx

    final protected val possibleConstructorNames: List[String] = List(
      "<init>", // Ctor of non-case class (no `apply`) has `<init>$default$idx` default (on Scala 2, unencoded).
      "apply", // Ctor of case class on Scala 2 has `apply$default$idx` default (= `apply` method).
      "$lessinit$greater$apply" // Ctor of case class on Scala 3 has `$lessinit$greater$default$idx`(no `apply` but encoded).
    )
  }

  trait UntypedMethodMethods { this: UntypedMethod =>

    def asTyped[Instance: Type]: Method.Of[Instance] = UntypedMethod.toTyped(this)

    def invocation: Invocation

    def hasTypeParameters: Boolean
    def parameters: UntypedParameters

    def unsafeApply(instanceTpe: UntypedType)(instance: Option[UntypedExpr], arguments: UntypedArguments): UntypedExpr
    final def unsafeApplyNoInstance(instanceTpe: UntypedType)(arguments: UntypedArguments): UntypedExpr =
      unsafeApply(instanceTpe)(None, arguments)
    final def unsafeApplyInstance(
        instanceTpe: UntypedType
    )(instance: UntypedExpr, arguments: UntypedArguments): UntypedExpr =
      unsafeApply(instanceTpe)(Some(instance), arguments)

    def name: String
    def position: Option[Position]

    def annotations: List[UntypedExpr]

    def isConstructor: Boolean

    def isConstructorArgument: Boolean
    def isCaseField: Boolean

    def isVal: Boolean
    def isVar: Boolean
    def isLazy: Boolean
    def isDef: Boolean
    def isDeclared: Boolean
    def isSynthetic: Boolean
    final def isInherited: Boolean = !isDeclared && !isSynthetic
    def isImplicit: Boolean

    def isAvailable(scope: Accessible): Boolean
  }

  implicit final lazy val UntypedMethodOrdering: Ordering[UntypedMethod] =
    // Stabilize order in case of https://github.com/scala/scala3/issues/21672 (does not solve the warnings!)
    // TODO: order Strings using lexicographic order
    Ordering[Option[Position]].on[UntypedMethod](_.position).orElseBy(_.name)
}
