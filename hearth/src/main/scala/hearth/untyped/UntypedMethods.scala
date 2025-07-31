package hearth
package untyped

import hearth.compat.*
import scala.collection.immutable.ListMap
import scala.collection.compat.*

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

    def fromTyped(param: Parameter): UntypedParameter = param.asUntyped
  }

  trait UntypedParameterMethods { this: UntypedParameter =>

    def name: String
    def index: Int
    def position: Option[Position]
    def annotations: List[UntypedExpr]

    def isByName: Boolean
    def isImplicit: Boolean
    def hasDefault: Boolean

    def default(instanceTpe: UntypedType): Option[UntypedExpr] = UntypedExpr.defaultValue(instanceTpe)(this)
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

    def fromTyped(typed: Arguments): UntypedArguments = typed.view.mapValues(_.value.asUntyped).toMap // wtf?
    def toTyped(untyped: UntypedArguments): Arguments = untyped.view.mapValues(_.as_??).toMap
  }

  implicit final class UntypedArgumentsMethods(private val arguments: UntypedArguments) {

    def adaptToParams(instanceTpe: UntypedType, method: UntypedMethod): List[List[UntypedExpr]] =
      method.parameters.map { params =>
        params.view.map { case (paramName, untyped) =>
          arguments.get(paramName).orElse(untyped.default(instanceTpe)).getOrElse {
            assertionFailed(
              s"Expected that ${Type.prettyPrint(using instanceTpe.asTyped[Any])}'s ${method.name} parameter `$paramName` would be provided or have default value"
            )
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

    final def fromTyped[Instance, Returned](method: Method[Instance, Returned]): UntypedMethod = method.untyped
    def toTyped[Instance: Type](untyped: UntypedMethod): Method.Of[Instance]

    def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod]
    def constructors(instanceTpe: UntypedType): List[UntypedMethod]
    def methods(instanceTpe: UntypedType): List[UntypedMethod]
  }

  trait UntypedMethodMethods { this: UntypedMethod =>

    def invocation: Invocation

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

    def isConstructorArgument: Boolean = false // TODO: next priority?
    def isCaseField: Boolean = false // TODO: next priority?

    def isVal: Boolean
    def isVar: Boolean
    def isLazy: Boolean
    def isDef: Boolean
    def isInherited: Boolean
    def isImplicit: Boolean

    def isAvailable(scope: Accessible): Boolean
  }

  implicit lazy val UntypedMethodOrdering: Ordering[UntypedMethod] =
    // Stabilize order in case of https://github.com/scala/scala3/issues/21672 (does not solve the warnings!)
    // TODO: order Strings using lexicographic order
    Ordering[Option[Position]].on[UntypedMethod](_.position).orElseBy(_.name)
}
