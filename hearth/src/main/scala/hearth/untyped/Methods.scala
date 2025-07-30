package hearth
package untyped

import hearth.compat.*
import scala.collection.immutable.ListMap
import scala.collection.compat.*

trait Methods { this: MacroCommons =>

  /** Platform-specific untyped parameter representation (`c.universe.TermSymbol` in 2, `quotes.reflect.Symbol` in 3)
    * together with an [[UntypedMethod]] to which it belongs.
    */
  type UntypedParameter

  val UntypedParameter: UntypedParameterModule
  trait UntypedParameterModule { this: UntypedParameter.type =>

    def fromTyped(param: Parameter): UntypedParameter = param.asUntyped

    def name(param: UntypedParameter): String

    def annotations(param: UntypedParameter): List[UntypedExpr]

    def isByName(param: UntypedParameter): Boolean
    def isImplicit(param: UntypedParameter): Boolean
    def hasDefault(param: UntypedParameter): Boolean
  }

  implicit final class UntypedParameterMethods(private val param: UntypedParameter) {

    def paramName: String = UntypedParameter.name(param)
    def annotations: List[UntypedExpr] = UntypedParameter.annotations(param)

    def isByName: Boolean = UntypedParameter.isByName(param)
    def isImplicit: Boolean = UntypedParameter.isImplicit(param)
    def hasDefault: Boolean = UntypedParameter.hasDefault(param)

    def default(instanceTpe: UntypedType): Option[UntypedExpr] = UntypedExpr.defaultValue(instanceTpe)(param)
  }

  /** Ordered map of [[UntypedParameter]]s by their name. */
  type UntypedParameters = List[ListMap[String, UntypedParameter]]

  val UntypedParameters: UntypedParametersModule
  trait UntypedParametersModule { this: UntypedParameters.type =>

    final def fromTyped(typed: Parameters): UntypedParameters = typed.map { inner =>
      ListMap.from(inner.view.mapValues(_.asUntyped))
    }

    def toTyped[Instance: Type](untyped: UntypedParameters): Parameters
  }

  type UntypedArguments = Map[String, UntypedExpr]
  object UntypedArguments {

    def fromTyped(typed: Arguments): UntypedArguments = typed.view.mapValues(_.value.asUntyped).toMap // wtf?
    def toTyped(untyped: UntypedArguments): Arguments = untyped.view.mapValues(_.as_??).toMap
  }

  implicit final class UntypedArgumentsMethods(private val arguments: UntypedArguments) {

    def adaptToParams(
        instanceTpe: UntypedType,
        method: UntypedMethod,
        paramss: UntypedParameters
    ): List[List[UntypedExpr]] =
      paramss.map { params =>
        params.view.map { case (paramName, untyped) =>
          arguments.get(paramName).orElse(untyped.default(instanceTpe)).getOrElse {
            assertionFailed(
              s"Expected that ${Type.prettyPrint(using instanceTpe.asTyped[Any])}'s ${method.methodName} parameter `$paramName` would be provided or have default value"
            )
          }
        }.toList
      }
  }

  /** Platform-specific method representation (`c.universe.MethodSymbol` in 2, `quotes.reflect.Symbol` in 3) */
  type UntypedMethod

  val UntypedMethod: UntypedMethodModule
  trait UntypedMethodModule { this: UntypedMethod.type =>

    final def fromTyped[Instance, Returned](method: Method[Instance, Returned]): UntypedMethod = method.untyped
    def toTyped[Instance: Type](untyped: UntypedMethod): Existential[Method[Instance, *]]

    def unsafeApply(
        instanceTpe: UntypedType,
        method: UntypedMethod
    )(instance: Option[UntypedExpr], arguments: UntypedArguments, isConstructor: Boolean): UntypedExpr

    def primaryConstructor(instanceTpe: UntypedType): Option[UntypedMethod]
    def constructors(instanceTpe: UntypedType): List[UntypedMethod]
    def methods(instanceTpe: UntypedType): List[UntypedMethod]

    def parametersAt(method: UntypedMethod)(instanceTpe: UntypedType): UntypedParameters

    def name(method: UntypedMethod): String
    def position(method: UntypedMethod): Position
    def annotations(method: UntypedMethod): List[UntypedExpr]

    // TODO: isConstructorArgument?
    def isVal(method: UntypedMethod): Boolean
    def isVar(method: UntypedMethod): Boolean
    def isLazy(method: UntypedMethod): Boolean
    def isDef(method: UntypedMethod): Boolean
    def isInherited(method: UntypedMethod): Boolean
    def isImplicit(method: UntypedMethod): Boolean

    def isAvailable(method: UntypedMethod, scope: Accessible): Boolean
  }

  implicit final class UntypedMethodMethods(private val method: UntypedMethod) {

    def methodName: String = UntypedMethod.name(method)
    def methodPosition: Position = UntypedMethod.position(method)

    def parametersAt(untyped: UntypedType): UntypedParameters =
      UntypedMethod.parametersAt(method)(untyped)

    def annotations: List[UntypedExpr] = UntypedMethod.annotations(method)

    def isVal: Boolean = UntypedMethod.isVal(method)
    def isVar: Boolean = UntypedMethod.isVar(method)
    def isLazy: Boolean = UntypedMethod.isLazy(method)
    def isDef: Boolean = UntypedMethod.isDef(method)
    def isInherited: Boolean = UntypedMethod.isInherited(method)
    def isImplicitMethod: Boolean = UntypedMethod.isImplicit(method)

    def isAvailable(scope: Accessible): Boolean = UntypedMethod.isAvailable(method, scope)

    // Unsafe apply methods
    def init(instanceTpe: UntypedType)(arguments: UntypedArguments): UntypedExpr =
      UntypedMethod.unsafeApply(instanceTpe, method)(None, arguments, isConstructor = true)
    def apply(instanceTpe: UntypedType)(arguments: UntypedArguments): UntypedExpr =
      UntypedMethod.unsafeApply(instanceTpe, method)(None, arguments, isConstructor = false)
    def apply(instanceTpe: UntypedType, on: UntypedExpr)(arguments: UntypedArguments): UntypedExpr =
      UntypedMethod.unsafeApply(instanceTpe, method)(Some(on), arguments, isConstructor = false)
  }

  implicit lazy val UntypedMethodOrdering: Ordering[UntypedMethod] =
    // Stabilize order in case of https://github.com/scala/scala3/issues/21672 (does not solve the warnings!)
    // TODO: order Strings using lexicographic order
    Ordering[Position].on[UntypedMethod](_.methodPosition).orElseBy(_.methodName)
}
