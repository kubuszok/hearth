package hearth
package typed

import scala.collection.immutable.ListMap

trait Methods { this: MacroCommons =>

  type Parameter

  implicit final class ParameterMethods(private val param: Parameter) {

    def paramName: String = param.toString // TODO
    def paramType: ?? = ???
    def defaultValue: Option[Expr_??] = ???
    def annotations: List[Expr_??] = ???

    def asUntyped: UntypedParameter = ???
  }

  type Parameters = List[ListMap[String, Parameter]]
  type Arguments = Map[String, Expr_??]

  final class Method[Out](val untyped: UntypedMethod, val untypedInstanceType: UntypedType)(implicit
      val returnType: Type[Out]
  ) {

    val parameters: Parameters = UntypedParameters.toTyped(untypedInstanceType.parametersAt(untyped))
    val applyUnsafe: Arguments => Expr[Out] = arguments =>
      untypedInstanceType.unsafeApplyAt(untyped)(UntypedArguments.fromTyped(arguments)).asTyped[Out]
  }
  object Method {
    def primaryConstructorOf[A: Type]: Option[Method[A]] = {
      val untyped = UntypedType.fromTyped[A]
      untyped.primaryConstructor.map { method =>
        new Method[A](method, untyped)
      }
    }
    def constructorsOf[A: Type]: List[Method[A]] = {
      val untyped = UntypedType.fromTyped[A]
      untyped.constructors.map { method =>
        new Method[A](method, untyped)
      }
    }

    // TODO: factories

    def unapply[Out](method: Method[Out]): Some[(Parameters, Arguments => Expr[Out])] =
      Some((method.parameters, method.applyUnsafe))
  }
}
