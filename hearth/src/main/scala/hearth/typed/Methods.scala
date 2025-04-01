package hearth
package typed

import scala.collection.immutable.ListMap

trait Methods { this: MacroCommons =>

  type Parameter

  val Parameter: ParameterModule
  trait ParameterModule { this: Parameter.type =>

    def name(param: Parameter): String

    def paramType(param: Parameter): ??
    def defaultValue(param: Parameter): Option[Expr_??]
    def annotations(param: Parameter): List[Expr_??]
  }

  implicit final class ParameterMethods(private val param: Parameter) {

    def paramName: String = Parameter.name(param)

    def paramType: ?? = Parameter.paramType(param)
    def defaultValue: Option[Expr_??] = Parameter.defaultValue(param)
    def annotations: List[Expr_??] = Parameter.annotations(param)

    def asUntyped: UntypedParameter = UntypedParameter.fromTyped(param)
  }

  type Parameters = List[ListMap[String, Parameter]]
  type Arguments = Map[String, Expr_??]

  final class Method[Out](val untyped: UntypedMethod, val untypedInstanceType: UntypedType)(implicit
      val returnType: Type[Out]
  ) {

    def name: String = UntypedMethod.name(untyped)

    def annotations: List[Expr_??] = UntypedMethod.annotations(untyped).map(_.as_??)

    def isVal: Boolean = UntypedMethod.isVal(untyped)
    def isVar: Boolean = UntypedMethod.isVar(untyped)
    def isLazy: Boolean = UntypedMethod.isLazy(untyped)
    def isDef: Boolean = UntypedMethod.isDef(untyped)
    def isInherited: Boolean = UntypedMethod.isInherited(untyped)
    def isImplicit: Boolean = UntypedMethod.isImplicit(untyped)

    def isPublic: Boolean = UntypedMethod.isPublic(untyped)
    def isAccessibleHere: Boolean = UntypedMethod.isAccessibleHere(untyped)

    def isAccessor: Boolean = isVal || isVar || isLazy || (isDef && parameters.forall(_.isEmpty))
    // TODO: defer until generid way of handling types is available
    // TODO: implement using existing methods: (get* + non-Unit || is* + Boolean) + List(Nil) as parameter list
    def isJavaGetter: Boolean = ???
    // TODO: implement using existing methods: set* + :Unit + List(List(input)) as paremeter list
    def isJavaSetter: Boolean = ???

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
