package hearth
package typed

import scala.collection.immutable.ListMap

trait Methods { this: MacroCommons =>

  final class Parameter(val asUntyped: UntypedParameter, private val instanceTpe: UntypedType) {

    def name: String = UntypedParameter.name(asUntyped)

    // def paramType: ?? = instanceTpe.parameter(asUntyped).as_??
    def defaultValue: Option[Expr_??] = UntypedExpr.defaultValue(instanceTpe)(asUntyped).map(_.as_??)
    def annotations: List[Expr_??] = UntypedParameter.annotations(asUntyped).map(_.as_??)

    def isByName: Boolean = asUntyped.isByName
    def isImplicit: Boolean = asUntyped.isImplicit
  }

  type Parameters = List[ListMap[String, Parameter]]
  type Arguments = Map[String, Expr_??]

  sealed trait Method[Instance, Returned] {
    val untyped: UntypedMethod
    val untypedInstanceType: UntypedType

    lazy val name: String = UntypedMethod.name(untyped)

    final def annotations: List[Expr_??] = UntypedMethod.annotations(untyped).map(_.as_??)

    final def isVal: Boolean = UntypedMethod.isVal(untyped)
    final def isVar: Boolean = UntypedMethod.isVar(untyped)
    final def isLazy: Boolean = UntypedMethod.isLazy(untyped)
    final def isDef: Boolean = UntypedMethod.isDef(untyped)
    final def isInherited: Boolean = UntypedMethod.isInherited(untyped)
    final def isImplicit: Boolean = UntypedMethod.isImplicit(untyped)

    final def isAvailable(scope: Accessible): Boolean = UntypedMethod.isAvailable(untyped, scope)

    final def isNAry(n: Int): Boolean = parameters.flatten.size == n
    lazy val isNullary: Boolean = isNAry(0)
    lazy val isUnary: Boolean = isNAry(1)

    final def isAccessor: Boolean = isVal || isVar || isLazy || (isDef && isNullary)
    def isJavaGetter: Boolean
    def isJavaSetter: Boolean

    def parameters: Parameters
  }
  object Method {

    @scala.annotation.nowarn
    def primaryConstructorOf[A: Type]: Option[Method.NoInstance[A]] =
      UntypedType.fromTyped[A].primaryConstructor.map(UntypedMethod.toTyped[A](_)).flatMap { tpd =>
        import tpd.Underlying as A0
        if (A0 <:< Type.of[A]) tpd.value match {
          case m: Method.NoInstance[A] => Some(m)
          case _                       => None
        }
        else None
      }
    @scala.annotation.nowarn
    def constructorsOf[A: Type]: List[Method.NoInstance[A]] =
      UntypedType.fromTyped[A].constructors.map(UntypedMethod.toTyped[A](_)).flatMap { tpd =>
        import tpd.Underlying as A0
        if (A0 <:< Type.of[A]) tpd.value match {
          case m: Method.NoInstance[A] => List(m)
          case _                       => Nil
        }
        else Nil
      }
    def methodsOf[A: Type]: List[Existential[Method[A, *]]] =
      UntypedType.fromTyped[A].methods.map(UntypedMethod.toTyped[A](_))

    /** Static method/stable object method */
    final case class NoInstance[Returned](
        val untyped: UntypedMethod,
        val untypedInstanceType: UntypedType
    )(implicit val returnType: Type[Returned])
        extends Method[Nothing, Returned] {

      // TODO: call in companion object symbol or sth
      val parameters: Parameters = UntypedParameters.toTyped[Returned](untyped.parametersAt(untypedInstanceType))
      // val applyUnsafe: Arguments => Expr[Returned] = arguments =>
      //   untypedInstanceType.unsafeApplyAt(untyped)(UntypedArguments.fromTyped(arguments)).asTyped[Returned]

      def isJavaGetter: Boolean = false
      def isJavaSetter: Boolean = false
    }

    /** Instance method */
    final case class OfInstance[Instance, Returned](
        val untyped: UntypedMethod,
        val untypedInstanceType: UntypedType
    )(implicit val returnType: Type[Returned])
        extends Method[Instance, Returned] {

      implicit val instanceType: Type[Instance] = UntypedType.toTyped[Instance](untypedInstanceType)

      val parameters: Parameters = UntypedParameters.toTyped[Instance](untyped.parametersAt(untypedInstanceType))
      // val applyUnsafe: (Expr[Instance], Arguments) => Expr[Returned] = (instance, arguments) =>
      //   untypedInstanceType.unsafeApplyAt(untyped)(UntypedArguments.fromTyped(arguments)).asTyped[Returned]

      lazy val isJavaGetter: Boolean = isAccessor && (
        (name.startsWith("get") && name.length > 3 && !(instanceType <:< Type.of[Unit])) ||
          (name.startsWith("is") && name.length > 2 && (instanceType <:< Type.of[Boolean]))
      )
      lazy val isJavaSetter: Boolean = isUnary &&
        (name.startsWith("set") && name.length > 3 && instanceType <:< Type.of[Unit])
    }

    /** Everything that we cannot handle with the above (polymorphic methods) */
    final case class Unsupported[Instance, Returned](
        val untyped: UntypedMethod,
        val untypedInstanceType: UntypedType,
        val reasonForUnsupported: String
    ) extends Method[Instance, Returned] {

      val parameters: Parameters = List.empty

      def isJavaGetter: Boolean = false
      def isJavaSetter: Boolean = false
    }
  }

  sealed trait Accessible extends Product with Serializable

  /** Class/Method is public */
  case object Everywhere extends Accessible

  /** Class/Method might package private/protected modifier but we are in the same package */
  case object AtCallSite extends Accessible
}
