package hearth
package typed

import scala.collection.immutable.ListMap

trait Methods { this: MacroCommons =>

  final class Parameter(
      val asUntyped: UntypedParameter,
      val untypedInstanceType: UntypedType,
      val tpe: ??
  ) {

    def name: String = asUntyped.name

    def defaultValue: Option[Expr_??] = asUntyped.default(untypedInstanceType).map(_.as_??)
    def annotations: List[Expr_??] = asUntyped.annotations.map(_.as_??)

    def isByName: Boolean = asUntyped.isByName
    def isImplicit: Boolean = asUntyped.isImplicit
  }

  type Parameters = List[ListMap[String, Parameter]]
  type Arguments = Map[String, Expr_??]

  sealed trait Method[+Instance, Returned] {
    val untyped: UntypedMethod
    val untypedInstanceType: UntypedType

    lazy val name: String = untyped.name

    final def annotations: List[Expr_??] = untyped.annotations.map(_.as_??)

    final def isVal: Boolean = untyped.isVal
    final def isVar: Boolean = untyped.isVar
    final def isLazy: Boolean = untyped.isLazy
    final def isDef: Boolean = untyped.isDef
    final def isInherited: Boolean = untyped.isInherited
    final def isImplicit: Boolean = untyped.isImplicit

    final def isAvailable(scope: Accessible): Boolean = untyped.isAvailable(scope)

    final lazy val arity: Int = parameters.flatten.size
    final def isNAry(n: Int): Boolean = arity == n
    lazy val isNullary: Boolean = isNAry(0)
    lazy val isUnary: Boolean = isNAry(1)
    lazy val isBinary: Boolean = isNAry(2)

    final def isConstructorArgument: Boolean = untyped.isConstructorArgument
    final def isCaseField: Boolean = untyped.isCaseField

    final def isScalaGetter: Boolean = isVal || isVar || isLazy
    final def isScalaSetter: Boolean = isVar
    final def isScalaAccessor: Boolean = isScalaGetter || isScalaSetter

    def isJavaGetter: Boolean
    def isJavaSetter: Boolean
    final def isJavaAccessor: Boolean = isJavaGetter || isJavaSetter

    final def isAccessor: Boolean = isScalaAccessor || isJavaAccessor

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

    /** Constructor/static method/stable object method.
      *
      * You are able to use the returned type with:
      *
      * {{{
      * val method: Method.NoInstance[...] = ...
      * import method.Returned
      * // both implicit and type becomes available
      * }}}
      *
      * @since 0.1.0
      *
      * @tparam Returned0
      *   the return type of the method
      * @param untyped
      *   the untyped method
      * @param untypedInstanceType
      *   the untyped instance type
      */
    final case class NoInstance[Returned0](
        untyped: UntypedMethod,
        untypedInstanceType: UntypedType
    )(implicit val Returned: Type[Returned0])
        extends Method[Nothing, Returned0] {

      final type Returned = Returned0

      val parameters: Parameters = UntypedParameters.toTyped[Returned](untyped.parameters)

      def apply(arguments: Arguments): Either[String, Expr[Returned]] = {
        val issues = parameters.flatten.flatMap { case (name, parameter) =>
          arguments.get(name) match {
            case None        => Some(s"Missing $name parameter")
            case Some(value) =>
              if (value.Underlying <:< parameter.tpe.Underlying) None
              else Some(s"Invalid $name parameter type: !(${value.Underlying} <:< ${parameter.tpe.Underlying})")
          }
        }
        if (issues.isEmpty)
          Right(
            untyped.unsafeApplyNoInstance(untypedInstanceType)(UntypedArguments.fromTyped(arguments)).asTyped[Returned]
          )
        else Left(issues.mkString("\n"))
      }

      def isJavaGetter: Boolean = false
      def isJavaSetter: Boolean = false

      // We need to compare types with =:=, on Scala 3, == does not work
      override def equals(that: Any): Boolean = that match {
        case that: NoInstance[?] =>
          untyped == that.untyped && untypedInstanceType =:= that.untypedInstanceType && Returned =:= that.Returned
        case _ => false
      }
      override def hashCode: Int = untyped.hashCode
    }

    /** Instance method.
      *
      * You are able to use the instance type with:
      *
      * {{{
      * val method: Method.OfInstance[..., ...] = ...
      * import method.{ Instance, Returned }
      * // both implicit and type becomes available
      * }}}
      *
      * @since 0.1.0
      *
      * @tparam Instance0
      *   the type of the instance (with all type parameters applied!)
      * @tparam Returned0
      *   the return type of the method
      * @param untyped
      *   the untyped method
      * @param untypedInstanceType
      *   the untyped instance type
      */
    final case class OfInstance[Instance0, Returned0](
        untyped: UntypedMethod,
        untypedInstanceType: UntypedType
    )(implicit val Returned: Type[Returned0])
        extends Method[Instance0, Returned0] {

      final type Returned = Returned0

      final type Instance = Instance0
      implicit val Instance: Type[Instance] = UntypedType.toTyped[Instance](untypedInstanceType)

      val parameters: Parameters = UntypedParameters.toTyped[Instance](untyped.parameters)

      def apply(instance: Expr[Instance], arguments: Arguments): Either[String, Expr[Returned]] = {
        val issues = parameters.flatten.flatMap { case (name, parameter) =>
          arguments.get(name) match {
            case None        => Some(s"Missing $name parameter")
            case Some(value) =>
              if (value.Underlying <:< parameter.tpe.Underlying) None
              else Some(s"Invalid $name parameter type: !(${value.Underlying} <:< ${parameter.tpe.Underlying})")
          }
        }
        if (issues.isEmpty)
          Right(
            untyped
              .unsafeApplyInstance(untypedInstanceType)(instance.asUntyped, UntypedArguments.fromTyped(arguments))
              .asTyped[Returned]
          )
        else Left(issues.mkString("\n"))
      }

      lazy val isJavaGetter: Boolean = isAccessor && (
        (name.startsWith("get") && name.length > 3 && !(Returned <:< Type.of[Unit])) ||
          (name.startsWith("is") && name.length > 2 && (Returned <:< Type.of[Boolean]))
      )
      lazy val isJavaSetter: Boolean = isUnary &&
        (name.startsWith("set") && name.length > 3 && Returned <:< Type.of[Unit])

      // We need to compare types with =:=, on Scala 3, == does not work
      override def equals(that: Any): Boolean = that match {
        case that: OfInstance[?, ?] =>
          untyped == that.untyped && untypedInstanceType =:= that.untypedInstanceType && Returned =:= that.Returned
        case _ => false
      }
      override def hashCode: Int = untyped.hashCode
    }

    /** Everything that we cannot handle with the above (e.g. polymorphic methods).
      *
      * @since 0.1.0
      *
      * @tparam Instance
      *   the type of the instance (with all type parameters applied!) - phantom type
      * @tparam Returned
      *   the return type of the method - phantom type
      * @param untyped
      *   the untyped method
      * @param untypedInstanceType
      *   the untyped instance type
      * @param reasonForUnsupported
      *   the reason why this method is unsupported
      */
    final case class Unsupported[Instance, Returned](
        untyped: UntypedMethod,
        untypedInstanceType: UntypedType
    )(val reasonForUnsupported: String)
        extends Method[Instance, Returned] {

      val parameters: Parameters = List.empty

      def isJavaGetter: Boolean = false
      def isJavaSetter: Boolean = false

      // We need to compare types with =:=, on Scala 3, == does not work
      override def equals(that: Any): Boolean = that match {
        case that: Unsupported[?, ?] => untyped == that.untyped && untypedInstanceType =:= that.untypedInstanceType
        case _                       => false
      }
      override def hashCode: Int = untyped.hashCode
    }
  }

  /** Where do we need to access the method? (The use case, not the visibility). */
  sealed trait Accessible extends Product with Serializable

  /** Class/Method is public */
  case object Everywhere extends Accessible

  /** Class/Method might package private/protected modifier but we are in the same package */
  case object AtCallSite extends Accessible
}
