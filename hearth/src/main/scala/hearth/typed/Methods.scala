package hearth
package typed

import scala.collection.immutable.ListMap

trait Methods { this: MacroCommons =>

  /** Represents a method parameter with resolved type.
    *
    * Allows easier access to:
    *   - name
    *   - default value
    *   - annotations
    *   - resolved type
    *   - checking if parameter is by-name
    *   - checking if parameter is implicit
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
    lazy val defaultValue: Option[Existential[Method.Of]] = asUntyped.default(untypedInstanceType).map { untyped =>
      untypedInstanceType.as_??.mapK[Method.Of] { tpe => _ =>
        untyped.asTyped(using tpe)
      }
    }

    lazy val annotations: List[Expr_??] = asUntyped.annotations.map(_.as_??)

    lazy val isByName: Boolean = asUntyped.isByName
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
    * Whether it is true is verified by [[Method.NoInstance.apply]] and [[Method.OfInstance.apply]]. It is not verified
    * when using the [[UntypedMethod]] directly.
    *
    * {{{
    * method match {
    *   case m: Method.NoInstance[?] =>
    *     import m.Returned
    *     method(arguments) // Either[String, Expr[Returned]]
    *   case m: Method.OfInstance[?, ?] =>
    *     import m.{ Instance, Returned }
    *     method(instance /* Expr[Instance] */, arguments) // Either[String, Expr[Returned]]
    *   case Method.Unsupported(_, _) =>
    *     Left(method.reasonForUnsupported)
    * }
    * }}}
    *
    * @since 0.1.0
    */
  type Arguments = Map[String, Expr_??]

  /** Represents a method, that can be called.
    *
    * Since not all methods are supported, and those that are can come in multiple flavors, we need to represent method
    * as an enumeration, where you have to pattern-match on it, to know how to call it.
    *
    * @since 0.1.0
    */
  sealed trait Method[+Instance, Returned] {
    val asUntyped: UntypedMethod
    val untypedInstanceType: UntypedType

    def parameters: Parameters

    final lazy val name: String = asUntyped.name
    final lazy val position: Option[Position] = asUntyped.position

    final lazy val annotations: List[Expr_??] = asUntyped.annotations.map(_.as_??)

    final lazy val isConstructor: Boolean = asUntyped.isConstructor

    final lazy val isVal: Boolean = asUntyped.isVal
    final lazy val isVar: Boolean = asUntyped.isVar
    final lazy val isLazy: Boolean = asUntyped.isLazy
    final lazy val isDef: Boolean = asUntyped.isDef
    final lazy val isImplicit: Boolean = asUntyped.isImplicit
    final lazy val isDeclared: Boolean = asUntyped.isDeclared
    final lazy val isSynthetic: Boolean = asUntyped.isSynthetic
    final lazy val isInherited: Boolean = asUntyped.isInherited

    final def isAvailable(scope: Accessible): Boolean = asUntyped.isAvailable(scope)

    final lazy val arity: Int = parameters.flatten.size
    final def isNAry(n: Int): Boolean = arity == n
    final lazy val isNullary: Boolean = isNAry(0)
    final lazy val isUnary: Boolean = isNAry(1)
    final lazy val isBinary: Boolean = isNAry(2)

    final lazy val isConstructorArgument: Boolean = asUntyped.isConstructorArgument
    final lazy val isCaseField: Boolean = asUntyped.isCaseField

    final lazy val isScalaGetter: Boolean = (isVal || isVar || isLazy) && !isScalaSetter
    final lazy val isScalaSetter: Boolean = (isUnary && name.endsWith("_="))
    final lazy val isScalaAccessor: Boolean = isScalaGetter || isScalaSetter

    final lazy val scalaAccessorName: Option[String] =
      if (isScalaGetter) Some(name) else if (isScalaSetter) Some(name.dropRight(2)) else None

    final lazy val isJavaGetter: Boolean = this match {
      case m: Method.OfInstance[?, ?] =>
        import m.Returned
        isNullary &&
        (name.startsWith("get") && name.length > 3 && !(Returned <:< Type.of[Unit])) ||
        (name.startsWith("is") && name.length > 2 && (Returned <:< Type.of[Boolean]))
      case _ => false
    }
    final lazy val isJavaSetter: Boolean = this match {
      case m: Method.OfInstance[?, ?] =>
        import m.Returned
        isUnary && (name.startsWith("set") && name.length > 3 && Returned <:< Type.of[Unit])
      case _ => false
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
  }
  object Method {

    /** Nice type alias [[Method]] taking some type `A` that can be of any type.
      *
      * @since 0.1.0
      */
    type Of[A] = Existential[Method[A, *]]

    @scala.annotation.nowarn
    def primaryConstructorOf[A: Type]: Option[Method.NoInstance[A]] =
      UntypedType.fromTyped[A].primaryConstructor.map(_.asTyped[A]).flatMap { tpd =>
        import tpd.Underlying as A0
        if (A0 <:< Type[A]) tpd.value match {
          case m: Method.NoInstance[A] => Some(m)
          case _                       => None
        }
        else None
      }
    @scala.annotation.nowarn
    def constructorsOf[A: Type]: List[Method.NoInstance[A]] =
      UntypedType.fromTyped[A].constructors.map(_.asTyped[A]).flatMap { tpd =>
        import tpd.Underlying as A0
        if (A0 <:< Type[A]) tpd.value match {
          case m: Method.NoInstance[A] => List(m)
          case _                       => Nil
        }
        else Nil
      }
    def methodsOf[A: Type]: List[Method.Of[A]] =
      UntypedType.fromTyped[A].methods.map(_.asTyped[A])

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
        asUntyped: UntypedMethod,
        untypedInstanceType: UntypedType
    )(implicit val Returned: Type[Returned0])
        extends Method[Nothing, Returned0] {

      final type Returned = Returned0

      lazy val parameters: Parameters = asUntyped.parameters.asTyped[Returned]

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
            asUntyped
              .unsafeApplyNoInstance(untypedInstanceType)(UntypedArguments.fromTyped(arguments))
              .asTyped[Returned]
          )
        else Left(issues.mkString("\n"))
      }

      // We need to compare types with =:=, on Scala 3, == does not work
      override def equals(that: Any): Boolean = that match {
        case that: NoInstance[?] =>
          asUntyped == that.asUntyped && untypedInstanceType =:= that.untypedInstanceType && Returned =:= that.Returned
        case _ => false
      }
      override def hashCode: Int = asUntyped.hashCode
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
        asUntyped: UntypedMethod,
        untypedInstanceType: UntypedType
    )(implicit val Returned: Type[Returned0])
        extends Method[Instance0, Returned0] {

      final type Returned = Returned0

      final type Instance = Instance0
      implicit val Instance: Type[Instance] = untypedInstanceType.asTyped[Instance]

      lazy val parameters: Parameters = asUntyped.parameters.asTyped[Instance]

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
            asUntyped
              .unsafeApplyInstance(untypedInstanceType)(instance.asUntyped, UntypedArguments.fromTyped(arguments))
              .asTyped[Returned]
          )
        else Left(issues.mkString("\n"))
      }

      // We need to compare types with =:=, on Scala 3, == does not work
      override def equals(that: Any): Boolean = that match {
        case that: OfInstance[?, ?] =>
          asUntyped == that.asUntyped && untypedInstanceType =:= that.untypedInstanceType && Returned =:= that.Returned
        case _ => false
      }
      override def hashCode: Int = asUntyped.hashCode
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
        asUntyped: UntypedMethod,
        untypedInstanceType: UntypedType
    )(val reasonForUnsupported: String)
        extends Method[Instance, Returned] {

      val parameters: Parameters = List.empty

      // We need to compare types with =:=, on Scala 3, == does not work
      override def equals(that: Any): Boolean = that match {
        case that: Unsupported[?, ?] => asUntyped == that.asUntyped && untypedInstanceType =:= that.untypedInstanceType
        case _                       => false
      }
      override def hashCode: Int = asUntyped.hashCode
    }
  }

  /** Where do we need to access the method? (The use case, not the visibility). */
  sealed trait Accessible extends Product with Serializable

  /** Class/Method is public */
  case object Everywhere extends Accessible

  /** Class/Method might package private/protected modifier but we are in the same package */
  case object AtCallSite extends Accessible
}
