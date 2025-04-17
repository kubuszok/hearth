package hearth
package typed

import hearth.fp.Id
import scala.language.implicitConversions

trait Types { this: MacroCommons =>

  /** Platform-specific type representation (`c.WeakTypeTag[A]` in 2, `scala.quoted.Type[A]` in 3) */
  type Type[A]

  val Type: TypeModule
  trait TypeModule { this: Type.type =>

    /** Summons `Type` instance */
    final def apply[A](implicit A: Type[A]): Type[A] = A

    final def apply[A](value: A)(implicit codec: TypeCodec[A]): Type[A] = codec.toType(value)
    final def unapply[A](tpe: Type[A])(implicit codec: TypeCodec[A]): Option[A] = codec.fromType(tpe).map(_.value)

    def shortName[A: Type]: String
    final def fcqn[A: Type]: String = plainPrint[A].takeWhile(_ != '[')
    final def plainPrint[A: Type]: String = removeAnsiColors(prettyPrint[A])
    def prettyPrint[A: Type]: String

    final def directChildren[A: Type]: Option[List[??<:[A]]] =
      UntypedType.fromTyped[A].directChildren.map(_.map(_.asTyped[A].as_??<:[A]))
    final def exhaustiveChildren[A: Type]: Option[List[??<:[A]]] =
      directChildren[A].flatMap(_.foldRight[Option[List[??<:[A]]]](Some(List.empty)) {
        case (_, None)                                 => None
        case (child, _) if child.Underlying.isAbstract => None
        case (child, Some(list)) if child.Underlying.isSealed =>
          exhaustiveChildren[A](using child.asUntyped.asTyped[A]).map(_ ++ list)
        case (child, Some(list)) => Some(child +: list)
      })

    def annotations[A: Type]: List[Expr_??]

    /** Types which might be compiled to both JVM primitives and javal.lang.Object: Boolean, Byte, Short, Char, Int,
      * Long, Float, Double.
      */
    val primitiveTypes: List[??]
    final def isPrimitive[A: Type]: Boolean = primitiveTypes.exists(tpe => Type[A] <:< tpe.Underlying)

    // TODO: rename to buildInJvmTypes
    // TODO: add: java.lang.Class, java.lang.Object, java.lang.Enum, java.lang.EnumValue?
    // TODO: add: java.lang.reflect.*, java.lang.invoke.*
    /** Types which are either primitives or specially treated by JVM: Unit, String. */
    val buildInTypes: List[??]
    final def isBuildIn[A: Type]: Boolean = buildInTypes.exists(tpe => Type[A] <:< tpe.Underlying)

    def isAbstract[A: Type]: Boolean
    def isFinal[A: Type]: Boolean

    // TODO: rename class to something more unambiguous
    def isClass[A: Type]: Boolean
    final def notBuildInClass[A: Type]: Boolean = isClass[A] && !isBuildIn[A]
    final def isPlainOldJavaObject[A: Type]: Boolean =
      notBuildInClass[A] && !(isAbstract[A] || isSealed[A] || isJavaEnum[A] || isJavaEnumValue[A])
    final def isJavaBean[A: Type]: Boolean = false // TODO

    def isSealed[A: Type]: Boolean
    def isJavaEnum[A: Type]: Boolean
    def isJavaEnumValue[A: Type]: Boolean

    def isCase[A: Type]: Boolean
    def isObject[A: Type]: Boolean
    def isVal[A: Type]: Boolean

    final def isCaseClass[A: Type]: Boolean = isClass[A] && isCase[A]
    final def isCaseObject[A: Type]: Boolean = isObject[A] && isCase[A]
    final def isCaseVal[A: Type]: Boolean = isVal[A] && isCase[A]

    def isPublic[A: Type]: Boolean
    def isAvailableHere[A: Type]: Boolean

    def isSubtypeOf[A: Type, B: Type]: Boolean
    def isSameAs[A: Type, B: Type]: Boolean

    // Literal types

    val BooleanCodec: TypeCodec[Boolean]
    val IntCodec: TypeCodec[Int]
    val LongCodec: TypeCodec[Long]
    val FloatCodec: TypeCodec[Float]
    val DoubleCodec: TypeCodec[Double]
    val CharCodec: TypeCodec[Char]
    val StringCodec: TypeCodec[String]

    private object ModuleCodecImpl extends TypeCodec[Any] {
      def toType[A <: Any](value: A): Type[A] =
        ??? // TODO

      def fromType[A](tpe: Type[A]): Option[Existential.UpperBounded[Any, Id]] = {
        // assuming this is "foo.bar.baz"...
        val name = plainPrint(using tpe)

        scala.collection.Iterator
          .iterate(name + '$')(_.reverse.replaceFirst("[.]", "\\$").reverse)
          .take(name.count(_ == '.') + 1) // ...then this is: "foo.bar.baz$", "foo.bar$baz$", "foo$bar$baz$"...
          .toArray
          .reverse // ...and this is: "foo.bar.baz$", "foo.bar$baz$", "foo$bar$baz$"
          .collectFirst { case ModuleSingleton(value) =>
            Existential.UpperBounded[Any, Id, A](value.asInstanceOf[A])(using tpe)
          } // attempts: top-level object, object in object, etc
      }

      // based on https://github.com/MateuszKubuszok/MacroTypeclass ideas
      private object ModuleSingleton {
        def unapply(className: String): Option[Any] =
          try
            Option(Class.forName(className).getField("MODULE$").get(null))
          catch {
            case _: Throwable => None
          }
      }
    }
    def ModuleCodec[ModuleSingleton]: TypeCodec[ModuleSingleton] =
      ModuleCodecImpl.asInstanceOf[TypeCodec[ModuleSingleton]]

    // Type constructors for some common types

    trait Ctor1[F[_]] extends Ctor1.Bounded[Nothing, Any, F]
    object Ctor1 {

      /** Allows applying and extracting some type `L <:< ? <:< U` */
      trait Bounded[L, U >: L, F[_ >: L <: U]] {
        def apply[A >: L <: U: Type]: Type[F[A]]
        def unapply[A](A: Type[A]): Option[L <:??<: U]
      }
      trait UpperBounded[U, F[_ <: U]] extends Bounded[Nothing, U, F]
    }

    trait Ctor2[F[_, _]] extends Ctor2.Bounded[Nothing, Any, Nothing, Any, F]
    object Ctor2 {

      /** Allow applying and extracting some types `L1 <:< ? <:< U1, L2 <:< ? <:< U2` */
      trait Bounded[L1, U1 >: L1, L2, U2 >: L2, F[_ >: L1 <: U1, _ >: L2 <: U2]] {
        def apply[A >: L1 <: U1: Type, B >: L2 <: U2: Type]: Type[F[A, B]]
        def unapply[A](A: Type[A]): Option[(L1 <:??<: U1, L2 <:??<: U2)]
      }
      trait UpperBounded[U1, U2, F[_ <: U1, _ <: U2]] extends Bounded[Nothing, U1, Nothing, U2, F]
    }

    trait Ctor3[F[_, _, _]] extends Ctor3.Bounded[Nothing, Any, Nothing, Any, Nothing, Any, F]
    object Ctor3 {

      /** Allow applying and extracting some types `L1 <:< ? <:< U1, L2 <:< ? <:< U2, L3 <:< ? <:< U3` */
      trait Bounded[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, F[_ >: L1 <: U1, _ >: L2 <: U2, _ >: L3 <: U3]] {
        def apply[A >: L1 <: U1: Type, B >: L2 <: U2: Type, C >: L3 <: U3: Type]: Type[F[A, B, C]]
        def unapply[A](A: Type[A]): Option[(L1 <:??<: U1, L2 <:??<: U2, L3 <:??<: U3)]
      }
      trait UpperBounded[U1, U2, U3, F[_ <: U1, _ <: U2, _ <: U3]]
          extends Bounded[Nothing, U1, Nothing, U2, Nothing, U3, F]
    }

    trait Ctor4[F[_, _, _, _]] extends Ctor4.Bounded[Nothing, Any, Nothing, Any, Nothing, Any, Nothing, Any, F]
    object Ctor4 {

      /** Allow applying and extracting some types `L1 <:< ? <:< U1, L2 <:< ? <:< U2, L3 <:< ? <:< U3, L4 <:< ? <:< U4`
        */
      trait Bounded[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, F[
          _ >: L1 <: U1,
          _ >: L2 <: U2,
          _ >: L3 <: U3,
          _ >: L4 <: U4
      ]] {
        def apply[A >: L1 <: U1: Type, B >: L2 <: U2: Type, C >: L3 <: U3: Type, D >: L4 <: U4: Type]
            : Type[F[A, B, C, D]]
        def unapply[A](A: Type[A]): Option[(L1 <:??<: U1, L2 <:??<: U2, L3 <:??<: U3, L4 <:??<: U4)]
      }
      trait UpperBounded[U1, U2, U3, U4, F[_ <: U1, _ <: U2, _ <: U3, _ <: U4]]
          extends Bounded[Nothing, U1, Nothing, U2, Nothing, U3, Nothing, U4, F]
    }

    // ToDo, till 22 - maybe use some script to generate them?
  }

  implicit final class TypeMethods[A](private val tpe: Type[A]) {

    def shortName: String = Type.shortName(using tpe)
    def fcqn: String = Type.fcqn(using tpe)
    def plainPrint: String = Type.plainPrint(using tpe)
    def prettyPrint: String = Type.prettyPrint(using tpe)

    def primaryConstructor: Option[Method[A]] = Method.primaryConstructorOf(using tpe)
    def defaultConstructor: Option[Method[A]] = ???
    def constructors: List[Method[A]] = Method.constructorsOf(using tpe)

    def directChildren: Option[List[??<:[A]]] = Type.directChildren(using tpe)
    def exhaustiveChildren: Option[List[??<:[A]]] = Type.exhaustiveChildren(using tpe)

    def annotations: List[Expr_??] = Type.annotations(using tpe)

    def summonExpr: Option[Expr[A]] = Expr.summonImplicit(using tpe)

    def isAbstract: Boolean = Type.isAbstract(using tpe)
    def isFinal: Boolean = Type.isFinal(using tpe)
    def isSealed: Boolean = Type.isSealed(using tpe)
    def isCaseClass: Boolean = Type.isCaseClass(using tpe)
    def isObject: Boolean = Type.isObject(using tpe)
    def isJavaBean: Boolean = Type.isJavaBean(using tpe)

    def isPublic: Boolean = Type.isPublic(using tpe)
    def isAvailableHere: Boolean = Type.isAvailableHere(using tpe)

    def <:<[B](tpe2: Type[B]): Boolean = Type.isSubtypeOf(using tpe, tpe2)
    def =:=[B](tpe2: Type[B]): Boolean = Type.isSameAs(using tpe, tpe2)

    def asUntyped: UntypedType = UntypedType.fromTyped(using tpe)

    def as_?? : ?? = Existential[Type, A](tpe)(using tpe)
    def as_??>:[L <: A]: ??>:[L] = Existential.LowerBounded[L, Type, A](tpe)(using tpe)
    def as_??<:[U >: A]: ??<:[U] = Existential.UpperBounded[U, Type, A](tpe)(using tpe)
    def as_<:??<:[L <: A, U >: A]: L <:??<: U = Existential.Bounded[L, U, Type, A](tpe)(using tpe)
  }

  // Aliases to make the (very common) existential types shorter

  final type ?? = Existential[Type]
  final type ??>:[L] = Existential.LowerBounded[L, Type]
  final type ??<:[U] = Existential.UpperBounded[U, Type]
  final type <:??<:[L, U >: L] = Existential.Bounded[L, U, Type]

  implicit def ExistentialTypeMethods(tpe: ??): BoundedExistentialTypeMethods[Nothing, Any] =
    new BoundedExistentialTypeMethods[Nothing, Any](tpe)
  implicit def LowerBoundedExistentialTypeMethods[L](tpe: ??>:[L]): BoundedExistentialTypeMethods[L, Any] =
    new BoundedExistentialTypeMethods[L, Any](tpe)
  implicit def UpperBoundedExistentialTypeMethods[U](tpe: ??<:[U]): BoundedExistentialTypeMethods[Nothing, U] =
    new BoundedExistentialTypeMethods[Nothing, U](tpe)
  implicit final class BoundedExistentialTypeMethods[L, U >: L](private val tpe: L <:??<: U) {

    def shortName: String = Type.shortName(using tpe.Underlying)
    def fcqn: String = Type.fcqn(using tpe.Underlying)
    def plainPrint: String = Type.plainPrint(using tpe.Underlying)
    def prettyPrint: String = Type.prettyPrint(using tpe.Underlying)

    def asUntyped: UntypedType = UntypedType.fromTyped(using tpe.Underlying)
  }

  /** Generalizes over the idea of conversion between singleton/literal type values and their type representation. */
  trait TypeCodec[U] {
    def toType[A <: U](value: A): Type[A]
    def fromType[A](tpe: Type[A]): Option[Existential.UpperBounded[U, Id]]
  }
  object TypeCodec {
    def apply[A](implicit codec: TypeCodec[A]): TypeCodec[A] = codec

    // TODO: more instances - for starters cover all types in covered by ToExpr i FromExpr in Quotes

    implicit val BooleanCodec: TypeCodec[Boolean] = Type.BooleanCodec
    implicit val IntCodec: TypeCodec[Int] = Type.IntCodec
    implicit val LongCodec: TypeCodec[Long] = Type.LongCodec
    implicit val FloatCodec: TypeCodec[Float] = Type.FloatCodec
    implicit val DoubleCodec: TypeCodec[Double] = Type.DoubleCodec
    implicit val CharCodec: TypeCodec[Char] = Type.CharCodec
    implicit val StringCodec: TypeCodec[String] = Type.StringCodec

    implicit def ModuleCodec[ModuleSingleton <: Product & Serializable & Singleton]: TypeCodec[ModuleSingleton] =
      Type.ModuleCodec[ModuleSingleton]
  }
}
