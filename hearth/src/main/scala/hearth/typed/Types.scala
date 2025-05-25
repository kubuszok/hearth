package hearth
package typed

import hearth.fp.Id
import scala.collection.compat.*
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

trait Types extends TypeConstructors with TypesCrossQuotes { this: MacroCommons =>

  /** Platform-specific type representation (`c.WeakTypeTag[A]` in 2, `scala.quoted.Type[A]` in 3) */
  type Type[A]

  val Type: TypeModule
  trait TypeModule extends Ctors with TypeCrossQuotes { this: Type.type =>

    /** Summons `Type` instance */
    final def apply[A](implicit A: Type[A]): Type[A] = A

    final def apply[A](value: A)(implicit codec: TypeCodec[A]): Type[A] = codec.toType(value)
    final def unapply[A](tpe: Type[A])(implicit codec: TypeCodec[A]): Option[A] = codec.fromType(tpe).map(_.value)

    def shortName[A: Type]: String
    final def fcqn[A: Type]: String = plainPrint[A].takeWhile(_ != '[')
    final def plainPrint[A: Type]: String = removeAnsiColors(prettyPrint[A])
    def prettyPrint[A: Type]: String

    final def directChildren[A: Type]: Option[ListMap[String, ??<:[A]]] =
      UntypedType.fromTyped[A].directChildren.map(m => ListMap.from(m.view.mapValues(_.asTyped[A].as_??<:[A])))
    final def exhaustiveChildren[A: Type]: Option[ListMap[String, ??<:[A]]] =
      UntypedType.fromTyped[A].exhaustiveChildren.map(m => ListMap.from(m.view.mapValues(_.asTyped[A].as_??<:[A])))

    def annotations[A: Type]: List[Expr_??]

    /** Types which might be compiled to both JVM primitives and javal.lang.Object: Boolean, Byte, Short, Char, Int,
      * Long, Float, Double.
      */
    final val primitiveTypes: List[??] = List(
      Type.of[Boolean].as_??,
      Type.of[Byte].as_??,
      Type.of[Short].as_??,
      Type.of[Int].as_??,
      Type.of[Long].as_??,
      Type.of[Float].as_??,
      Type.of[Double].as_??,
      Type.of[Char].as_??
    )
    final def isPrimitive[A: Type]: Boolean = UntypedType.fromTyped[A].isPrimitive

    // TODO: rename to builtInJvmTypes
    // TODO: add: java.lang.Class, java.lang.Object, java.lang.Enum, java.lang.EnumValue?
    // TODO: add: java.lang.reflect.*, java.lang.invoke.*
    /** Types which are either primitives or specially treated by JVM: Unit, String. */
    final val builtInTypes: List[??] = primitiveTypes ++ List(
      Type.of[String].as_??,
      Type.of[Unit].as_??
    )
    final def isBuiltIn[A: Type]: Boolean = UntypedType.fromTyped[A].isBuiltIn

    final def isAbstract[A: Type]: Boolean = UntypedType.fromTyped[A].isAbstract
    final def isFinal[A: Type]: Boolean = UntypedType.fromTyped[A].isFinal

    // TODO: rename class to something more unambiguous
    final def isClass[A: Type]: Boolean = UntypedType.fromTyped[A].isClass
    final def notBuiltInClass[A: Type]: Boolean = isClass[A] && !isBuiltIn[A]
    final def isPlainOldJavaObject[A: Type]: Boolean =
      notBuiltInClass[A] && !(isAbstract[A] || isSealed[A] || isJavaEnum[A] || isJavaEnumValue[A])
    final def isJavaBean[A: Type]: Boolean = false // TODO

    final def isSealed[A: Type]: Boolean = UntypedType.fromTyped[A].isSealed
    final def isJavaEnum[A: Type]: Boolean = UntypedType.fromTyped[A].isJavaEnum
    final def isJavaEnumValue[A: Type]: Boolean = UntypedType.fromTyped[A].isJavaEnumValue

    final def isCase[A: Type]: Boolean = UntypedType.fromTyped[A].isCase
    final def isObject[A: Type]: Boolean = UntypedType.fromTyped[A].isObject
    final def isVal[A: Type]: Boolean = UntypedType.fromTyped[A].isVal

    final def isCaseClass[A: Type]: Boolean = UntypedType.fromTyped[A].isCaseClass
    final def isCaseObject[A: Type]: Boolean = UntypedType.fromTyped[A].isCaseObject
    final def isCaseVal[A: Type]: Boolean = UntypedType.fromTyped[A].isCaseVal

    final def isAvailable[A: Type](scope: Accessible): Boolean = UntypedType.fromTyped[A].isAvailable(scope)

    final def isSubtypeOf[A: Type, B: Type]: Boolean = UntypedType.fromTyped[A] <:< UntypedType.fromTyped[B]
    final def isSameAs[A: Type, B: Type]: Boolean = UntypedType.fromTyped[A] =:= UntypedType.fromTyped[B]

    // Literal types

    val BooleanCodec: TypeCodec[Boolean]
    val ByteCodec: TypeCodec[Byte]
    val ShortCodec: TypeCodec[Short]
    val IntCodec: TypeCodec[Int]
    val LongCodec: TypeCodec[Long]
    val FloatCodec: TypeCodec[Float]
    val DoubleCodec: TypeCodec[Double]
    val CharCodec: TypeCodec[Char]
    val StringCodec: TypeCodec[String]
    val UnitCodec: TypeCodec[Unit]
    val NullCodec: TypeCodec[Null]

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
  }

  implicit final class TypeMethods[A](private val tpe: Type[A]) {

    def shortName: String = Type.shortName(using tpe)
    def fcqn: String = Type.fcqn(using tpe)
    def plainPrint: String = Type.plainPrint(using tpe)
    def prettyPrint: String = Type.prettyPrint(using tpe)

    def primaryConstructor: Option[Method.NoInstance[A]] = Method.primaryConstructorOf(using tpe)
    def defaultConstructor: Option[Method.NoInstance[A]] = constructors.find(_.isNullary)
    def constructors: List[Method.NoInstance[A]] = Method.constructorsOf(using tpe)

    def methods: List[Existential[Method[A, *]]] = Method.methodsOf(using tpe)

    def directChildren: Option[ListMap[String, ??<:[A]]] = Type.directChildren(using tpe)
    def exhaustiveChildren: Option[ListMap[String, ??<:[A]]] = Type.exhaustiveChildren(using tpe)

    def annotations: List[Expr_??] = Type.annotations(using tpe)

    def summonExpr: Option[Expr[A]] = Expr.summonImplicit(using tpe)

    def isAbstract: Boolean = Type.isAbstract(using tpe)
    def isFinal: Boolean = Type.isFinal(using tpe)

    def isClass: Boolean = Type.isClass(using tpe)
    def isJavaBean: Boolean = Type.isJavaBean(using tpe)

    def isSealed: Boolean = Type.isSealed(using tpe)
    def isJavaEnum: Boolean = Type.isJavaEnum(using tpe)
    def isJavaEnumValue: Boolean = Type.isJavaEnumValue(using tpe)

    def isCase: Boolean = Type.isCase(using tpe)
    def isObject: Boolean = Type.isObject(using tpe)
    def isVal: Boolean = Type.isVal(using tpe)

    def isCaseClass: Boolean = Type.isCaseClass(using tpe)
    def isCaseObject: Boolean = Type.isCaseObject(using tpe)
    def isCaseVal: Boolean = Type.isCaseVal(using tpe)

    def isAvailable(scope: Accessible): Boolean = Type.isAvailable[A](scope)(using tpe)

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
