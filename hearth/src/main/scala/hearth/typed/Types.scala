package hearth
package typed

import hearth.fp.Id
import hearth.fp.data.*
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

trait Types extends TypeConstructors with TypesCrossQuotes { this: MacroCommons =>

  /** Platform-specific type representation (`c.WeakTypeTag[A]` in 2, `scala.quoted.Type[A]` in 3).
    *
    * Typed [[Type]] and [[UntypedType]] exist because some macro operations do not require the full knowledge about the
    * type (because they operate on Symbols, and we would have to convert from the typed representation to Symbols to
    * use them), and some require the full knowledge about the type (because e.g. type parameters from the class have to
    * be applied to its methods and their arguments/returned values).
    *
    * The implementation will use the right underlying representation to perform the operations, and where possible
    * convert between typed and untyped representations, but the distinction would be useful in cases where some
    * operations are available to only one of them. Then the user could convert between them in the context where the
    * missing information is available.
    *
    * Note that existential type [[??]], is not an [[UntypedType]] - it's a typed representation, where the macro during
    * **its execution** would know the exact type BUT it's inconvenient for us to use generics to represent that exact
    * type during compilation of the macro itself (not its expansion).
    *
    * @since 0.1.0
    */
  type Type[A]

  val Type: TypeModule
  trait TypeModule extends Ctors with TypeCrossQuotes { this: Type.type =>

    /** Summons `Type` instance */
    final def apply[A](implicit A: Type[A]): Type[A] = A

    final def apply[A](value: A)(implicit codec: TypeCodec[A]): Type[A] = codec.toType(value)
    final def unapply[A](tpe: Type[A])(implicit codec: TypeCodec[A]): Option[A] = codec.fromType(tpe).map(_.value)

    def shortName[A: Type]: String
    final def fqcn[A: Type]: String = plainPrint[A].takeWhile(_ != '[')
    def plainPrint[A: Type]: String
    def prettyPrint[A: Type]: String

    /** This can only work if the type is available in the classpath, so it's not a good idea to use it for e.g. types
      * from the current project.
      */
    final def classOfType[A: Type]: Option[java.lang.Class[A]] =
      UntypedType.toClass(UntypedType.fromTyped[A]).map(_.asInstanceOf[java.lang.Class[A]])

    /** Attempts: top-level object, object in object, etc.
      *
      * We can use the resulting stream to find first class name that e.g. is an actual class, or is a class
      * representing a module singleton.
      *
      * Based on https://github.com/MateuszKubuszok/MacroTypeclass ideas.
      */
    final def possibleClassesOfType[A: Type]: Array[String] = {
      val name = {
        val plain = plainPrint[A].takeWhile(_ != '[')
        // TODO: test and list other cases where replacing .type with $ would help
        // TODO: test and add case when .type could be dropped without adding $ (Java enum values?)
        if (isObject[A]) {
          assert(plain.endsWith(".type"), s"$plain does not end with .type while it describes an object")
          plain.dropRight(".type".length) + '$'
        } else plain
      }

      scala.collection.Iterator
        .iterate(name)(_.reverse.replaceFirst("[.]", "\\$").reverse)
        .take(name.count(_ == '.') + 1) // ...then this is: "foo.bar.baz$", "foo.bar$baz$", "foo$bar$baz$"...
        .toArray
        .reverse // ...and this is: "foo.bar.baz$", "foo.bar$baz$", "foo$bar$baz$"
    }

    final def position[A: Type]: Option[Position] = UntypedType.fromTyped[A].position

    final def companionObject[A: Type]: Option[Expr_??] =
      UntypedType.fromTyped[A].companionObject.map { case (tpe, expr) =>
        val A0 = tpe.asTyped[A]
        val expr0 = expr.asTyped[A](using A0)
        Existential(expr0)(using A0)
      }

    final def directChildren[A: Type]: Option[ListMap[String, ??<:[A]]] =
      UntypedType.fromTyped[A].directChildren.map(m => ListMap.from(m.view.mapValues(_.asTyped[A].as_??<:[A])))
    final def exhaustiveChildren[A: Type]: Option[NonEmptyMap[String, ??<:[A]]] =
      UntypedType.fromTyped[A].exhaustiveChildren.map(m => m.map { case (k, v) => (k, v.asTyped[A].as_??<:[A]) })

    final def annotations[A: Type]: List[Expr_??] = UntypedType.fromTyped[A].annotations.map(UntypedExpr.as_??)

    /** Types which might be compiled to both JVM primitives and java.lang.Object: Boolean, Byte, Short, Char, Int,
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
    final def isArray[A: Type]: Boolean = UntypedType.fromTyped[A].isArray

    // TODO: add: java.lang.Class, java.lang.Object, java.lang.Enum, java.lang.EnumValue?
    // TODO: add: java.lang.reflect.*, java.lang.invoke.*
    /** Types which are either primitives or specially treated by JVM: Unit, String.
      *
      * Arrays are also considered built-in types, but we cannot list all possible array types, so we should check for
      * isArray instead.
      */
    final val jvmBuiltInTypes: List[??] = primitiveTypes ++ List(
      Type.of[String].as_??,
      Type.of[Unit].as_??
    )
    final def isJvmBuiltIn[A: Type]: Boolean = UntypedType.fromTyped[A].isJvmBuiltIn

    final val typeSystemSpecialTypes: List[??] = List(
      Type.of[Any].as_??,
      Type.of[AnyRef].as_??,
      Type.of[AnyVal].as_??,
      Type.of[Null].as_??,
      Type.of[Nothing].asInstanceOf[Type[Any]].as_?? // Type[Nothing] sees no extension methods
    )
    final def isTypeSystemSpecial[A: Type]: Boolean = UntypedType.fromTyped[A].isTypeSystemSpecial
    final def isOpaqueType[A: Type]: Boolean = UntypedType.fromTyped[A].isOpaqueType
    final def isTuple[A: Type]: Boolean = UntypedType.fromTyped[A].isTuple

    final def isAbstract[A: Type]: Boolean = UntypedType.fromTyped[A].isAbstract
    final def isFinal[A: Type]: Boolean = UntypedType.fromTyped[A].isFinal

    final def isClass[A: Type]: Boolean = UntypedType.fromTyped[A].isClass
    final def notJvmBuiltInClass[A: Type]: Boolean = isClass[A] && !isJvmBuiltIn[A]
    final def isPlainOldJavaObject[A: Type]: Boolean =
      notJvmBuiltInClass[A] && !(isAbstract[A] || isSealed[A] || isJavaEnum[A] || isJavaEnumValue[A])
    final def isJavaBean[A: Type]: Boolean =
      !isObject[A] && isPlainOldJavaObject[A] && Type[A].defaultConstructor.exists(_.isAvailable(Everywhere))

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

    def NullCodec: TypeCodec[Null]
    def UnitCodec: TypeCodec[Unit]
    def BooleanCodec: TypeCodec[Boolean]
    def ByteCodec: TypeCodec[Byte]
    def ShortCodec: TypeCodec[Short]
    def IntCodec: TypeCodec[Int]
    def LongCodec: TypeCodec[Long]
    def FloatCodec: TypeCodec[Float]
    def DoubleCodec: TypeCodec[Double]
    def CharCodec: TypeCodec[Char]
    def StringCodec: TypeCodec[String]

    // TODO: BigInt, BigDecimal, StringContext, Class, ClassTag, Tuple1-Tuple22

    // TODO: specialize for primitive types
    final def ArrayCodec[A: Type]: TypeCodec[Array[A]] = new TypeCodec[Array[A]] {
      override def toType[B <: Array[A]](value: B): Type[B] = Type.of[Array[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Array[A], Id]] = None // TODO
    }
    // TODO: specialize for: List, Vector, Nil, at least the types that are available in the standard library, etc
    final def SeqCodec[A: Type]: TypeCodec[Seq[A]] = new TypeCodec[Seq[A]] {
      override def toType[B <: Seq[A]](value: B): Type[B] = Type.of[Seq[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Seq[A], Id]] = None // TODO
    }
    final def ListCodec[A: Type]: TypeCodec[List[A]] = new TypeCodec[List[A]] {
      override def toType[B <: List[A]](value: B): Type[B] = Type.of[List[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[List[A], Id]] = None // TODO
    }
    final lazy val NilCodec: TypeCodec[Nil.type] = new TypeCodec[Nil.type] {
      override def toType[B <: Nil.type](value: B): Type[B] = Type.of[Nil.type].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Nil.type, Id]] = None // TODO
    }
    final def VectorCodec[A: Type]: TypeCodec[Vector[A]] = new TypeCodec[Vector[A]] {
      override def toType[B <: Vector[A]](value: B): Type[B] = Type.of[Vector[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Vector[A], Id]] = None // TODO
    }
    final def MapCodec[K: Type, V: Type]: TypeCodec[Map[K, V]] = new TypeCodec[Map[K, V]] {
      override def toType[B <: Map[K, V]](value: B): Type[B] = Type.of[Map[K, V]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Map[K, V], Id]] = None // TODO
    }
    final def SetCodec[A: Type]: TypeCodec[Set[A]] = new TypeCodec[Set[A]] {
      override def toType[B <: Set[A]](value: B): Type[B] = Type.of[Set[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Set[A], Id]] = None // TODO
    }
    final def OptionCodec[A: Type]: TypeCodec[Option[A]] = new TypeCodec[Option[A]] {
      override def toType[B <: Option[A]](value: B): Type[B] = Type.of[Option[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Option[A], Id]] = None // TODO
    }
    final def SomeCodec[A: Type]: TypeCodec[Some[A]] = new TypeCodec[Some[A]] {
      override def toType[B <: Some[A]](value: B): Type[B] = Type.of[Some[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Some[A], Id]] = None // TODO
    }
    final lazy val NoneCodec: TypeCodec[None.type] = new TypeCodec[None.type] {
      override def toType[B <: None.type](value: B): Type[B] = Type.of[None.type].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[None.type, Id]] = None // TODO
    }
    final def EitherCodec[L: Type, R: Type]: TypeCodec[Either[L, R]] = new TypeCodec[Either[L, R]] {
      override def toType[B <: Either[L, R]](value: B): Type[B] = Type.of[Either[L, R]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Either[L, R], Id]] = None // TODO
    }
    final def LeftCodec[L: Type, R: Type]: TypeCodec[Left[L, R]] = new TypeCodec[Left[L, R]] {
      override def toType[B <: Left[L, R]](value: B): Type[B] = Type.of[Left[L, R]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Left[L, R], Id]] = None // TODO
    }
    final def RightCodec[L: Type, R: Type]: TypeCodec[Right[L, R]] = new TypeCodec[Right[L, R]] {
      override def toType[B <: Right[L, R]](value: B): Type[B] = Type.of[Right[L, R]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Right[L, R], Id]] = None // TODO
    }

    private object ModuleCodecImpl extends TypeCodec[Any] {

      def toType[A](value: A): Type[A] = UntypedType.fromClass(value.getClass).asTyped[A]

      def fromType[A](tpe: Type[A]): Option[Existential.UpperBounded[Any, Id]] =
        possibleClassesOfType(tpe).collectFirst { case ModuleSingleton(value) =>
          Existential.UpperBounded[Any, Id, A](value.asInstanceOf[A])(using tpe)
        }

      /** Matches if name represents a module singleton existing in the classpath. */
      private object ModuleSingleton {
        def unapply(className: String): Option[Any] = try
          Option(java.lang.Class.forName(className).getField("MODULE$").get(null))
        catch {
          case _: Throwable => None
        }
      }
    }
    final def ModuleCodec[ModuleSingleton <: Singleton]: TypeCodec[ModuleSingleton] =
      ModuleCodecImpl.asInstanceOf[TypeCodec[ModuleSingleton]]
  }

  implicit final class TypeMethods[A](private val tpe: Type[A]) {

    def shortName: String = Type.shortName(using tpe)
    def fqcn: String = Type.fqcn(using tpe)
    def plainPrint: String = Type.plainPrint(using tpe)
    def prettyPrint: String = Type.prettyPrint(using tpe)

    def position: Option[Position] = Type.position(using tpe)

    def getRuntimeClass: Option[java.lang.Class[A]] = Type.classOfType(using tpe)

    def primaryConstructor: Option[Method.NoInstance[A]] = Method.primaryConstructorOf(using tpe)
    def defaultConstructor: Option[Method.NoInstance[A]] = constructors.find(_.isNullary)
    def constructors: List[Method.NoInstance[A]] = Method.constructorsOf(using tpe)

    def methods: List[Method.Of[A]] = Method.methodsOf(using tpe)

    def companionObject: Option[Expr_??] = Type.companionObject(using tpe)

    def directChildren: Option[ListMap[String, ??<:[A]]] = Type.directChildren(using tpe)
    def exhaustiveChildren: Option[NonEmptyMap[String, ??<:[A]]] = Type.exhaustiveChildren(using tpe)

    def annotations: List[Expr_??] = Type.annotations(using tpe)

    def summonExpr: SummoningResult[A] = Expr.summonImplicit(using tpe)
    def summonExprIgnoring(excluded: UntypedMethod*): SummoningResult[A] =
      Expr.summonImplicitIgnoring(excluded*)(using tpe)

    def isPrimitive: Boolean = Type.isPrimitive(using tpe)
    def isArray: Boolean = Type.isArray(using tpe)
    def isJvmBuiltIn: Boolean = Type.isJvmBuiltIn(using tpe)
    def isTypeSystemSpecial: Boolean = Type.isTypeSystemSpecial(using tpe)
    def isOpaqueType: Boolean = Type.isOpaqueType(using tpe)
    def isTuple: Boolean = Type.isTuple(using tpe)

    def isAbstract: Boolean = Type.isAbstract(using tpe)
    def isFinal: Boolean = Type.isFinal(using tpe)

    def isClass: Boolean = Type.isClass(using tpe)
    def notJvmBuiltInClass: Boolean = Type.notJvmBuiltInClass(using tpe)
    def isPlainOldJavaObject: Boolean = Type.isPlainOldJavaObject(using tpe)
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

  implicit final def ExistentialTypeMethods(tpe: ??): BoundedExistentialTypeMethods[Nothing, Any] =
    new BoundedExistentialTypeMethods[Nothing, Any](tpe)
  implicit final def LowerBoundedExistentialTypeMethods[L](tpe: ??>:[L]): BoundedExistentialTypeMethods[L, Any] =
    new BoundedExistentialTypeMethods[L, Any](tpe)
  implicit final def UpperBoundedExistentialTypeMethods[U](tpe: ??<:[U]): BoundedExistentialTypeMethods[Nothing, U] =
    new BoundedExistentialTypeMethods[Nothing, U](tpe)
  implicit final class BoundedExistentialTypeMethods[L, U >: L](private val tpe: L <:??<: U) {

    def shortName: String = Type.shortName(using tpe.Underlying)
    def fqcn: String = Type.fqcn(using tpe.Underlying)
    def plainPrint: String = Type.plainPrint(using tpe.Underlying)
    def prettyPrint: String = Type.prettyPrint(using tpe.Underlying)

    def asUntyped: UntypedType = UntypedType.fromTyped(using tpe.Underlying)
  }

  /** Generalizes over the idea of conversion between singleton/literal type values and their type representation.
    *
    * @since 0.1.0
    */
  trait TypeCodec[U] {

    def toType[A <: U](value: A): Type[A]
    def fromType[A](tpe: Type[A]): Option[Existential.UpperBounded[U, Id]]
  }
  object TypeCodec {

    def apply[A](implicit codec: TypeCodec[A]): TypeCodec[A] = codec

    implicit lazy val NullCodec: TypeCodec[Null] = Type.NullCodec
    implicit lazy val UnitCodec: TypeCodec[Unit] = Type.UnitCodec
    implicit lazy val BooleanCodec: TypeCodec[Boolean] = Type.BooleanCodec
    implicit lazy val ByteCodec: TypeCodec[Byte] = Type.ByteCodec
    implicit lazy val ShortCodec: TypeCodec[Short] = Type.ShortCodec
    implicit lazy val IntCodec: TypeCodec[Int] = Type.IntCodec
    implicit lazy val LongCodec: TypeCodec[Long] = Type.LongCodec
    implicit lazy val FloatCodec: TypeCodec[Float] = Type.FloatCodec
    implicit lazy val DoubleCodec: TypeCodec[Double] = Type.DoubleCodec
    implicit lazy val CharCodec: TypeCodec[Char] = Type.CharCodec
    implicit lazy val StringCodec: TypeCodec[String] = Type.StringCodec

    implicit def ArrayCodec[A: Type]: TypeCodec[Array[A]] = Type.ArrayCodec[A]
    implicit def SeqCodec[A: Type]: TypeCodec[Seq[A]] = Type.SeqCodec[A]
    implicit def ListCodec[A: Type]: TypeCodec[List[A]] = Type.ListCodec[A]
    implicit lazy val NilCodec: TypeCodec[Nil.type] = Type.NilCodec
    implicit def VectorCodec[A: Type]: TypeCodec[Vector[A]] = Type.VectorCodec[A]
    implicit def MapCodec[K: Type, V: Type]: TypeCodec[Map[K, V]] = Type.MapCodec[K, V]
    implicit def SetCodec[A: Type]: TypeCodec[Set[A]] = Type.SetCodec[A]
    implicit def OptionCodec[A: Type]: TypeCodec[Option[A]] = Type.OptionCodec[A]
    implicit def SomeCodec[A: Type]: TypeCodec[Some[A]] = Type.SomeCodec[A]
    implicit lazy val NoneCodec: TypeCodec[None.type] = Type.NoneCodec
    implicit def EitherCodec[L: Type, R: Type]: TypeCodec[Either[L, R]] = Type.EitherCodec[L, R]
    implicit def LeftCodec[L: Type, R: Type]: TypeCodec[Left[L, R]] = Type.LeftCodec[L, R]
    implicit def RightCodec[L: Type, R: Type]: TypeCodec[Right[L, R]] = Type.RightCodec[L, R]

    implicit def ModuleCodec[ModuleSingleton <: Singleton]: TypeCodec[ModuleSingleton] =
      Type.ModuleCodec[ModuleSingleton]
  }
}
