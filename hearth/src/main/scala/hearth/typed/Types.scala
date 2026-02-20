package hearth
package typed

import hearth.fp.Id
import hearth.fp.data.*
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

trait Types extends TypeConstructors with TypesCrossQuotes with TypesCompat { this: MacroCommons =>

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
  trait TypeModule extends Ctors with TypeCrossQuotes with TypeCompat { this: Type.type =>

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
      val plain = plainPrint[A].takeWhile(_ != '[')
      // When we see ".type", don't call isObject (avoids cyclic dependency in Scala 3). Produce candidates for both:
      // Scala object ("foo.Bar.type" -> "foo.Bar$") and Java enum/singleton ("java.lang.Thread.State.type" -> "java.lang.Thread.State").
      val names = if (plain.endsWith(".type")) {
        val base = plain.dropRight(".type".length)
        def iter(n: String): Array[String] =
          scala.collection.Iterator
            .iterate(n)(_.reverse.replaceFirst("[.]", "\\$").reverse)
            .take(n.count(_ == '.') + 1)
            .toArray
            .reverse
        (iter(base + '$') ++ iter(base)).distinct
      } else {
        val name = plain
        scala.collection.Iterator
          .iterate(name)(_.reverse.replaceFirst("[.]", "\\$").reverse)
          .take(name.count(_ == '.') + 1)
          .toArray
          .reverse
      }
      names
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
    final def isIArray[A: Type]: Boolean = UntypedType.fromTyped[A].isIArray

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
    final def isNamedTuple[A: Type]: Boolean = UntypedType.fromTyped[A].isNamedTuple

    final def isAbstract[A: Type]: Boolean = UntypedType.fromTyped[A].isAbstract
    final def isFinal[A: Type]: Boolean = UntypedType.fromTyped[A].isFinal

    final def isClass[A: Type]: Boolean = UntypedType.fromTyped[A].isClass
    final def notJvmBuiltInClass[A: Type]: Boolean = isClass[A] && !isJvmBuiltIn[A]
    final def isPlainOldJavaObject[A: Type]: Boolean =
      notJvmBuiltInClass[A] && !(isAbstract[A] || isSealed[A] || isJavaEnum[A] || isJavaEnumValue[A] || isEnumeration[
        A
      ])
    final def isJavaBean[A: Type]: Boolean =
      !isObject[A] && isPlainOldJavaObject[A] && Type[A].defaultConstructor.exists(_.isAvailable(Everywhere))

    final def isSealed[A: Type]: Boolean = UntypedType.fromTyped[A].isSealed
    final def isJavaEnum[A: Type]: Boolean = UntypedType.fromTyped[A].isJavaEnum
    final def isJavaEnumValue[A: Type]: Boolean = UntypedType.fromTyped[A].isJavaEnumValue
    final def isEnumeration[A: Type]: Boolean = UntypedType.fromTyped[A].isEnumeration

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

    final def ClassCodec[A: Type]: TypeCodec[java.lang.Class[A]] = new TypeCodec[java.lang.Class[A]] {
      override def toType[B <: java.lang.Class[A]](value: B): Type[B] =
        Type.of[java.lang.Class[A]].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[java.lang.Class[A], Id]] =
        Type.classOfType[A].map { clazz =>
          Existential.UpperBounded[java.lang.Class[A], Id, java.lang.Class[A]](clazz)(using
            B.asInstanceOf[Type[java.lang.Class[A]]]
          )
        }
    }
    final def ClassTagCodec[A: Type]: TypeCodec[scala.reflect.ClassTag[A]] =
      new TypeCodec[scala.reflect.ClassTag[A]] {
        override def toType[B <: scala.reflect.ClassTag[A]](value: B): Type[B] =
          Type.of[scala.reflect.ClassTag[A]].asInstanceOf[Type[B]]
        override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[scala.reflect.ClassTag[A], Id]] =
          Type.classOfType[A].map { clazz =>
            Existential.UpperBounded[scala.reflect.ClassTag[A], Id, scala.reflect.ClassTag[A]](
              scala.reflect.ClassTag(clazz)
            )(using B.asInstanceOf[Type[scala.reflect.ClassTag[A]]])
          }
      }

    // Tuple codecs
    def Tuple1Codec[T1: TypeCodec]: TypeCodec[Tuple1[T1]]
    def Tuple2Codec[T1: TypeCodec, T2: TypeCodec]: TypeCodec[(T1, T2)]
    def Tuple3Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec]: TypeCodec[(T1, T2, T3)]
    def Tuple4Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec]: TypeCodec[(T1, T2, T3, T4)]
    def Tuple5Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5)]
    def Tuple6Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec, T6: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5, T6)]
    def Tuple7Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7)]
    def Tuple8Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8)]
    def Tuple9Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9)]
    def Tuple10Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)]
    def Tuple11Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]
    def Tuple12Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)]
    def Tuple13Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)]
    def Tuple14Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]
    def Tuple15Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)]
    def Tuple16Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)]
    def Tuple17Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)]
    def Tuple18Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)]
    def Tuple19Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)]
    def Tuple20Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)]
    def Tuple21Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)]
    def Tuple22Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec,
        T22: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)]

    // TODO: specialize for primitive types
    final def ArrayCodec[A: Type: TypeCodec]: TypeCodec[Array[A]] = new TypeCodec[Array[A]] {
      private lazy val ArrayCtor = Type.Ctor1.of[Array]
      override def toType[B <: Array[A]](value: B): Type[B] = ArrayCtor[A].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Array[A], Id]] =
        B match {
          case ArrayCtor(elem) =>
            import elem.Underlying as Elem
            for {
              ct <- Type.classOfType[A].map(scala.reflect.ClassTag[A](_))
              v <- TypeCodec[A].fromType(Type[Elem])
            } yield {
              implicit val classTag: scala.reflect.ClassTag[A] = ct
              Existential.UpperBounded[Array[A], Id, Array[A]](
                Array(v.value.asInstanceOf[A])
              )(using B.asInstanceOf[Type[Array[A]]])
            }
          case _ => None
        }
    }
    final def SeqCodec[A: Type: TypeCodec]: TypeCodec[Seq[A]] = new TypeCodec[Seq[A]] {
      private lazy val SeqCtor = Type.Ctor1.of[Seq]
      override def toType[B <: Seq[A]](value: B): Type[B] = SeqCtor[A].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Seq[A], Id]] =
        if (B =:= Type.of[Nil.type])
          Some(Existential.UpperBounded[Seq[A], Id, Seq[A]](Nil)(using B.asInstanceOf[Type[Seq[A]]]))
        else
          B match {
            case SeqCtor(elem) =>
              import elem.Underlying as Elem
              TypeCodec[A].fromType(Type[Elem]).map { v =>
                Existential.UpperBounded[Seq[A], Id, Seq[A]](
                  Seq(v.value.asInstanceOf[A])
                )(using B.asInstanceOf[Type[Seq[A]]])
              }
            case _ => None
          }
    }
    final def ListCodec[A: Type: TypeCodec]: TypeCodec[List[A]] = new TypeCodec[List[A]] {
      private lazy val ListCtor = Type.Ctor1.of[List]
      override def toType[B <: List[A]](value: B): Type[B] = ListCtor[A].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[List[A], Id]] =
        if (B =:= Type.of[Nil.type])
          Some(Existential.UpperBounded[List[A], Id, List[A]](Nil)(using B.asInstanceOf[Type[List[A]]]))
        else
          B match {
            case ListCtor(elem) =>
              import elem.Underlying as Elem
              TypeCodec[A].fromType(Type[Elem]).map { v =>
                Existential.UpperBounded[List[A], Id, List[A]](
                  List(v.value.asInstanceOf[A])
                )(using B.asInstanceOf[Type[List[A]]])
              }
            case _ => None
          }
    }
    final lazy val NilCodec: TypeCodec[Nil.type] = new TypeCodec[Nil.type] {
      override def toType[B <: Nil.type](value: B): Type[B] = Type.of[Nil.type].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Nil.type, Id]] =
        if (B =:= Type.of[Nil.type])
          Some(Existential.UpperBounded[Nil.type, Id, Nil.type](Nil)(using B.asInstanceOf[Type[Nil.type]]))
        else None
    }
    final def VectorCodec[A: Type: TypeCodec]: TypeCodec[Vector[A]] = new TypeCodec[Vector[A]] {
      private lazy val VectorCtor = Type.Ctor1.of[Vector]
      override def toType[B <: Vector[A]](value: B): Type[B] = VectorCtor[A].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Vector[A], Id]] =
        B match {
          case VectorCtor(elem) =>
            import elem.Underlying as Elem
            TypeCodec[A].fromType(Type[Elem]).map { v =>
              Existential.UpperBounded[Vector[A], Id, Vector[A]](
                Vector(v.value.asInstanceOf[A])
              )(using B.asInstanceOf[Type[Vector[A]]])
            }
          case _ => None
        }
    }
    final def MapCodec[K: Type: TypeCodec, V: Type: TypeCodec]: TypeCodec[Map[K, V]] = new TypeCodec[Map[K, V]] {
      private lazy val MapCtor = Type.Ctor2.of[Map]
      override def toType[B <: Map[K, V]](value: B): Type[B] = MapCtor[K, V].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Map[K, V], Id]] =
        B match {
          case MapCtor(k, v) =>
            import k.Underlying as K0
            import v.Underlying as V0
            for {
              dk <- TypeCodec[K].fromType(Type[K0])
              dv <- TypeCodec[V].fromType(Type[V0])
            } yield Existential.UpperBounded[Map[K, V], Id, Map[K, V]](
              Map(dk.value.asInstanceOf[K] -> dv.value.asInstanceOf[V])
            )(using B.asInstanceOf[Type[Map[K, V]]])
          case _ => None
        }
    }
    final def SetCodec[A: Type: TypeCodec]: TypeCodec[Set[A]] = new TypeCodec[Set[A]] {
      private lazy val SetCtor = Type.Ctor1.of[Set]
      override def toType[B <: Set[A]](value: B): Type[B] = SetCtor[A].asInstanceOf[Type[B]]
      override def fromType[B](B: Type[B]): Option[Existential.UpperBounded[Set[A], Id]] =
        B match {
          case SetCtor(elem) =>
            import elem.Underlying as Elem
            TypeCodec[A].fromType(Type[Elem]).map { v =>
              Existential.UpperBounded[Set[A], Id, Set[A]](
                Set(v.value.asInstanceOf[A])
              )(using B.asInstanceOf[Type[Set[A]]])
            }
          case _ => None
        }
    }
    def OptionCodec[A: TypeCodec]: TypeCodec[Option[A]]
    def SomeCodec[A: TypeCodec]: TypeCodec[Some[A]]
    def NoneCodec: TypeCodec[None.type]
    def EitherCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Either[L, R]]
    def LeftCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Left[L, R]]
    def RightCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Right[L, R]]

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
    def isIArray: Boolean = Type.isIArray(using tpe)
    def isJvmBuiltIn: Boolean = Type.isJvmBuiltIn(using tpe)
    def isTypeSystemSpecial: Boolean = Type.isTypeSystemSpecial(using tpe)
    def isOpaqueType: Boolean = Type.isOpaqueType(using tpe)
    def isTuple: Boolean = Type.isTuple(using tpe)
    def isNamedTuple: Boolean = Type.isNamedTuple(using tpe)

    def isAbstract: Boolean = Type.isAbstract(using tpe)
    def isFinal: Boolean = Type.isFinal(using tpe)

    def isClass: Boolean = Type.isClass(using tpe)
    def notJvmBuiltInClass: Boolean = Type.notJvmBuiltInClass(using tpe)
    def isPlainOldJavaObject: Boolean = Type.isPlainOldJavaObject(using tpe)
    def isJavaBean: Boolean = Type.isJavaBean(using tpe)

    def isSealed: Boolean = Type.isSealed(using tpe)
    def isJavaEnum: Boolean = Type.isJavaEnum(using tpe)
    def isJavaEnumValue: Boolean = Type.isJavaEnumValue(using tpe)
    def isEnumeration: Boolean = Type.isEnumeration(using tpe)

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
  object TypeCodec extends TypeCodecCompat {

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

    implicit def ClassCodec[A: Type]: TypeCodec[java.lang.Class[A]] = Type.ClassCodec[A]
    implicit def ClassTagCodec[A: Type]: TypeCodec[scala.reflect.ClassTag[A]] = Type.ClassTagCodec[A]

    implicit def ArrayCodec[A: Type: TypeCodec]: TypeCodec[Array[A]] = Type.ArrayCodec[A]
    implicit def SeqCodec[A: Type: TypeCodec]: TypeCodec[Seq[A]] = Type.SeqCodec[A]
    implicit def ListCodec[A: Type: TypeCodec]: TypeCodec[List[A]] = Type.ListCodec[A]
    implicit lazy val NilCodec: TypeCodec[Nil.type] = Type.NilCodec
    implicit def VectorCodec[A: Type: TypeCodec]: TypeCodec[Vector[A]] = Type.VectorCodec[A]
    implicit def MapCodec[K: Type: TypeCodec, V: Type: TypeCodec]: TypeCodec[Map[K, V]] = Type.MapCodec[K, V]
    implicit def SetCodec[A: Type: TypeCodec]: TypeCodec[Set[A]] = Type.SetCodec[A]
    implicit def OptionCodec[A: TypeCodec]: TypeCodec[Option[A]] = Type.OptionCodec[A]
    implicit def SomeCodec[A: TypeCodec]: TypeCodec[Some[A]] = Type.SomeCodec[A]
    implicit lazy val NoneCodec: TypeCodec[None.type] = Type.NoneCodec
    implicit def EitherCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Either[L, R]] = Type.EitherCodec[L, R]
    implicit def LeftCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Left[L, R]] = Type.LeftCodec[L, R]
    implicit def RightCodec[L: TypeCodec: Type, R: TypeCodec: Type]: TypeCodec[Right[L, R]] = Type.RightCodec[L, R]

    implicit def Tuple1Codec[T1: TypeCodec]: TypeCodec[Tuple1[T1]] = Type.Tuple1Codec[T1]
    implicit def Tuple2Codec[T1: TypeCodec, T2: TypeCodec]: TypeCodec[(T1, T2)] = Type.Tuple2Codec[T1, T2]
    implicit def Tuple3Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec]: TypeCodec[(T1, T2, T3)] =
      Type.Tuple3Codec[T1, T2, T3]
    implicit def Tuple4Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec]: TypeCodec[(T1, T2, T3, T4)] =
      Type.Tuple4Codec[T1, T2, T3, T4]
    implicit def Tuple5Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5)] =
      Type.Tuple5Codec[T1, T2, T3, T4, T5]
    implicit def Tuple6Codec[T1: TypeCodec, T2: TypeCodec, T3: TypeCodec, T4: TypeCodec, T5: TypeCodec, T6: TypeCodec]
        : TypeCodec[(T1, T2, T3, T4, T5, T6)] =
      Type.Tuple6Codec[T1, T2, T3, T4, T5, T6]
    implicit def Tuple7Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7)] =
      Type.Tuple7Codec[T1, T2, T3, T4, T5, T6, T7]
    implicit def Tuple8Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8)] =
      Type.Tuple8Codec[T1, T2, T3, T4, T5, T6, T7, T8]
    implicit def Tuple9Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
      Type.Tuple9Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9]
    implicit def Tuple10Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
      Type.Tuple10Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    implicit def Tuple11Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
      Type.Tuple11Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    implicit def Tuple12Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
      Type.Tuple12Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    implicit def Tuple13Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
      Type.Tuple13Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    implicit def Tuple14Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
      Type.Tuple14Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    implicit def Tuple15Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
      Type.Tuple15Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    implicit def Tuple16Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
      Type.Tuple16Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
    implicit def Tuple17Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
      Type.Tuple17Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    implicit def Tuple18Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
      Type.Tuple18Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    implicit def Tuple19Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
      Type.Tuple19Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    implicit def Tuple20Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec
    ]: TypeCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
      Type.Tuple20Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    implicit def Tuple21Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec
    ]: TypeCodec[
      (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)
    ] =
      Type.Tuple21Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    implicit def Tuple22Codec[
        T1: TypeCodec,
        T2: TypeCodec,
        T3: TypeCodec,
        T4: TypeCodec,
        T5: TypeCodec,
        T6: TypeCodec,
        T7: TypeCodec,
        T8: TypeCodec,
        T9: TypeCodec,
        T10: TypeCodec,
        T11: TypeCodec,
        T12: TypeCodec,
        T13: TypeCodec,
        T14: TypeCodec,
        T15: TypeCodec,
        T16: TypeCodec,
        T17: TypeCodec,
        T18: TypeCodec,
        T19: TypeCodec,
        T20: TypeCodec,
        T21: TypeCodec,
        T22: TypeCodec
    ]: TypeCodec[
      (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)
    ] =
      Type.Tuple22Codec[
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
        T18,
        T19,
        T20,
        T21,
        T22
      ]

    implicit def ModuleCodec[ModuleSingleton <: Singleton]: TypeCodec[ModuleSingleton] =
      Type.ModuleCodec[ModuleSingleton]
  }
}
