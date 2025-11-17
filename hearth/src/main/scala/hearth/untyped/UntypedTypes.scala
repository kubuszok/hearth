package hearth.untyped

import hearth.MacroCommons
import hearth.fp.data.*
import scala.collection.immutable.ListMap

trait UntypedTypes { this: MacroCommons =>

  /** Platform-specific untyped type representation (`c.Type` in 2, `quotes.TypeRepr` in 3).
    *
    * Typed [[Type]] and [[UntypedType]] exist because some macro operations do not require the full knowledge about the
    * type (because they operate on Symbols, and we would have to convert from the typed representation to Symbols to
    * use them), and some require the full knowledge about the type (because e.g. type parameters from the class has to
    * be applied to its methods and their arguments/returned values).
    *
    * The implementation will use the right underlying representation to perform the operations, and where possible
    * convert between typed and untyped representations, but the distinction would be useful in cases where some
    * operations are only available to only one of them. Then user could covert between them in the context where the
    * missing information is available.
    *
    * Note that existential type [[??]], is not an [[UntypedType]] - it's a typed representaiton, where the macro during
    * **its excution** would know the exact type BUT it's inconvenient for us to use generics to represent that exact
    * type during compilation of the macro itself (not it's expansion).
    *
    * @since 0.1.0
    */
  type UntypedType

  val UntypedType: UntypedTypeModule
  trait UntypedTypeModule { this: UntypedType.type =>

    def fromTyped[A: Type]: UntypedType
    def toTyped[A](untyped: UntypedType): Type[A]
    final def as_??(untyped: UntypedType): ?? = toTyped[Any](untyped).as_??

    final def shortName(untyped: UntypedType): String = toTyped[Any](untyped).shortName
    final def fqcn(untyped: UntypedType): String = toTyped[Any](untyped).fqcn
    final def plainPrint(untyped: UntypedType): String = toTyped[Any](untyped).plainPrint
    final def prettyPrint(untyped: UntypedType): String = toTyped[Any](untyped).prettyPrint

    def position(untyped: UntypedType): Option[Position]

    def fromClass(clazz: java.lang.Class[?]): UntypedType
    final def toClass(untyped: UntypedType): Option[java.lang.Class[?]] =
      if (isTypeSystemSpecial(untyped)) {
        if (untyped =:= Type.of[Any].asUntyped) Some(classOf[Any])
        else if (untyped =:= Type.of[AnyRef].asUntyped) Some(classOf[AnyRef])
        else if (untyped =:= Type.of[AnyVal].asUntyped) Some(classOf[AnyVal])
        else if (untyped =:= Type.of[Null].asUntyped) Some(classOf[Null])
        else if (untyped =:= Type.of[Nothing].asInstanceOf[Type[Any]].asUntyped) Some(classOf[Nothing])
        else
          // $COVERAGE-OFF$
          hearthAssertionFailed(
            s"${untyped.prettyPrint} is recognized as type-system-special type, but is not handled by a type-system-special branch"
          )
        // $COVERAGE-ON$
      } else if (isJvmBuiltIn(untyped)) {
        // classOf[Unit].toString == "void" while possbleClassesOfType[Unit]....toString would resolve to "class scala.Unit"
        // If we want to be consistent, we have to fix this manually.
        if (untyped <:< Type.of[Unit].asUntyped) Some(classOf[Unit])
        else if (untyped <:< Type.of[Boolean].asUntyped) Some(classOf[Boolean])
        else if (untyped <:< Type.of[Byte].asUntyped) Some(classOf[Byte])
        else if (untyped <:< Type.of[Short].asUntyped) Some(classOf[Short])
        else if (untyped <:< Type.of[Int].asUntyped) Some(classOf[Int])
        else if (untyped <:< Type.of[Long].asUntyped) Some(classOf[Long])
        else if (untyped <:< Type.of[Float].asUntyped) Some(classOf[Float])
        else if (untyped <:< Type.of[Double].asUntyped) Some(classOf[Double])
        else if (untyped <:< Type.of[Char].asUntyped) Some(classOf[Char])
        else if (untyped <:< Type.of[String].asUntyped) Some(classOf[String])
        else
          untyped.asTyped[Any] match {
            case ArrayCtor(elementType) =>
              toClass(elementType.asUntyped).map { elementClass =>
                scala.reflect.ClassTag(elementClass).newArray(0).getClass()
              }
            case _ =>
              // $COVERAGE-OFF$
              hearthAssertionFailed(
                s"${untyped.prettyPrint} is recognized as built-in type, but is not handled by a built-in branch"
              )
            // $COVERAGE-ON$
          }
      } else
        Type.possibleClassesOfType(untyped.asTyped[Any]).collectFirst { case AvailableClass(value) =>
          value
        }

    final def isPrimitive(instanceTpe: UntypedType): Boolean =
      Type.primitiveTypes.exists(tpe => instanceTpe <:< fromTyped(using tpe.Underlying))
    final def isArray(instanceTpe: UntypedType): Boolean =
      ArrayCtor.unapply(toTyped[Any](instanceTpe)).isDefined
    final def isJvmBuiltIn(instanceTpe: UntypedType): Boolean =
      Type.jvmBuiltInTypes.exists(tpe => instanceTpe <:< fromTyped(using tpe.Underlying)) || isArray(instanceTpe)
    final def isTypeSystemSpecial(instanceTpe: UntypedType): Boolean =
      Type.typeSystemSpecialTypes.exists(tpe => instanceTpe =:= fromTyped(using tpe.Underlying))

    def isAbstract(instanceTpe: UntypedType): Boolean
    def isFinal(instanceTpe: UntypedType): Boolean

    def isClass(instanceTpe: UntypedType): Boolean

    def isSealed(instanceTpe: UntypedType): Boolean
    def isJavaEnum(instanceTpe: UntypedType): Boolean
    def isJavaEnumValue(instanceTpe: UntypedType): Boolean

    def isCase(instanceTpe: UntypedType): Boolean
    def isObject(instanceTpe: UntypedType): Boolean
    def isVal(instanceTpe: UntypedType): Boolean

    final def isCaseClass(instanceTpe: UntypedType): Boolean =
      isClass(instanceTpe) && !isObject(instanceTpe) && isCase(instanceTpe)
    final def isCaseObject(instanceTpe: UntypedType): Boolean = isObject(instanceTpe) && isCase(instanceTpe)
    final def isCaseVal(instanceTpe: UntypedType): Boolean = isVal(instanceTpe) && isCase(instanceTpe)

    def isAvailable(instanceTpe: UntypedType, scope: Accessible): Boolean

    def isSubtypeOf(subtype: UntypedType, supertype: UntypedType): Boolean
    def isSameAs(a: UntypedType, b: UntypedType): Boolean

    def companionObject(untyped: UntypedType): Option[(UntypedType, UntypedExpr)]

    def directChildren(instanceTpe: UntypedType): Option[ListMap[String, UntypedType]]
    final def exhaustiveChildren(instanceTpe: UntypedType): Option[NonEmptyMap[String, UntypedType]] =
      directChildren(instanceTpe)
        .flatMap(_.foldLeft[Option[Vector[(String, UntypedType)]]](Some(Vector.empty)) {
          case (None, _)                                                                    => None
          case (Some(vector), (_, subtype)) if subtype.isSealed && !subtype.isJavaEnumValue =>
            exhaustiveChildren(subtype).map(vector ++ _.toVector)
          case (_, (_, subtype)) if subtype.isAbstract => None
          case (Some(vector), nameSubtype)             => Some(vector :+ nameSubtype)
        })
        .map(_.filter(_._2 <:< instanceTpe)) // TODO: handle it somehow for GADT in abstract type context
        .map(ListMap.from(_))
        .flatMap(NonEmptyMap.fromListMap(_))

    def annotations(untyped: UntypedType): List[UntypedExpr]

    private lazy val ArrayCtor = Type.Ctor1.of[Array]

    /** Matches if name represents a class existing in the classpath. */
    private object AvailableClass {
      def unapply(className: String): Option[java.lang.Class[?]] = try
        Option(java.lang.Class.forName(className))
      catch {
        case _: Throwable => None
      }
    }
  }

  implicit final class UntypedTypeMethods(private val untyped: UntypedType) {

    def asTyped[A]: Type[A] = UntypedType.toTyped(untyped)
    def as_?? : ?? = UntypedType.as_??(untyped)

    def shortName: String = UntypedType.shortName(untyped)
    def fqcn: String = UntypedType.fqcn(untyped)
    def plainPrint: String = UntypedType.plainPrint(untyped)
    def prettyPrint: String = UntypedType.prettyPrint(untyped)

    def position: Option[Position] = UntypedType.position(untyped)

    def isPrimitive: Boolean = UntypedType.isPrimitive(untyped)
    def isArray: Boolean = UntypedType.isArray(untyped)
    def isJvmBuiltIn: Boolean = UntypedType.isJvmBuiltIn(untyped)
    def isTypeSystemSpecial: Boolean = UntypedType.isTypeSystemSpecial(untyped)

    def isAbstract: Boolean = UntypedType.isAbstract(untyped)
    def isFinal: Boolean = UntypedType.isFinal(untyped)

    def isClass: Boolean = UntypedType.isClass(untyped)

    def isSealed: Boolean = UntypedType.isSealed(untyped)
    def isJavaEnum: Boolean = UntypedType.isJavaEnum(untyped)
    def isJavaEnumValue: Boolean = UntypedType.isJavaEnumValue(untyped)

    def isCase: Boolean = UntypedType.isCase(untyped)
    def isObject: Boolean = UntypedType.isObject(untyped)
    def isVal: Boolean = UntypedType.isVal(untyped)

    def isCaseClass: Boolean = UntypedType.isCaseClass(untyped)
    def isCaseObject: Boolean = UntypedType.isCaseObject(untyped)
    def isCaseVal: Boolean = UntypedType.isCaseVal(untyped)

    def isAvailable(scope: Accessible): Boolean = UntypedType.isAvailable(untyped, scope)

    def <:<(other: UntypedType): Boolean = UntypedType.isSubtypeOf(untyped, other)
    def =:=(other: UntypedType): Boolean = UntypedType.isSameAs(untyped, other)

    def primaryConstructor: Option[UntypedMethod] = UntypedMethod.primaryConstructor(untyped)
    def constructors: List[UntypedMethod] = UntypedMethod.constructors(untyped)
    def methods: List[UntypedMethod] = UntypedMethod.methods(untyped)

    def companionObject: Option[(UntypedType, UntypedExpr)] = UntypedType.companionObject(untyped)

    def directChildren: Option[ListMap[String, UntypedType]] = UntypedType.directChildren(untyped)
    def exhaustiveChildren: Option[NonEmptyMap[String, UntypedType]] = UntypedType.exhaustiveChildren(untyped)

    def defaultValue(param: UntypedParameter): Option[UntypedMethod] = UntypedMethod.defaultValue(untyped)(param)

    def annotations: List[UntypedExpr] = UntypedType.annotations(untyped)
  }
}
