package hearth
package untyped

import hearth.fp.ignore
import scala.collection.immutable.ListMap

trait UntypedTypesScala3 extends UntypedTypes { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type UntypedType = TypeRepr

  object UntypedType extends UntypedTypeModule {

    object platformSpecific {

      def subtypeName(subtype: Symbol): String = {
        val name = subtype.name
        if name.endsWith('$'.toString) then name.dropRight(1) else name
      }

      /** Applies type arguments from supertype to subtype if there are any */
      def subtypeTypeOf(instanceTpe: UntypedType, subtype: Symbol): UntypedType =
        subtype.primaryConstructor.paramSymss match {
          // subtype takes type parameters
          case typeParamSymbols :: _ if typeParamSymbols.exists(_.isType) =>
            // we have to figure how subtypes type params map to parent type params
            val appliedTypeByParam: Map[String, TypeRepr] =
              subtype.typeRef
                .baseType(instanceTpe.typeSymbol)
                .typeArgs
                .map(_.typeSymbol.name)
                .zip(instanceTpe.typeArgs)
                .toMap
            // TODO: some better error message if child has an extra type param that doesn't come from the parent
            val typeParamReprs: List[TypeRepr] = typeParamSymbols.map(_.name).map(appliedTypeByParam)
            subtype.typeRef.appliedTo(typeParamReprs)
          // subtype is monomorphic
          case _ =>
            subtype.typeRef
        }
    }
    import platformSpecific.*

    override def fromTyped[A: Type]: UntypedType = TypeRepr.of[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = untyped.asType.asInstanceOf[Type[A]]

    override def position(untyped: UntypedType): Option[Position] = untyped.typeSymbol.pos

    override def fromClass(clazz: java.lang.Class[?]): UntypedType = TypeRepr.typeConstructorOf(clazz)

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // We use =:= to check whether A is known to be exactly of the build-in type or is it some upper bound.
      !A.isNoSymbol && (A.flags.is(Flags.Abstract) || A.flags.is(Flags.Trait)) &&
      !Type.builtInTypes.exists(tpe => instanceTpe =:= fromTyped(using tpe.Underlying))
    }
    override def isFinal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // String is not being detected as a final in Scala 3, so we need to check it manually.
      // TODO: check if it's not a general issue with Java classes in Scala 3
      !A.isNoSymbol && ((A.flags.is(Flags.Final) || instanceTpe.asTyped[Any] <:< Type.of[String]) || isArray(
        instanceTpe
      ))
    }

    override def isClass(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // String is not being detected as a class in Scala 3, so we need to check it manually.
      // TODO: check if it's not a general issue with Java classes in Scala 3
      !A.isNoSymbol && ((A.isClassDef || instanceTpe.asTyped[Any] <:< Type.of[String]) && !isArray(instanceTpe))
    }

    override def isSealed(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Sealed)
    }
    override def isJavaEnum(instanceTpe: UntypedType): Boolean =
      isEnumOrEnumValue(instanceTpe) && !instanceTpe.typeSymbol.flags.is(Flags.JavaStatic)
    override def isJavaEnumValue(instanceTpe: UntypedType): Boolean =
      isEnumOrEnumValue(instanceTpe) && instanceTpe.typeSymbol.flags.is(Flags.JavaStatic)
    private def isEnumOrEnumValue(instanceTpe: UntypedType): Boolean =
      instanceTpe <:< fromTyped[java.lang.Enum[?]] && instanceTpe.typeSymbol.flags.is(
        Flags.Enum | Flags.Final | Flags.JavaDefined
      )

    override def isCase(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Case)
    }
    override def isObject(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Module)
    }
    override def isVal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      def attempt(sym: Symbol): Boolean =
        A.flags.is(Flags.Enum) && (A.flags.is(Flags.JavaStatic) || A.flags.is(Flags.StableRealizable))
      !A.isNoSymbol && (attempt(A) || attempt(instanceTpe.termSymbol))
    }

    override def isAvailable(instanceTpe: UntypedType, scope: Accessible): Boolean = scope match {
      case Everywhere =>
        val A = instanceTpe.typeSymbol
        !(A.flags.is(Flags.Private) || A.flags.is(Flags.PrivateLocal) || A.flags.is(Flags.Protected) ||
          A.privateWithin.isDefined || A.protectedWithin.isDefined)
      case AtCallSite =>
        try {
          // Try to access the type in the current context.
          // If it's not accessible, this will throw an exception.
          // TODO: test this assumption
          val A0 = instanceTpe.as_??
          import A0.Underlying as A
          ignore('{ null.asInstanceOf[A] })
          true
        } catch {
          case _: Throwable => false
        }
    }

    override def isSubtypeOf(subtype: UntypedType, supertype: UntypedType): Boolean =
      quotes.reflect.TypeReprMethods.<:<(subtype)(supertype)
    override def isSameAs(a: UntypedType, b: UntypedType): Boolean =
      quotes.reflect.TypeReprMethods.=:=(a)(b)

    override def directChildren(instanceTpe: UntypedType): Option[ListMap[String, UntypedType]] =
      // no need for separate java.lang.Enum handling contrary to Scala 2
      if isSealed(instanceTpe) || isJavaEnum(instanceTpe)
      then Some( // TODO: check if isSealed should be aligned on Scala 2
        ListMap.from {
          def handleSymbols(sym: Symbol): Symbol =
            if sym.flags.is(Flags.Enum) then sym.typeRef.typeSymbol
            else if sym.flags.is(Flags.Module) then sym.typeRef.typeSymbol.moduleClass
            else sym

          // calling .distinct here as `children` returns duplicates for multiply-inherited types
          instanceTpe.typeSymbol.children
            .map(handleSymbols)
            // .sorted(Ordering.by(_.pos)) // TODO: check if this is necessary?
            .map(subtypeSymbol => subtypeName(subtypeSymbol) -> subtypeTypeOf(instanceTpe, subtypeSymbol))
        }
      )
      else None

    override def annotations(untyped: UntypedType): List[UntypedExpr] =
      untyped.typeSymbol.annotations
  }
}
