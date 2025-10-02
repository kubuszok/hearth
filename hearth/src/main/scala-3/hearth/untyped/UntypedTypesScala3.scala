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

      def symbolAvailable(symbol: Symbol, scope: Accessible): Boolean = {
        val owner = symbol.owner
        val enclosing = Symbol.spliceOwner
        def enclosings = Iterator.iterate(enclosing)(_.owner).takeWhile(!_.isNoSymbol)

        // Helper methods
        def isPrivate: Boolean = symbol.flags.is(Flags.Private)
        def isProtected: Boolean = symbol.flags.is(Flags.Protected)

        // High-level checks
        def isPublic: Boolean =
          !isPrivate && !isProtected && symbol.privateWithin.isEmpty && symbol.protectedWithin.isEmpty
        def isPrivateButInTheSameClass: Boolean = isPrivate && enclosing == owner
        def isProtectedButInTheSameClass: Boolean =
          isProtected && enclosing.isClassDef && (enclosing.typeRef <:< owner.typeRef)
        def isPrivateWithinButInTheRightPlace: Boolean =
          symbol.privateWithin.orElse(symbol.protectedWithin).exists { pw =>
            val pwType = pw.typeSymbol
            enclosings.exists(e => pwType == e || pwType == e.companionClass || pwType == e.companionModule)
          }

        scope match {
          case Everywhere => isPublic
          case AtCallSite =>
            isPublic || isPrivateButInTheSameClass || isProtectedButInTheSameClass || isPrivateWithinButInTheRightPlace
        }
      }

      def positionOf(symbol: Symbol): Option[Position] =
        symbol.pos
          // Removes values like "/BCDEF/java.base/java/lang/Object.sig" which are not actual positions.
          .filterNot(_.sourceFile.path.endsWith(".sig"))

      implicit val symbolOrdering: Ordering[Symbol] = {
        val stringSorting = hearth.fp.NaturalLanguageOrdering.caseSensitive
        Ordering.by((_: Symbol).pos).orElse(stringSorting.on((_: Symbol).name))
      }
    }
    import platformSpecific.*

    override def fromTyped[A: Type]: UntypedType = TypeRepr.of[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = untyped.asType.asInstanceOf[Type[A]]

    override def position(untyped: UntypedType): Option[Position] = positionOf(untyped.typeSymbol)

    override def fromClass(clazz: java.lang.Class[?]): UntypedType = TypeRepr.typeConstructorOf(clazz)

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // We use =:= to check whether A is known to be exactly of the built-in type or is it some upper bound.
      !A.isNoSymbol && (A.flags.is(Flags.Abstract) || A.flags.is(Flags.Trait)) &&
      !Type.jvmBuiltInTypes.exists(tpe => instanceTpe =:= tpe.Underlying.asUntyped)
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

    override def isAvailable(instanceTpe: UntypedType, scope: Accessible): Boolean =
      symbolAvailable(instanceTpe.typeSymbol, scope)

    override def isSubtypeOf(subtype: UntypedType, supertype: UntypedType): Boolean =
      quotes.reflect.TypeReprMethods.<:<(subtype)(supertype)
    override def isSameAs(a: UntypedType, b: UntypedType): Boolean =
      quotes.reflect.TypeReprMethods.=:=(a)(b)

    override def companionObject(untyped: UntypedType): Option[(UntypedType, UntypedExpr)] =
      if untyped.isObject then None
      else
        Option(untyped.typeSymbol.companionModule).filterNot(_.isNoSymbol).map { module =>
          // So... if you have `object Foo`, the `Foo` is a `Term` and have a `Symbol` (via `.companionModule`),
          // while its type is `Foo.type` and it has another `Symbol` (via `.moduleClass`).
          // We need to use 2 of them in different places, so we have to pass a tuple.
          (subtypeTypeOf(untyped, module.moduleClass), Ref(module))
        }

    override def directChildren(instanceTpe: UntypedType): Option[ListMap[String, UntypedType]] =
      // no need for separate java.lang.Enum handling contrary to Scala 2
      if isSealed(instanceTpe) || isJavaEnum(instanceTpe) then Some(
        ListMap.from {
          def handleSymbols(sym: Symbol): Symbol =
            if sym.flags.is(Flags.Enum) then sym.typeRef.typeSymbol
            else if sym.flags.is(Flags.Module) then sym.typeRef.typeSymbol.moduleClass
            else sym

          // calling .distinct here as `children` returns duplicates for multiply-inherited types
          instanceTpe.typeSymbol.children
            .map(handleSymbols)
            .sorted
            .map(subtypeSymbol => subtypeName(subtypeSymbol) -> subtypeTypeOf(instanceTpe, subtypeSymbol))
        }
      )
      else None

    override def annotations(untyped: UntypedType): List[UntypedExpr] =
      untyped.typeSymbol.annotations
  }
}
