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

    override def isOpaqueType(instanceTpe: UntypedType): Boolean = {
      val sym = instanceTpe.dealias.typeSymbol
      !sym.isNoSymbol && sym.flags.is(Flags.Opaque)
    }

    override def isTuple(instanceTpe: UntypedType): Boolean = {
      val tupleBase = fromTyped[Tuple]
      val nonEmptyBase = fromTyped[NonEmptyTuple]
      instanceTpe <:< tupleBase && !(instanceTpe =:= tupleBase) && !(instanceTpe =:= nonEmptyBase)
    }

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // We use =:= to check whether A is known to be exactly of the built-in type or is it some upper bound.
      // Also exclude enumeration Value types (they're not abstract)
      !A.isNoSymbol && (A.flags.is(Flags.Abstract) || A.flags.is(Flags.Trait)) &&
      !Type.jvmBuiltInTypes.exists(tpe => instanceTpe =:= tpe.Underlying.asUntyped) &&
      !isEnumeration(instanceTpe)
    }
    private lazy val IArrayCtor = Type.Ctor1.of[IArray]

    override def isIArray(instanceTpe: UntypedType): Boolean =
      IArrayCtor.unapply(toTyped[Any](instanceTpe)).isDefined

    override def toClassJvmBuiltInExtra(untyped: UntypedType): Option[java.lang.Class[?]] =
      untyped.asTyped[Any] match {
        case IArrayCtor(elementType) =>
          toClass(elementType.asUntyped).map { elementClass =>
            scala.reflect.ClassTag(elementClass).newArray(0).getClass()
          }
        case _ => None
      }

    override def isFinal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // String is not being detected as a final in Scala 3, so we need to check it manually.
      // TODO: check if it's not a general issue with Java classes in Scala 3
      !A.isNoSymbol && ((A.flags.is(Flags.Final) || instanceTpe.asTyped[Any] <:< Type.of[String]) || isArray(
        instanceTpe
      ) || isIArray(instanceTpe))
    }

    override def isClass(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // String is not being detected as a class in Scala 3, so we need to check it manually.
      // TODO: check if it's not a general issue with Java classes in Scala 3
      !A.isNoSymbol && ((A.isClassDef || instanceTpe.asTyped[Any] <:< Type.of[String]) && !isArray(
        instanceTpe
      ) && !isIArray(instanceTpe))
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
    override def isEnumeration(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      if A.isNoSymbol then false
      else {
        val enumType = fromTyped[scala.Enumeration]
        // Case (a): Object extending Enumeration (e.g. WeekDay.type)
        // For object types, check if the module class extends Enumeration
        val isEnumerationObject = isObject(instanceTpe) && {
          // For WeekDay.type, the typeSymbol is the module term, we need the module class
          val moduleClass = if A.flags.is(Flags.Module) then A.moduleClass else A
          val enumSymbol = enumType.typeSymbol
          !moduleClass.isNoSymbol && {
            val moduleClassType = moduleClass.typeRef
            moduleClassType.baseClasses.contains(enumSymbol)
          }
        }
        // Case (b): Value type member of an Enumeration object (e.g. WeekDay.Value)
        // Check if the type is a type member/alias whose owner is an Enumeration object
        val isEnumerationValue = !isEnumerationObject && {
          // TypeRef.unapply can crash with ClassCastException on some TypeReprs (SimpleName vs TypeName),
          // so we use try-catch to guard against it.
          val isTypeRefToValue =
            try
              instanceTpe match {
                case TypeRef(qual, name) if name == "Value" =>
                  // Check if qualifier is a module type that extends Enumeration
                  qual.typeSymbol.flags.is(Flags.Module) && qual <:< enumType
                case _ => false
              }
            catch { case _: ClassCastException => false }

          isTypeRefToValue || {
            // Check via owner: the type symbol's owner chain
            val dealiased = instanceTpe.dealias
            val dealiasedSym = dealiased.typeSymbol
            def checkOwner(sym: Symbol): Boolean = {
              val owner = sym.owner
              !owner.isNoSymbol && owner.flags.is(Flags.Module) && {
                val ownerType = owner.typeRef
                ownerType <:< enumType
              }
            }
            // Check original symbol
            (A.isTypeDef && checkOwner(A)) ||
            // Check dealiased symbol (in case it's a type alias)
            (dealiasedSym != A && dealiasedSym.isTypeDef && checkOwner(dealiasedSym))
          }
        }
        isEnumerationObject || isEnumerationValue
      }
    }

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
        sym.flags.is(Flags.Enum) && (sym.flags.is(Flags.JavaStatic) || sym.flags.is(Flags.StableRealizable))
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
      else {
        val sym = untyped.typeSymbol
        val isOpaque = sym.flags.is(Flags.Opaque)
        // First try the standard companionModule lookup (works for classes)
        Option(sym.companionModule)
          .filterNot(_.isNoSymbol)
          .orElse {
            // For opaque types, companionModule returns NoSymbol because opaque types are type aliases.
            // We need to look for a module with the same name in the owner's declarations.
            if isOpaque then {
              sym.owner.declarations.find { s =>
                s.isValDef && s.name == sym.name && s.flags.is(Flags.Module)
              }
            } else None
          }
          .map { module =>
            // So... if you have `object Foo`, the `Foo` is a `Term` and have a `Symbol` (via `.companionModule`),
            // while its type is `Foo.type` and it has another `Symbol` (via `.moduleClass`).
            // We need to use 2 of them in different places, so we have to pass a tuple.
            (subtypeTypeOf(untyped, module.moduleClass), Ref(module))
          }
      }

    override def directChildren(instanceTpe: UntypedType): Option[ListMap[String, UntypedType]] =
      if isEnumeration(instanceTpe) then {
        // Determine if we have the object type or the Value type
        val enumObjectTypeOpt: Option[UntypedType] = if instanceTpe.typeSymbol.flags.is(Flags.Module) then {
          // We have the object type directly
          Some(instanceTpe)
        } else {
          // We have the Value type - find the companion object
          // In Scala 3, WeekDay.Value might be a TypeRef(WeekDay.type, "Value")
          val fromTypeRef =
            try
              instanceTpe match {
                case TypeRef(qual, _) if qual.typeSymbol.flags.is(Flags.Module) =>
                  Some(qual)
                case _ => None
              }
            catch { case _: ClassCastException => None }

          fromTypeRef.orElse {
            // Fallback: try owner chain
            val owner = instanceTpe.typeSymbol.owner
            if owner.isNoSymbol || !owner.flags.is(Flags.Module) then None
            else {
              // Get the module type
              Some(owner.typeRef)
            }
          }
        }

        enumObjectTypeOpt.flatMap { enumObjectType =>
          // Get the Value type as seen from the enum object (like Scala 2's enumObjectType.member(TypeName("Value")))
          // We look for the "Value" type member in the enum object's declarations or inherited members
          val enumType = fromTyped[scala.Enumeration]
          val enumSymbol = enumType.typeSymbol

          // Find the Value class in scala.Enumeration's declarations
          val valueClassSymOpt = enumSymbol.declarations
            .find(sym => sym.isClassDef && sym.name == "Value")

          valueClassSymOpt.flatMap { valueClassSym =>
            // In Scala 3, path-dependent types make <:< unreliable for Enumeration.this.Value vs WeekDay.Value.
            // Instead, we check if the val's type has Value (or a subclass) in its baseClasses.
            val children = enumObjectType.typeSymbol.declarations
              .filter(_.isValDef)
              .filter { term =>
                val termType = enumObjectType.memberType(term)
                termType.baseClasses.contains(valueClassSym)
              }
              .sorted
              .map { term =>
                // Use the singleton type (e.g. WeekDay.Mon.type) rather than the declared type (WeekDay.Value)
                // TermRef gives the properly qualified singleton type
                (subtypeName(term), TermRef(enumObjectType, term.name))
              }

            if children.isEmpty then None
            else Some(ListMap.from(children))
          }
        }
      } else if isSealed(instanceTpe) || isJavaEnum(instanceTpe) then Some(
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
