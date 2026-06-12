package hearth
package untyped

import hearth.fp.ignore
import hearth.fp.data.*
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
        // For setters (name_=), visibility may only be recorded on the corresponding getter/val.
        // Look it up so that e.g. private[scala] var next propagates to next_=.
        val effectiveSymbol =
          if symbol.name.endsWith("_=") then symbol.owner.fieldMember(symbol.name.stripSuffix("_=")) match {
            case s if !s.isNoSymbol => s
            case _                  => symbol
          }
          else symbol

        val owner = effectiveSymbol.owner
        val enclosing = Symbol.spliceOwner
        def enclosings = Iterator.iterate(enclosing)(_.owner).takeWhile(!_.isNoSymbol)

        // Helper methods
        def isPrivate: Boolean = effectiveSymbol.flags.is(Flags.Private)
        def isProtected: Boolean = effectiveSymbol.flags.is(Flags.Protected)

        // High-level checks
        def isPublic: Boolean =
          !isPrivate && !isProtected && effectiveSymbol.privateWithin.isEmpty && effectiveSymbol.protectedWithin.isEmpty
        def isPrivateButInTheSameClass: Boolean = isPrivate && enclosing == owner
        def isProtectedButInTheSameClass: Boolean =
          isProtected && enclosing.isClassDef && (enclosing.typeRef <:< owner.typeRef)
        def isPrivateWithinButInTheRightPlace: Boolean =
          effectiveSymbol.privateWithin.orElse(effectiveSymbol.protectedWithin).exists { pw =>
            val pwType = pw.typeSymbol
            enclosings.exists(e => pwType == e || pwType == e.companionClass || pwType == e.companionModule)
          }

        scope match {
          case Everywhere => isPublic
          case AtCallSite =>
            isPublic || isPrivateButInTheSameClass || isProtectedButInTheSameClass || isPrivateWithinButInTheRightPlace
          case Unrestricted => true
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

      private def callReflectMethod[R](tpe: TypeRepr, methodName: String): R = {
        val q = quotes
        val reflectModule = q.getClass.getMethod("reflect").invoke(q)
        val typeReprMethods = reflectModule.getClass.getMethod("TypeReprMethods").invoke(reflectModule)
        val method = typeReprMethods.getClass.getMethod(methodName, classOf[Object])
        method.invoke(typeReprMethods, tpe).asInstanceOf[R]
      }
      def typeReprParents(tpe: TypeRepr): List[TypeRepr] =
        callReflectMethod[List[TypeRepr]](tpe, "parents")
      def typeReprBaseClasses(tpe: TypeRepr): List[Symbol] =
        callReflectMethod[List[Symbol]](tpe, "baseClasses")

      /** Makes an annotation tree obtained from `Symbol.annotations` spliceable into macro output.
        *
        * On Scala 3, annotation trees carry no source spans, so splicing them into the macro result is rejected by
        * `-Xcheck-macros` ("position not set"). There is no public API to set a span on an existing tree, but every
        * tree created through the public `quotes.reflect` constructors is automatically assigned the macro-expansion
        * position. So we rebuild the annotation tree node by node: this preserves the structure (constructor call +
        * argument trees), keeping macro-time reading (e.g. `DestructuredExpr`, `Expr.semiEval`) intact while making the
        * tree safe to splice.
        *
        * Tree shapes that cannot appear in (or are irrelevant for) annotation constructor calls are returned as-is.
        */
      def repositionAnnotation(annotation: Term): Term = {
        def rebuild(term: Term): Term = term match {
          case Apply(fun, args)      => Apply(rebuild(fun), args.map(rebuild))
          case TypeApply(fun, args)  => TypeApply(rebuild(fun), args.map(arg => Inferred(arg.tpe)))
          case Select(qualifier, _)  => Select(rebuild(qualifier), term.symbol)
          case New(tpt)              => New(Inferred(tpt.tpe))
          case Literal(constant)     => Literal(constant)
          case ident: Ident          => if ident.symbol.isTerm then Ref(ident.symbol) else ident
          case Typed(expr, tpt)      => Typed(rebuild(expr), Inferred(tpt.tpe))
          case NamedArg(name, arg)   => NamedArg(name, rebuild(arg))
          case Repeated(elems, tpt)  => Repeated(elems.map(rebuild), Inferred(tpt.tpe))
          case Inlined(_, Nil, body) => rebuild(body)
          case other                 => other
        }
        rebuild(annotation)
      }
    }
    import platformSpecific.*

    override def fromTyped[A: Type]: UntypedType = TypeRepr.of[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = untyped.asType.asInstanceOf[Type[A]]

    override def position(untyped: UntypedType): Option[Position] = positionOf(untyped.typeSymbol)

    override def fromClassName(fullyQualifiedName: String): UntypedType =
      Symbol.requiredClass(fullyQualifiedName).typeRef

    override def fromClass(clazz: java.lang.Class[?]): UntypedType =
      TypeRepr.typeConstructorOf(clazz)

    override def isInJavaLangPackage(instanceTpe: UntypedType): Boolean = {
      val sym = instanceTpe.typeSymbol
      !sym.isNoSymbol && sym.owner.isPackageDef && sym.owner.fullName == "java.lang"
    }

    override def isOpaqueType(instanceTpe: UntypedType): Boolean = {
      val sym = instanceTpe.dealias.typeSymbol
      !sym.isNoSymbol && sym.flags.is(Flags.Opaque)
    }

    override def opaqueUnderlyingType(instanceTpe: UntypedType): Option[UntypedType] = {
      // `TypeRef.translucentSuperType` is the compiler-supported way of looking through an opaque type:
      // on an opaque TypeRef it returns `symbol.opaqueAlias.asSeenFrom(prefix, owner)` - the real RHS,
      // visible from outside the defining scope, correct through bounds (`opaque type X <: Int = Int`
      // resolves to `Int`, not the bound), nested opaque chains, and path-dependent prefixes.
      @scala.annotation.tailrec
      def loop(tpe: TypeRepr): TypeRepr = tpe match {
        case tr: TypeRef if tr.isOpaqueAlias                          => loop(tr.translucentSuperType.dealias)
        case AppliedType(tycon: TypeRef, args) if tycon.isOpaqueAlias =>
          tycon.translucentSuperType.dealias match {
            // For parameterized opaques (`opaque type Wrapper[A] = List[A]`) the translucent supertype of the
            // bare constructor is a TypeLambda - re-apply the original type arguments so that `Wrapper[Int]`
            // resolves to `List[Int]` rather than the bare `List` constructor (`appliedTo` beta-reduces).
            case tl: TypeLambda => loop(tl.appliedTo(args).dealias)
            // Not expected for an opaque constructor, but if the RHS is not a type lambda, stop here rather
            // than dropping the type arguments.
            case _ => tpe
          }
        case _ => tpe
      }
      val result = loop(instanceTpe.dealias)
      if result =:= instanceTpe then None else Some(result)
    }

    override def isTuple(instanceTpe: UntypedType): Boolean = {
      val tupleBase = fromTyped[Tuple]
      val nonEmptyBase = fromTyped[NonEmptyTuple]
      instanceTpe <:< tupleBase && !(instanceTpe =:= tupleBase) && !(instanceTpe =:= nonEmptyBase)
    }

    // Named tuple detection (Scala 3.7+ only; returns None on 3.3.x where the module doesn't exist)
    private lazy val namedTupleTypeSymbol: Option[Symbol] =
      try
        Symbol
          .requiredModule("scala.NamedTuple")
          .declaredType("NamedTuple")
          .headOption
      catch { case _: Throwable => None }

    override def isNamedTuple(instanceTpe: UntypedType): Boolean =
      namedTupleTypeSymbol.exists { ntSym =>
        val sym = instanceTpe.typeSymbol
        sym == ntSym || (instanceTpe match {
          case AppliedType(tycon, _) => tycon.typeSymbol == ntSym
          case _                     => false
        })
      }

    override def isUnionType(instanceTpe: UntypedType): Boolean =
      instanceTpe.dealias match {
        case _: OrType => true
        case _         => false
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

    override def isTrait(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Trait)
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
    override def isJavaEnum(instanceTpe: UntypedType): Boolean = {
      val flags = instanceTpe.typeSymbol.flags
      // Java enum classes have Enum | JavaDefined but may not have Final (e.g. enums with abstract methods)
      instanceTpe <:< fromTyped[java.lang.Enum[?]] && flags.is(Flags.Enum | Flags.JavaDefined) &&
      !flags.is(Flags.JavaStatic)
    }
    override def isJavaEnumValue(instanceTpe: UntypedType): Boolean = {
      val flags = instanceTpe.typeSymbol.flags
      instanceTpe <:< fromTyped[java.lang.Enum[?]] && flags.is(Flags.Enum | Flags.JavaDefined) &&
      flags.is(Flags.JavaStatic)
    }
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
            typeReprBaseClasses(moduleClassType).contains(enumSymbol)
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

    override def parents(instanceTpe: UntypedType): List[UntypedType] =
      instanceTpe.dealias match {
        case AndType(left, right) => List(left, right)
        case _                    =>
          if instanceTpe.typeSymbol.isNoSymbol then Nil
          else typeReprParents(instanceTpe)
      }

    override def baseClasses(instanceTpe: UntypedType): List[UntypedType] =
      if instanceTpe.typeSymbol.isNoSymbol then Nil
      else typeReprBaseClasses(instanceTpe).map(bc => instanceTpe.baseType(bc))

    private object experimentalReflect {
      private val symbolObj = Symbol.getClass
      private val newClassMethod = symbolObj.getMethods
        .find { m =>
          m.getName == "newClass" && m.getParameterCount == 5
        }
        .getOrElse(throw new NoSuchMethodException("Symbol.newClass with 5 params"))

      private val classDefObj = quotes.reflect.ClassDef.getClass
      private val classDefApplyMethod = classDefObj.getMethods
        .find { m =>
          m.getName == "apply" && m.getParameterCount == 3
        }
        .getOrElse(throw new NoSuchMethodException("ClassDef.apply with 3 params"))

      def newClass(
          owner: Symbol,
          name: String,
          parents: List[TypeRepr],
          decls: Symbol => List[Symbol],
          selfType: Option[TypeRepr]
      ): Symbol =
        newClassMethod
          .invoke(Symbol, owner, name, parents, decls, selfType)
          .asInstanceOf[Symbol]

      def classDefApply(cls: Symbol, parents: List[Tree], body: List[Statement]): ClassDef =
        classDefApplyMethod
          .invoke(quotes.reflect.ClassDef, cls, parents, body)
          .asInstanceOf[ClassDef]
    }

    override def unsafeNewSubtype(
        targetType: UntypedType,
        parentTypes: List[UntypedType],
        constructor: Option[UntypedMethod],
        constructorArgs: List[List[UntypedExpr]],
        overrides: List[UntypedOverride]
    ): Either[NonEmptyVector[String], UntypedExpr] = {
      val owner = Symbol.spliceOwner

      val overrideDecls = overrides.map { ovr =>
        val methodSym = ovr.method.symbol
        val memberType = safeMemberType(targetType, methodSym)
        (ovr, methodSym, memberType)
      }

      val effectiveParents =
        if parentTypes.headOption.exists(_.typeSymbol.flags.is(Flags.Trait)) then TypeRepr.of[AnyRef] :: parentTypes
        else
          parentTypes

      val cls = experimentalReflect.newClass(
        owner,
        "$anon",
        effectiveParents,
        decls = cls =>
          overrideDecls.map { case (ovr, _, memberType) =>
            Symbol.newMethod(
              cls,
              ovr.method.name,
              memberType,
              flags = Flags.Override,
              privateWithin = Symbol.noSymbol
            )
          },
        selfType = None
      )

      val overrideDefs = cls.declaredMethods.zip(overrideDecls).map { case (newMethodSym, (ovr, _, _)) =>
        DefDef(
          newMethodSym,
          argss => {
            val selfExpr: Term = This(cls)
            val flatParams: List[Term] = argss.flatten.collect { case t: Term => t }
            Some(ovr.body(selfExpr, flatParams).changeOwner(newMethodSym))
          }
        )
      }

      val classParent = effectiveParents.head
      val ctorSymbol = constructor.map(_.symbol).getOrElse(classParent.typeSymbol.primaryConstructor)
      val parentCtorCall =
        if constructorArgs.nonEmpty then {
          val ctor = New(TypeTree.of(using classParent.asType.asInstanceOf[scala.quoted.Type[Any]]))
            .select(ctorSymbol)
          val applied =
            if classParent.typeArgs.nonEmpty then ctor.appliedToTypes(classParent.typeArgs) else ctor
          applied.appliedToArgss(constructorArgs)
        } else {
          val ctor = New(TypeTree.of(using classParent.asType.asInstanceOf[scala.quoted.Type[Any]]))
            .select(ctorSymbol)
          ctor.appliedToNone
        }

      val traitParentTrees = effectiveParents.drop(1).map { tpe =>
        TypeTree.of(using tpe.asType.asInstanceOf[scala.quoted.Type[Any]])
      }
      val classDef = experimentalReflect.classDefApply(cls, parentCtorCall :: traitParentTrees, overrideDefs)
      val newExpr = Block(
        List(classDef),
        Typed(
          Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil),
          TypeTree.of(using targetType.asType.asInstanceOf[scala.quoted.Type[Any]])
        )
      )

      Right(newExpr)
    }

    // Cache for type comparison results, using identity-based lookup.
    // TypeRepr objects are typically reused for the same type within a macro expansion (e.g. lazy val Type.of[Int]),
    // so identity-based caching effectively deduplicates repeated comparisons across different provider/rule calls.
    private val subtypeCache =
      new java.util.IdentityHashMap[UntypedType, java.util.IdentityHashMap[UntypedType, java.lang.Boolean]]()
    private val sameTypeCache =
      new java.util.IdentityHashMap[UntypedType, java.util.IdentityHashMap[UntypedType, java.lang.Boolean]]()

    override def isSubtypeOf(subtype: UntypedType, supertype: UntypedType): Boolean = {
      // Fast path: reference equality means identical types
      if subtype eq supertype then return true
      // Fast negative: if the dealiased subtype is a simple class type (not an AndType, ConstantType, or
      // other compound type) and the supertype is final with a different symbol, they can't be in a subtype
      // relationship (except for bottom types Nothing/Null). We must dealias to avoid false negatives for
      // type aliases that expand to intersection types (e.g. `type Byte1 = 1 with Byte`).
      // We avoid pattern matching on TypeRef because its unapply in some Scala 3 versions throws
      // ClassCastException for enum value types (SimpleName cannot be cast to TypeName).
      val dealiased = subtype.dealias
      val subSym = dealiased.typeSymbol
      if !subSym.isNoSymbol && subSym.isClassDef && dealiased.typeArgs.isEmpty then {
        val supSym = supertype.typeSymbol
        if subSym != supSym && !supSym.isNoSymbol &&
        supSym.flags.is(Flags.Final) && supertype.typeArgs.isEmpty &&
        subSym != defn.NothingClass && subSym != defn.NullClass then return false
      }
      // Check cache before expensive compiler check
      var inner = subtypeCache.get(subtype)
      if inner == null then {
        inner = new java.util.IdentityHashMap()
        subtypeCache.put(subtype, inner): Unit
      }
      val cached = inner.get(supertype)
      if cached != null then return cached.booleanValue()
      val result = quotes.reflect.TypeReprMethods.<:<(subtype)(supertype)
      inner.put(supertype, java.lang.Boolean.valueOf(result)): Unit
      result
    }

    override def isSameAs(a: UntypedType, b: UntypedType): Boolean = {
      // Fast path: reference equality
      if a eq b then return true
      // Dealias for fast comparison (dealias is cheap — just follows alias chain)
      val ad = a.dealias
      val bd = b.dealias
      if ad eq bd then return true
      val adSym = ad.typeSymbol
      val bdSym = bd.typeSymbol
      // Fast negative: different non-NoSymbol dealiased symbols means definitely different types.
      // We do NOT use a fast positive here because same symbol doesn't guarantee same type
      // (e.g. singleton/literal types like `true` vs `Boolean`, or path-dependent types like
      // `WeekDay.Value` vs `Planet.Value`).
      if adSym != bdSym && !adSym.isNoSymbol && !bdSym.isNoSymbol then return false
      // Check cache before expensive compiler check
      var inner = sameTypeCache.get(a)
      if inner == null then {
        inner = new java.util.IdentityHashMap()
        sameTypeCache.put(a, inner): Unit
      }
      val cached = inner.get(b)
      if cached != null then return cached.booleanValue()
      val result = quotes.reflect.TypeReprMethods.=:=(a)(b)
      inner.put(b, java.lang.Boolean.valueOf(result)): Unit
      result
    }

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
                val termType = safeMemberType(enumObjectType, term)
                typeReprBaseClasses(termType).contains(valueClassSym)
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
      else if isUnionType(instanceTpe) then unionMemberDispatch(instanceTpe).flatMap { dispatch =>
        // Members that cannot be soundly discriminated by a runtime class test are acceptable only when
        // the user provides an implicit scala.reflect.TypeTest[Union, Member] (a total runtime
        // discriminator); otherwise the whole union is refused (see unionRefusalReason for diagnostics).
        val allMembersDiscriminable = dispatch.forall {
          case (member, UnionMemberDispatch.ByTypeTest) => userProvidedUnionTypeTestExists(instanceTpe, member)
          case _                                        => true
        }
        if allMembersDiscriminable then Some(ListMap.from(dispatch.map { case (tpe, _) =>
          UntypedType.plainPrint(tpe) -> tpe
        }))
        else None
      }
      else None

    /** How a generated pattern match would dispatch on a particular union member. */
    sealed private trait UnionMemberDispatch extends Product with Serializable
    private object UnionMemberDispatch {

      /** Stable singletons (literal types, modules, Scala 3 enum case vals) - matched by value/identity. */
      case object ByValue extends UnionMemberDispatch

      /** Members whose runtime class is determinable and disjoint from every other member's - matched with a
        * `case x: Member` class test.
        */
      final case class ByClassTest(runtimeClass: java.lang.Class[?]) extends UnionMemberDispatch

      /** Members whose runtime class is undeterminable (abstract types, type params) or related to another member's
        * (same/related erasure, e.g. `List[Int]` vs `List[String]`) - a class test would be unsound, so they require a
        * user-provided `scala.reflect.TypeTest[Union, Member]` extractor.
        */
      case object ByTypeTest extends UnionMemberDispatch
    }

    /** Classifies the members of a union type by how a generated pattern match has to dispatch on them.
      *
      * Returns `None` when the type is not a union we could ever decompose: not an `OrType` at all, fewer than 2
      * distinct members after dropping `Nothing` and duplicates, or members with static subtype relationships between
      * them (where "which branch should win" has no sound answer).
      */
    private def unionMemberDispatch(instanceTpe: UntypedType): Option[List[(UntypedType, UnionMemberDispatch)]] = {
      val nothingType = TypeRepr.of[Nothing]

      // Flatten nested OrType tree
      def flatten(tpe: TypeRepr): List[TypeRepr] = tpe.dealias match {
        case OrType(left, right) => flatten(left) ++ flatten(right)
        case other               => List(other)
      }

      if !isUnionType(instanceTpe) then None
      else {
        val allMembers = flatten(instanceTpe.dealias)

        // Filter Nothing, deduplicate
        val members = allMembers
          .filterNot(_ =:= nothingType)
          .foldLeft(Vector.empty[TypeRepr]) { (acc, tpe) =>
            if acc.exists(_ =:= tpe) then acc else acc :+ tpe
          }

        // Need >= 2 members for a meaningful union
        if members.size < 2 then None
        else {
          // Check no subtype relationships between members
          val hasSubtypeOverlap = members.exists { a =>
            members.exists(b => !(a =:= b) && (a <:< b || b <:< a))
          }
          if hasSubtypeOverlap then None
          else {
            // Stable singleton members (literal types, case objects/modules, Scala 3 enum case vals)
            // are matched BY VALUE (equality/identity test), not by a runtime class test, so they are
            // exempt from the runtime-class disjointness requirement below. They never overlap with
            // class-tested members either: that would require a static subtype relationship
            // (e.g. Color.Red.type <:< Color), which was already rejected by the pre-check above.
            def isSingletonMember(tpe: TypeRepr): Boolean = tpe match {
              case _: ConstantType => true
              case _               =>
                val termSym = tpe.termSymbol
                def isModuleRef =
                  !tpe.typeSymbol.isNoSymbol && tpe.typeSymbol.flags.is(Flags.Module)
                def isEnumCaseVal =
                  !termSym.isNoSymbol && termSym.flags.is(Flags.Enum) &&
                    (termSym.flags.is(Flags.JavaStatic) || termSym.flags.is(Flags.StableRealizable))
                isModuleRef || isEnumCaseVal
            }

            // Union values are boxed at runtime, so primitives have to be normalized to their boxed
            // representations (e.g. Int and java.lang.Integer are NOT disjoint at runtime).
            val boxedClasses = Map[java.lang.Class[?], java.lang.Class[?]](
              classOf[Unit] -> classOf[scala.runtime.BoxedUnit],
              classOf[Boolean] -> classOf[java.lang.Boolean],
              classOf[Byte] -> classOf[java.lang.Byte],
              classOf[Short] -> classOf[java.lang.Short],
              classOf[Int] -> classOf[java.lang.Integer],
              classOf[Long] -> classOf[java.lang.Long],
              classOf[Float] -> classOf[java.lang.Float],
              classOf[Double] -> classOf[java.lang.Double],
              classOf[Char] -> classOf[java.lang.Character]
            )

            // Compute the runtime class used by the `case x: Member` class test. Members whose runtime
            // class cannot be determined (abstract types, type params, refinements) cannot be proven
            // to dispatch soundly with a class test, so they require a TypeTest.
            def runtimeClass(tpe: TypeRepr): Option[java.lang.Class[?]] = {
              // Opaque types hide their underlying representation, but `opaqueUnderlyingType` can resolve
              // it from outside the defining scope - the class test dispatches on the underlying class
              // (e.g. `OpaqueId(= Long) | String` is accepted, while `OpaqueId | Long` requires TypeTests).
              val classTestedTpe = opaqueUnderlyingType(tpe).getOrElse(tpe)
              toClass(classTestedTpe.widen.dealias).map(clazz => boxedClasses.getOrElse(clazz, clazz))
            }

            val singletonFlags = members.map(isSingletonMember)
            val classes = members.zip(singletonFlags).map { case (tpe, isSingleton) =>
              if isSingleton then None else runtimeClass(tpe)
            }
            // A class test is sound only when no OTHER member's runtime class is RELATED to this one:
            // `case x: List[Int]` would also catch a `Seq[String]` value that happens to be a List,
            // dispatching to the wrong branch. (Equal classes - e.g. List[Int] vs List[String] - are a
            // special case of related ones.)
            def relatedToAnotherMember(i: Int): Boolean = classes(i).exists { a =>
              classes.indices.exists { j =>
                j != i && classes(j).exists(b => a.isAssignableFrom(b) || b.isAssignableFrom(a))
              }
            }

            Some(members.toList.zipWithIndex.map { case (tpe, i) =>
              if singletonFlags(i) then tpe -> UnionMemberDispatch.ByValue
              else
                classes(i) match {
                  case Some(clazz) if !relatedToAnotherMember(i) => tpe -> UnionMemberDispatch.ByClassTest(clazz)
                  case _                                         => tpe -> UnionMemberDispatch.ByTypeTest
                }
            })
          }
        }
      }
    }

    private lazy val typeTestSymbol = Symbol.requiredClass("scala.reflect.TypeTest")

    /** Whether a USER-PROVIDED `scala.reflect.TypeTest[Union, Member]` can be summoned at the expansion point.
      *
      * The compiler synthesizes `TypeTest` instances on its own whenever it considers the underlying class test
      * checkable (e.g. for `TypeTest[Int | java.lang.Integer, Int]`) - but members are classified as
      * [[UnionMemberDispatch.ByTypeTest]] precisely because that class test is NOT a sound discriminator at runtime, so
      * compiler-synthesized instances (inlined anonymous classes, returned as `Block` trees) are ignored; only actual
      * givens/implicits (resolved as references to a real symbol) count.
      */
    private def userProvidedUnionTypeTestExists(unionTpe: UntypedType, member: UntypedType): Boolean = {
      @scala.annotation.tailrec
      def isUserProvided(term: Term): Boolean = term match {
        case Inlined(_, _, inner) => isUserProvided(inner)
        case Typed(inner, _)      => isUserProvided(inner)
        case Block(_, _)          => false // compiler-synthesized anonymous TypeTest instance
        case other                => !other.symbol.isNoSymbol
      }
      Implicits.search(typeTestSymbol.typeRef.appliedTo(List(unionTpe.dealias, member))) match {
        case success: ImplicitSearchSuccess => isUserProvided(success.tree)
        case _                              => false
      }
    }

    override def unionMemberRequiresTypeTest(unionTpe: UntypedType, member: UntypedType): Boolean =
      unionMemberDispatch(unionTpe).exists(_.exists { case (tpe, dispatch) =>
        dispatch == UnionMemberDispatch.ByTypeTest && tpe =:= member
      })

    override def unionRefusalReason(instanceTpe: UntypedType): Option[String] =
      unionMemberDispatch(instanceTpe).flatMap { dispatch =>
        val missing = dispatch.collect {
          case (member, UnionMemberDispatch.ByTypeTest) if !userProvidedUnionTypeTestExists(instanceTpe, member) =>
            member
        }
        if missing.isEmpty then None
        else
          Some(
            missing
              .map { member =>
                s"union member ${UntypedType.plainPrint(member)} is not runtime-distinguishable; " +
                  s"provide an implicit scala.reflect.TypeTest[${UntypedType.plainPrint(instanceTpe)}, ${UntypedType.plainPrint(member)}]"
              }
              .mkString(", ")
          )
      }

    override def typeArguments(untyped: UntypedType): List[UntypedType] =
      untyped.typeArgs

    override def applyTypeArgs(untyped: UntypedType, args: List[UntypedType]): UntypedType =
      untyped.appliedTo(args)

    override def annotations(untyped: UntypedType): List[UntypedExpr] =
      untyped.typeSymbol.annotations.map(repositionAnnotation)
    override def annotationTypes(untyped: UntypedType): List[UntypedType] =
      untyped.typeSymbol.annotations.map(_.tpe)
  }
}
