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
          case Anywhere => true
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
      def typeReprBaseClasses(tpe: TypeRepr): List[Symbol] =
        callReflectMethod[List[Symbol]](tpe, "baseClasses")

      // `TypeReprMethods` has no `parents` member on Scala 3.3.x (only `baseClasses`), and `Symbol.declaredParents`
      // does not exist there either. We derive the *direct* parents from the linearization: a base class is a direct
      // parent when no other base class (other than the type itself) has it among ITS own base classes. The result is
      // resolved against the instance type via `baseType` so type arguments are propagated.
      def typeReprDirectParents(tpe: TypeRepr): List[TypeRepr] = {
        val selfSym = tpe.typeSymbol
        val bases = typeReprBaseClasses(tpe).filterNot(_ == selfSym)
        // baseClasses of each base (excluding the base itself and self), used to drop transitive ancestors.
        val ancestorsOfBases: Set[Symbol] =
          bases.flatMap(b => typeReprBaseClasses(b.typeRef).filterNot(s => s == b || s == selfSym)).toSet
        bases.filterNot(ancestorsOfBases.contains).map(tpe.baseType)
      }

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

    // [hearth#315] The symbol to classify a type by. For an `export`-created alias of a real CLASS (e.g.
    // `export Inner.Foo`) the alias's own `typeSymbol` carries none of the class flags, so resolve through to the
    // underlying class. But ONLY when the dealiased symbol is a non-module class: this leaves aliases of OBJECTS (e.g.
    // `type EmptyTuple = EmptyTuple.type`, whose dealias is a module class and must stay `isClass=false`/non-final),
    // direct object types (e.g. an `Enumeration` object, which classify as before), and ordinary aliases untouched —
    // the change is purely additive (it only lets an export alias of a class see its class's flags).
    private def classificationSymbol(instanceTpe: UntypedType): Symbol = {
      val orig = instanceTpe.typeSymbol
      val dealiased = instanceTpe.dealias.typeSymbol
      if dealiased != orig && !dealiased.isNoSymbol && dealiased.isClassDef && !dealiased.flags.is(Flags.Module)
      then dealiased
      else orig
    }

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = classificationSymbol(instanceTpe)
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
          // [hearth#333] `IArray[E]` erases to an array, so its branch must ALWAYS answer with a `Class` (never fall
          // through to the shared "unhandled built-in" assertion, which exists to catch built-ins that have no branch
          // at all). Use the element's runtime class when resolvable; when it is not — e.g. `IArray[Int | String]`,
          // whose element is a union with no single class — fall back to the erased array-of-Object class.
          Some(
            toClass(elementType.asUntyped)
              .map(elementClass => scala.reflect.ClassTag(elementClass).newArray(0).getClass())
              .getOrElse(classOf[Array[Object]])
          )
        case _ => None
      }

    override def isFinal(instanceTpe: UntypedType): Boolean = {
      val A = classificationSymbol(instanceTpe) // resolve `export`-created aliases of classes, see #315
      // String is not being detected as a final in Scala 3, so we need to check it manually.
      // TODO: check if it's not a general issue with Java classes in Scala 3
      !A.isNoSymbol && ((A.flags.is(Flags.Final) || instanceTpe.asTyped[Any] <:< Type.of[String]) || isArray(
        instanceTpe
      ) || isIArray(instanceTpe))
    }

    override def isTrait(instanceTpe: UntypedType): Boolean = {
      val A = classificationSymbol(instanceTpe) // resolve `export`-created aliases of classes, see #315
      !A.isNoSymbol && A.flags.is(Flags.Trait)
    }

    override def isClass(instanceTpe: UntypedType): Boolean = {
      val A = classificationSymbol(instanceTpe) // resolve `export`-created aliases of classes, see #315
      // String is not being detected as a class in Scala 3, so we need to check it manually.
      // TODO: check if it's not a general issue with Java classes in Scala 3
      !A.isNoSymbol && ((A.isClassDef || instanceTpe.asTyped[Any] <:< Type.of[String]) && !isArray(
        instanceTpe
      ) && !isIArray(instanceTpe))
    }

    override def isSealed(instanceTpe: UntypedType): Boolean = {
      val A = classificationSymbol(instanceTpe) // resolve `export`-created aliases of classes, see #315
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
      // Parameterless Scala 3 enum cases (`enum Foo { case A }`) present their singleton with the enum CLASS as the
      // type symbol (no Case flag) while the `Case|Enum|StableRealizable` flags live on the TERM symbol - so we have to
      // inspect both, mirroring `isVal`. See issue #311.
      def hasCase(sym: Symbol): Boolean = !sym.isNoSymbol && sym.flags.is(Flags.Case)
      // Resolve `export`-created aliases of case classes (identity for non-alias/object types); the term symbol still
      // carries the `Case` flag for parameterless enum cases / case objects. See #315 and #311.
      hasCase(classificationSymbol(instanceTpe)) || hasCase(instanceTpe.termSymbol)
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
          else typeReprDirectParents(instanceTpe)
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

      // Resolve a member's result type against its own (re-bound) type parameters. `memberType` is the abstract
      // method's type as seen from the target (a PolyType over the ORIGINAL type-parameter binders for a generic
      // method); applying it to the actual type arguments handed to the DefDef body substitutes those binders with
      // the fresh type parameters and yields the concrete result — `String` for `def f[T](t: T): String`, the
      // re-bound `T` for `def f[T](t: T): T`. This replaces the old `Any` fallback that lost concrete returns of
      // generic methods. (We avoid `Symbol.info`, which is `@experimental`, hence the reuse of `memberType`.)
      def methodResultType(memberType: TypeRepr, typeArgs: List[TypeRepr]): TypeRepr = {
        val applied = memberType match {
          case pt: PolyType if typeArgs.nonEmpty => pt.appliedTo(typeArgs)
          case other                             => other
        }
        def walk(t: TypeRepr): TypeRepr = t match {
          case MethodType(_, _, res) => walk(res)
          case PolyType(_, _, res)   => walk(res)
          // A nullary `def value: Int` (or an abstract `val v: Int`) has member type `ByNameType(Int)` (`=> Int`);
          // the override's declared result is the strict `Int`, so strip the by-name wrapper to avoid synthesizing
          // `null.asInstanceOf[=> Int]` (or an ill-typed val).
          case ByNameType(res) => walk(res)
          case other           => other
        }
        walk(applied)
      }

      // Replaces the final result type of a (possibly curried / polymorphic) member type, preserving every parameter
      // clause. Used to retarget a `this.type` result onto the synthesized subtype.
      def replaceResult(tpe: TypeRepr, newResult: TypeRepr): TypeRepr = tpe match {
        case mt: MethodType => MethodType(mt.paramNames)(_ => mt.paramTypes, _ => replaceResult(mt.resType, newResult))
        case pt: PolyType   =>
          PolyType(pt.paramNames)(_ => pt.paramBounds, _ => replaceResult(pt.resType, newResult))
        // A nullary `def chain: this.type` has member type `ByNameType(...)` (`=> ...`); keep the by-name wrapper so
        // the synthesized symbol stays a method type (Scala 3 rejects a bare value type for a `def`).
        case ByNameType(_) => ByNameType(newResult)
        case _             => newResult
      }

      // Detects a `this.type` result (e.g. `def chain: this.type`). The member type, as seen from the target, has
      // already widened `this.type` to the parent, so the only reliable signal is the symbol's own declared return.
      def returnsThisType(sym: Symbol): Boolean =
        try
          sym.tree match {
            case d: DefDef =>
              d.returnTpt.tpe match {
                case _: ThisType => true
                case _           => false
              }
            case _ => false
          }
        catch { case _: Throwable => false }

      def isContextFunctionType(tpe: TypeRepr): Boolean = {
        val sym = tpe.dealias.typeSymbol
        !sym.isNoSymbol && sym.fullName.startsWith("scala.ContextFunction")
      }

      // A context-function result `def f(x): B ?=> C`. On newer Scala 3 (e.g. 3.8.4) `safeMemberType` desugars the
      // return into a trailing `using` parameter clause (`MethodType(x)(MethodType(using B)(C))`), which makes the
      // synthesized override return `C` where the trait returns `B ?=> C` — a runtime `ClassCastException` at the call
      // site. The source return type is still visible on the symbol's tree, so detect it there and recover both the
      // ContextFunctionN result and the count of REAL (pre-desugaring) value parameter clauses. On 3.3.8 the member
      // type is not desugared and this collapses to a no-op.
      def contextFunctionReturn(sym: Symbol): Option[(Int, TypeRepr)] =
        try
          sym.tree match {
            case d: DefDef if isContextFunctionType(d.returnTpt.tpe) =>
              val valueClauses = d.paramss.count {
                case _: TermParamClause => true
                case _                  => false
              }
              Some((valueClauses, d.returnTpt.tpe))
            case _ => None
          }
        catch { case _: Throwable => None }

      // Keeps the first `valueClauses` value parameter clauses of `tpe` and replaces everything after with `result`
      // (so a desugared trailing `using` clause from a context-function return is dropped). Type-parameter (Poly)
      // clauses pass through without consuming a value-clause budget.
      def truncateToResult(tpe: TypeRepr, valueClauses: Int, result: TypeRepr): TypeRepr =
        if valueClauses <= 0 then result
        else
          tpe match {
            case mt: MethodType =>
              MethodType(mt.paramNames)(_ => mt.paramTypes, _ => truncateToResult(mt.resType, valueClauses - 1, result))
            case pt: PolyType =>
              PolyType(pt.paramNames)(_ => pt.paramBounds, _ => truncateToResult(pt.resType, valueClauses, result))
            case _ => result
          }

      val overrideDecls = overrides.map { ovr =>
        val methodSym = ovr.method.symbol
        val rawMemberType = safeMemberType(targetType, methodSym)
        // Recover a context-function return that a newer compiler desugared into a trailing `using` clause.
        val memberType = contextFunctionReturn(methodSym) match {
          case Some((valueClauses, ctxFn)) => truncateToResult(rawMemberType, valueClauses, ctxFn)
          case None                        => rawMemberType
        }
        // An abstract `val` must be overridden with a `val`, not a `def` (Scala 3 rejects a method overriding a
        // stable value: "wrong type, expect a method type"). Vars keep the def/setter shape.
        val isValTarget = ovr.method.isVal && !ovr.method.isVar
        val isThisTypeTarget = !isValTarget && returnsThisType(methodSym)
        (ovr, methodSym, memberType, isValTarget, isThisTypeTarget)
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
          overrideDecls.map { case (ovr, _, memberType, isValTarget, isThisTypeTarget) =>
            if isValTarget then Symbol.newVal(
              cls,
              ovr.method.name,
              methodResultType(memberType, Nil),
              flags = Flags.Override,
              privateWithin = Symbol.noSymbol
            )
            else {
              // For a `this.type` result, retarget the result onto the synthesized subtype's own `this.type`, so that
              // returning `this` (`ctx.self`) conforms — otherwise the override returns the parent (does not conform).
              val effectiveType =
                if isThisTypeTarget then replaceResult(memberType, This(cls).tpe) else memberType
              Symbol.newMethod(
                cls,
                ovr.method.name,
                effectiveType,
                flags = Flags.Override,
                privateWithin = Symbol.noSymbol
              )
            }
          },
        selfType = None
      )

      // `declaredMethods`/`declaredFields` each preserve creation order, so zipping them with the same-kind subset of
      // `overrideDecls` (also in creation order) keeps the per-member alignment — including overloads sharing a name.
      val methodTargets = overrideDecls.filterNot(_._4)
      val valTargets = overrideDecls.filter(_._4)

      val methodDefs =
        cls.declaredMethods.zip(methodTargets).map { case (newMethodSym, (ovr, _, memberType, _, isThisTypeTarget)) =>
          DefDef(
            newMethodSym,
            argss => {
              val selfExpr: Term = This(cls)
              // Type arguments come first in `argss` for a generic member (re-bound type-parameter references), value
              // arguments are the Terms. Splitting them lets us surface the type parameters to the override body and
              // resolve the return type against this member's own type parameters.
              val typeParamReprs: List[TypeRepr] = argss.flatten.collect { case t: TypeTree => t.tpe }
              val flatParams: List[Term] = argss.flatten.collect { case t: Term => t }
              val returnTpe: TypeRepr = methodResultType(memberType, typeParamReprs)
              Some(
                ovr.body(selfExpr, flatParams, returnTpe, typeParamReprs, isThisTypeTarget).changeOwner(newMethodSym)
              )
            }
          )
        }

      val valDefs = cls.declaredFields.zip(valTargets).map { case (newValSym, (ovr, _, memberType, _, _)) =>
        val valType = methodResultType(memberType, Nil)
        ValDef(newValSym, Some(ovr.body(This(cls), Nil, valType, Nil, false).changeOwner(newValSym)))
      }

      val overrideDefs = methodDefs ++ valDefs

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
      directChildrenList(instanceTpe).map(ListMap.from)

    override def directChildrenList(instanceTpe: UntypedType): Option[List[(String, UntypedType)]] =
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
            else Some(children.toList)
          }
        }
      } else if isSealed(instanceTpe) || isJavaEnum(instanceTpe) then Some {
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
      else if isUnionType(instanceTpe) then unionMemberDispatch(instanceTpe).flatMap { dispatch =>
        // Members that cannot be soundly discriminated by a runtime class test are acceptable only when
        // the user provides an implicit scala.reflect.TypeTest[Union, Member] (a total runtime
        // discriminator); otherwise the whole union is refused (see unionRefusalReason for diagnostics).
        val allMembersDiscriminable = dispatch.forall {
          case (member, UnionMemberDispatch.ByTypeTest) => userProvidedUnionTypeTestExists(instanceTpe, member)
          case _                                        => true
        }
        if allMembersDiscriminable then Some(dispatch.map { case (tpe, _) =>
          UntypedType.plainPrint(tpe) -> tpe
        }.toList)
        else None
      }
      else None

    /** How a generated pattern match would dispatch on a particular union member. */
    sealed private trait UnionMemberDispatch extends Product with Serializable
    private object UnionMemberDispatch {

      /** Stable singletons (literal types, modules, Scala 3 enum case vals) - matched by value/identity. */
      case object ByValue extends UnionMemberDispatch

      /** Members whose erased class is determinable and disjoint from every other member's - matched with a
        * `case x: Member` class test.
        */
      case object ByClassTest extends UnionMemberDispatch

      /** Members whose erased class is undeterminable (abstract types, type params, refinements) or related to another
        * member's (same/related erasure, e.g. `List[Int]` vs `List[String]`) - a class test would be unsound, so they
        * require a user-provided `scala.reflect.TypeTest[Union, Member]` extractor.
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
            // are matched BY VALUE (equality/identity test), not by a class test, so they are
            // exempt from the erased-class disjointness requirement below. They never overlap with
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

            // Compute the ERASED CLASS SYMBOL used by the `case x: Member` class test, working at the
            // COMPILER-SYMBOL level (not via `java.lang.Class.forName`) so the check also works for types
            // defined in the SAME compilation run as the macro expansion, whose classfiles do not exist yet.
            //
            // Members whose erased class cannot be determined (abstract types, type params, refinements) cannot
            // be proven to dispatch soundly with a class test, so they require a TypeTest. Union values are boxed
            // at runtime, so primitive value-class symbols are normalized to their boxed counterparts (e.g. `Int`
            // and `java.lang.Integer` are NOT disjoint at runtime).
            def erasedClassSymbol(tpe: TypeRepr): Option[Symbol] = {
              // Opaque types hide their underlying representation, but `opaqueUnderlyingType` can resolve
              // it from outside the defining scope - the class test dispatches on the underlying class
              // (e.g. `OpaqueId(= Long) | String` is accepted, while `OpaqueId | Long` requires TypeTests).
              val classTestedTpe = opaqueUnderlyingType(tpe).getOrElse(tpe)
              // `classSymbol` returns the class symbol of the erased type - for an `AppliedType` this is the
              // tycon's class symbol (e.g. `List[Int]` -> `scala.collection.immutable.List`).
              classTestedTpe.widen.dealias.classSymbol.map(sym => boxedClassSymbols.getOrElse(sym, sym))
            }

            val singletonFlags = members.map(isSingletonMember)
            val classSymbols = members.zip(singletonFlags).map { case (tpe, isSingleton) =>
              if isSingleton then None else erasedClassSymbol(tpe)
            }
            // A class test is sound only when no OTHER member's erased class is RELATED to this one:
            // `case x: List[Int]` would also catch a `Seq[String]` value that happens to be a List,
            // dispatching to the wrong branch. (Equal classes - e.g. List[Int] vs List[String] - are a
            // special case of related ones.) Relatedness is checked at the class level: A and B are related
            // iff A's erased class derives from B's OR vice versa, tested through their class symbols' typeRefs
            // (`TypeRepr.derivesFrom(cls)` is the compiler-symbol-level subclass check).
            def relatedToAnotherMember(i: Int): Boolean = classSymbols(i).exists { a =>
              classSymbols.indices.exists { j =>
                j != i && classSymbols(j).exists(b => a.typeRef.derivesFrom(b) || b.typeRef.derivesFrom(a))
              }
            }

            Some(members.toList.zipWithIndex.map { case (tpe, i) =>
              if singletonFlags(i) then tpe -> UnionMemberDispatch.ByValue
              else
                classSymbols(i) match {
                  case Some(_) if !relatedToAnotherMember(i) => tpe -> UnionMemberDispatch.ByClassTest
                  case _                                     => tpe -> UnionMemberDispatch.ByTypeTest
                }
            })
          }
        }
      }
    }

    /** Union values are boxed at runtime, so primitive class symbols have to be normalized to their boxed counterparts
      * before the disjointness comparison (e.g. `Int` and `java.lang.Integer` are NOT disjoint at runtime). Built once:
      * keyed by the primitive value class symbol, valued by the boxed class symbol.
      */
    private lazy val boxedClassSymbols: Map[Symbol, Symbol] = Map(
      TypeRepr.of[Unit].classSymbol.get -> Symbol.requiredClass("scala.runtime.BoxedUnit"),
      TypeRepr.of[Boolean].classSymbol.get -> Symbol.requiredClass("java.lang.Boolean"),
      TypeRepr.of[Byte].classSymbol.get -> Symbol.requiredClass("java.lang.Byte"),
      TypeRepr.of[Short].classSymbol.get -> Symbol.requiredClass("java.lang.Short"),
      TypeRepr.of[Int].classSymbol.get -> Symbol.requiredClass("java.lang.Integer"),
      TypeRepr.of[Long].classSymbol.get -> Symbol.requiredClass("java.lang.Long"),
      TypeRepr.of[Float].classSymbol.get -> Symbol.requiredClass("java.lang.Float"),
      TypeRepr.of[Double].classSymbol.get -> Symbol.requiredClass("java.lang.Double"),
      TypeRepr.of[Char].classSymbol.get -> Symbol.requiredClass("java.lang.Character")
    )

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

    override def dealias(untyped: UntypedType): UntypedType =
      untyped.dealias

    override def typeConstructor(untyped: UntypedType): UntypedType =
      untyped.dealias match {
        case AppliedType(tycon, _) => tycon
        case other                 => other
      }

    override def typeArguments(untyped: UntypedType): List[UntypedType] =
      untyped.typeArgs

    override def applyTypeArgs(untyped: UntypedType, args: List[UntypedType]): UntypedType =
      // Apply to the (dealiased) type CONSTRUCTOR rather than the type as-given, so that re-applying an
      // already-applied type (e.g. a wildcard example `F[Any, ?]`) replaces its arguments instead of silently
      // producing a malformed `F[Any, ?][Int, String]`. This matches Scala 2's `appliedType(typeConstructor, args)`.
      // See issue #312.
      typeConstructor(untyped).appliedTo(args)

    override def sameTypeConstructorAs(a: UntypedType, b: UntypedType): Boolean =
      typeConstructor(a).typeSymbol == typeConstructor(b).typeSymbol

    private val noSymbolCacheBucket = new AnyRef
    override private[hearth] def cacheBucketKey(untyped: UntypedType): AnyRef = {
      val symbol = untyped.dealias.typeSymbol
      if symbol.isNoSymbol then noSymbolCacheBucket else symbol
    }

    override def annotations(untyped: UntypedType): List[UntypedExpr] =
      untyped.typeSymbol.annotations.map(repositionAnnotation)
    override def annotationTypes(untyped: UntypedType): List[UntypedType] =
      untyped.typeSymbol.annotations.map(_.tpe)

    // Type-position annotations live on `AnnotatedType(underlying, annot)` wrappers (`X @Ann`), not on the type symbol;
    // stacked annotations nest (`X @A @B` => `AnnotatedType(AnnotatedType(X, @A), @B)`), so we walk them all. Dealias so
    // alias forms (`type Y = X @Ann`) are seen through. See issue #306.
    private def annotatedTypeTerms(untyped: UntypedType): List[Term] = {
      // Match `AnnotatedType` BEFORE dealiasing: on Scala 3 `AnnotatedType(X, @Ann).dealias` strips the annotation.
      // Dealias only to see through a plain alias to an annotated type (`type Y = X @Ann`); the `d != tpe` guard stops
      // recursion at a fixpoint.
      def loop(tpe: TypeRepr): List[Term] = tpe match {
        case AnnotatedType(underlying, annot) => annot :: loop(underlying)
        case _                                =>
          val d = tpe.dealias
          if d != tpe then loop(d) else Nil
      }
      loop(untyped)
    }
    override def typeAnnotations(untyped: UntypedType): List[UntypedExpr] =
      annotatedTypeTerms(untyped).map(repositionAnnotation)
    override def typeAnnotationTypes(untyped: UntypedType): List[UntypedType] =
      annotatedTypeTerms(untyped).map(_.tpe)
  }
}
