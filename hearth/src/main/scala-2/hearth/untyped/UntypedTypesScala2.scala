package hearth
package untyped

import hearth.fp.data.*
import scala.collection.immutable.ListMap

trait UntypedTypesScala2 extends UntypedTypes { this: MacroCommonsScala2 =>

  import c.universe.*
  import Type.platformSpecific.*

  final override type UntypedType = c.Type

  object UntypedType extends UntypedTypeModule {

    object platformSpecific {

      /** Finds the actual companion symbol for the given type.
        *
        * Borrowed from Jsoniter-Scala:
        * https://github.com/plokhotnyuk/jsoniter-scala/blob/b14dbe51d3ae6752e5a9f90f1f3caf5bceb5e4b0/jsoniter-scala-macros/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L462
        * which in turn Borrowed from Magnolia:
        * https://github.com/propensive/magnolia/blob/f21f2aabb49e43b372240e98ec77981662cc570c/core/shared/src/main/scala/magnolia.scala#L123-L155
        * which I believe borrowed it from AVSystem/scala-commons (?) (the first version of ownerChain that I found):
        * https://github.com/AVSystem/scala-commons/blob/51776d33d48050fc201357a83ed469da5a60dbf2/commons-macros/src/main/scala/com/avsystem/commons/macros/MacroCommons.scala#L19
        *
        * Basically, the issue is that: when you ask for a symbol of a companion object, you'll get some symbol... but
        * it might not be an object.
        *
        * When you have things like:
        * {{{
        * object Foo {
        *   object Bar {
        *     object Baz {
        *       ...
        *     }
        *   }
        * }
        * }}}
        *
        * only the outermost object is an actual object, the inner ones are not necessarily treated modules: they might
        * not contains the default values definitions, etc.
        *
        * Similarly messed up situation with classes containing objects containing classes, etc.
        *
        * This function basically finds the Symbol that can be used as an actual companion object.
        */
      def companionSymbol(untyped: UntypedType): scala.util.Try[Symbol] = {
        val sym = untyped.typeSymbol
        val comp = sym.companion
        if (comp.isModule) scala.util.Success(comp)
        else {
          val ownerChainOf: Symbol => Iterator[Symbol] =
            s => Iterator.iterate(s)(_.owner).takeWhile(x => x != null && x != NoSymbol).toVector.reverseIterator
          val path = ownerChainOf(sym)
            .zipAll(ownerChainOf(c.internal.enclosingOwner), NoSymbol, NoSymbol)
            .dropWhile { case (x, y) => x == y }
            .takeWhile(_._1 != NoSymbol)
            .map(_._1.name.toTermName)
          // $COVERAGE-OFF$ Should never happen unless someone mess around with type-level representation
          scala.util.Try {
            if (path.isEmpty) hearthAssertionFailed(s"Cannot find a companion for ${untyped.prettyPrint}")
            else c.typecheck(path.foldLeft[Tree](Ident(path.next()))(Select(_, _)), silent = true).symbol
          }
          // $COVERAGE-ON$
        }
      }

      def subtypeName(typeSymbol: TypeSymbol): String = typeSymbol.name.toString

      /** Applies type arguments from supertype to subtype if there are any */
      def subtypeTypeOf(instanceTpe: UntypedType, subtypeSymbol: TypeSymbol): UntypedType = {
        val sEta = subtypeSymbol.toType.etaExpand

        sEta.finalResultType.substituteTypes(
          sEta.baseType(instanceTpe.typeSymbol).typeArgs.map(_.typeSymbol),
          instanceTpe.typeArgs
        )
      }

      def symbolName(symbol: Symbol): String = symbol.name.decodedName.toString

      private val syntheticCaseAccessorName = "(.+)\\$access\\$\\d+".r

      def symbolAvailable(symbol0: Symbol, scope: Accessible): Boolean = {
        // For a non-public case field `b`, Scala 2 marks the public synthetic accessor `b$access$1` as the case
        // accessor, while the user-declared member keeps its declared visibility (Scala 3 has no such synthetic
        // accessor and keeps the restricted `b` as the case accessor). To keep visibility filtering consistent
        // between the platforms, we answer for the underlying user-declared member instead of the synthetic accessor.
        val symbol = symbolName(symbol0) match {
          case syntheticCaseAccessorName(fieldName) if symbol0.isMethod && symbol0.asMethod.isCaseAccessor =>
            symbol0.owner.typeSignature.member(TermName(fieldName)) match {
              case NoSymbol   => symbol0
              case underlying => underlying
            }
          case _ => symbol0
        }

        val owner = symbol.owner
        val enclosing = c.internal.enclosingOwner

        // Helper methods
        def privateWithin: Option[Symbol] = Option(symbol.privateWithin).filterNot(_ == NoSymbol)

        // High-level checks
        def isPublic: Boolean = symbol.isPublic
        def isPrivateButInTheSameClass: Boolean = symbol.isPrivate && enclosing == owner
        def isProtectedButInTheSameClass: Boolean =
          symbol.isProtected && enclosing.isClass && enclosing.asClass.toType <:< owner.asClass.toType
        def isPrivateWithinButInTheRightPlace: Boolean = privateWithin.exists { pw =>
          def isPackagePrivateButInTheRightPackage: Boolean = pw.isPackage && {
            val en = enclosing.fullName.toString
            val pn = pw.fullName.toString
            en == pn || en.startsWith(pn + ".")
          }
          isPackagePrivateButInTheRightPackage
        }

        scope match {
          case Everywhere => isPublic
          case AtCallSite =>
            isPublic || isPrivateButInTheSameClass || isProtectedButInTheSameClass || isPrivateWithinButInTheRightPlace
          case Anywhere => true
        }
      }

      def positionOf(symbol: Symbol): Option[Position] =
        Option(symbol.pos)
          .filter(_ != NoPosition)
          // Prevent crash in case of https://github.com/scala/scala3/issues/21672
          .filter(pos => scala.util.Try(pos.start).isSuccess)

      implicit val symbolOrdering: Ordering[Symbol] = {
        val stringSorting = hearth.fp.NaturalLanguageOrdering.caseSensitive
        Ordering.by(positionOf).orElse(stringSorting.on(symbolName))
      }
    }
    import platformSpecific.*

    override def fromTyped[A: Type]: UntypedType = c.weakTypeOf[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = c.WeakTypeTag(untyped)

    override def position(untyped: UntypedType): Option[Position] =
      positionOf(untyped.typeSymbol)

    override def fromClassName(fullyQualifiedName: String): UntypedType =
      c.mirror.staticClass(fullyQualifiedName).toType

    override def fromClass(clazz: java.lang.Class[?]): UntypedType = {
      val staticClass = c.mirror.staticClass(clazz.getName)
      scala.util.Try(staticClass.toType).filter(_ != NoType).getOrElse {
        val typeSignature = staticClass.typeSignature
        if (typeSignature != NoType) typeSignature
        else {
          // $COVERAGE-OFF$
          hearthAssertionFailed(
            s"""Cannot find a type for Class ${clazz.getName}.""".stripMargin
          )
          // $COVERAGE-ON$
        }
      }
    }

    override def isInJavaLangPackage(instanceTpe: UntypedType): Boolean = {
      val sym = instanceTpe.typeSymbol
      sym != NoSymbol && sym.owner.isPackage && sym.owner.fullName == "java.lang"
    }

    override def isOpaqueType(instanceTpe: UntypedType): Boolean = false

    // Scala 2 has no opaque types, so there is never an underlying type to resolve.
    override def opaqueUnderlyingType(instanceTpe: UntypedType): Option[UntypedType] = None

    override def isTuple(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && {
        val fullName = A.fullName
        fullName.startsWith("scala.Tuple") && {
          val suffix = fullName.stripPrefix("scala.Tuple")
          suffix.nonEmpty && suffix.forall(_.isDigit) && {
            val n = suffix.toInt
            n >= 1 && n <= 22
          }
        }
      }
    }

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // We use =:= to check whether A is known to be exactly of the built-in type or is it some upper bound.
      // Also exclude enumeration Value types (they're not abstract)
      A != NoSymbol &&
      (isJavaEnum(instanceTpe) || (A.isAbstract && !Type.jvmBuiltInTypes
        .exists(tpe => instanceTpe =:= tpe.Underlying.asUntyped))) &&
      !isEnumeration(instanceTpe)
    }
    override def isFinal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && (A.isFinal || A.isModuleClass || isArray(instanceTpe))
    }
    override def isTrait(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && A.isClass && A.asClass.isTrait
    }

    override def isClass(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && (A.isClass && !isArray(instanceTpe))
    }

    override def isSealed(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && A.isClass && A.asClass.isSealed && !isJavaEnumValue(instanceTpe)
    }
    override def isJavaEnum(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A.isJavaEnum && !javaEnumRegexpFormat.matches(instanceTpe.toString)
    }
    override def isJavaEnumValue(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A.isJavaEnum && javaEnumRegexpFormat.matches(instanceTpe.toString)
    }
    override def isEnumeration(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      if (A == NoSymbol) false
      else {
        val enumClass = c.mirror.staticClass("scala.Enumeration")
        // Case (a): Object extending Enumeration (e.g. WeekDay.type)
        val isEnumerationObject = A.isModuleClass &&
          A.asClass.baseClasses.contains(enumClass)
        // Case (b): Value type member of an Enumeration object (e.g. WeekDay.Value)
        // Value's owner is scala.Enumeration (the class), not the specific enum object.
        // We check the type's prefix to see if it comes through an Enumeration-extending module.
        val isEnumerationValue = !isEnumerationObject && {
          instanceTpe match {
            case TypeRef(prefix, sym, _) if sym.name == TypeName("Value") =>
              prefix.typeSymbol != NoSymbol && prefix.typeSymbol.isModuleClass &&
              prefix.typeSymbol.asClass.baseClasses.contains(enumClass)
            case _ =>
              // Fallback: check if the type itself is or extends scala.Enumeration#Value
              // and is accessed through an Enumeration object
              A.isType && A.owner == enumClass && {
                instanceTpe match {
                  case TypeRef(prefix, _, _) =>
                    prefix.typeSymbol != NoSymbol && prefix.typeSymbol.isModuleClass &&
                    prefix.typeSymbol.asClass.baseClasses.contains(enumClass)
                  case _ => false
                }
              }
          }
        }
        isEnumerationObject || isEnumerationValue
      }
    }

    override def isCase(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // TODO: make it pass true for Scala 3 case val
      A != NoSymbol && A.isClass && A.asClass.isCaseClass
    }
    override def isObject(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && A.isClass && (A.asClass.isModule || A.asClass.isModuleClass || A.name.decodedName.toString
        .endsWith("$"))
    }
    override def isVal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      (isObject(instanceTpe) && A.isStatic && A.isFinal) || isJavaEnumValue(instanceTpe)
    }

    override def isAvailable(instanceTpe: UntypedType, scope: Accessible): Boolean =
      symbolAvailable(instanceTpe.typeSymbol, scope)

    override def parents(instanceTpe: UntypedType): List[UntypedType] =
      instanceTpe.parents

    override def baseClasses(instanceTpe: UntypedType): List[UntypedType] =
      instanceTpe.baseClasses.map(_.asType.toType)

    override def unsafeNewSubtype(
        targetType: UntypedType,
        parentTypes: List[UntypedType],
        constructor: Option[UntypedMethod],
        constructorArgs: List[List[UntypedExpr]],
        overrides: List[UntypedOverride]
    ): Either[NonEmptyVector[String], UntypedExpr] = {
      import c.universe.*

      val overrideDefs: List[Tree] = overrides.map { ovr =>
        val sym = ovr.method.symbol
        val methodName = TermName(ovr.method.name)
        val rawSig = sym.asMethod.typeSignatureIn(targetType)
        val paramLists = rawSig.paramLists

        // Reconstruct the method's type parameters (for generic / polymorphic methods like `def f[T](t: T): T`).
        // The parameter types and result type computed from `typeSignatureIn` still reference the ORIGINAL
        // type-parameter symbols, so we create matching TypeDefs and substitute old symbols -> new symbols, keeping
        // the override's signature in sync with the abstract method's polymorphic signature.
        val originalTypeParams: List[Symbol] = sym.asMethod.typeParams
        val typeParamDefs: List[TypeDef] = originalTypeParams.map(tp => c.internal.typeDef(tp))
        val newTypeParamSyms: List[Symbol] = typeParamDefs.map(_.symbol)

        def subst(tpe: c.universe.Type): c.universe.Type =
          if (originalTypeParams.isEmpty) tpe
          else tpe.substituteSymbols(originalTypeParams, newTypeParamSyms)

        val (paramDefs, paramRefs) = paramLists.map { paramList =>
          paramList.map { param =>
            val paramName = TermName(param.name.toString)
            val paramType = subst(param.typeSignature)
            val vd = ValDef(Modifiers(Flag.PARAM), paramName, TypeTree(paramType), EmptyTree)
            val ref: UntypedExpr = Ident(paramName)
            (vd, ref)
          }.unzip
        }.unzip

        val selfIdent: UntypedExpr = q"this"
        val bodyExpr = ovr.body(selfIdent, paramRefs.flatten)

        val resultType = subst(rawSig.finalResultType)
        DefDef(Modifiers(Flag.OVERRIDE), methodName, typeParamDefs, paramDefs, TypeTree(resultType), bodyExpr)
      }

      val classParentTypes = parentTypes.filterNot(pt => pt.typeSymbol.isClass && pt.typeSymbol.asClass.isTrait)
      val traitParentTypes = parentTypes.filter(pt => pt.typeSymbol.isClass && pt.typeSymbol.asClass.isTrait)

      val firstParent = classParentTypes.headOption.getOrElse(parentTypes.head)
      val mixinTraits = if (classParentTypes.nonEmpty) traitParentTypes else traitParentTypes.drop(1)

      val ctorArgTrees: List[Tree] = constructorArgs.flatten
      val mixinTrees: List[Tree] = mixinTraits.map(pt => tq"$pt")

      val result: Tree =
        if (mixinTrees.isEmpty)
          q"new $firstParent(..$ctorArgTrees) { ..$overrideDefs }"
        else
          q"new $firstParent(..$ctorArgTrees) with ..$mixinTrees { ..$overrideDefs }"

      Right(result)
    }

    // Cache for type comparison results, using identity-based lookup.
    // Type objects are typically reused for the same type within a macro expansion (e.g. lazy val Type.of[Int]),
    // so identity-based caching effectively deduplicates repeated comparisons across different provider/rule calls.
    private val subtypeCache =
      new java.util.IdentityHashMap[UntypedType, java.util.IdentityHashMap[UntypedType, java.lang.Boolean]]()
    private val sameTypeCache =
      new java.util.IdentityHashMap[UntypedType, java.util.IdentityHashMap[UntypedType, java.lang.Boolean]]()

    override def isSubtypeOf(subtype: UntypedType, supertype: UntypedType): Boolean = {
      // Fast path: reference equality means identical types
      if (subtype eq supertype) return true
      // Fast negative: if the dealiased subtype is a simple TypeRef (not a refinement, constant, or
      // existential type) and the supertype is final with a different symbol, they can't be in a subtype
      // relationship (except for bottom types Nothing/Null). We must dealias + match on TypeRef to avoid
      // false negatives for type aliases that expand to intersection types (e.g. `type Byte1 = 1 with Byte`).
      subtype.dealias match {
        case TypeRef(_, subSym, Nil) =>
          val supSym = supertype.typeSymbol
          if (
            subSym != supSym && supSym != NoSymbol &&
            (supSym.isFinal || supSym.isModuleClass) && supertype.typeArgs.isEmpty &&
            subSym != c.universe.definitions.NothingClass && subSym != c.universe.definitions.NullClass
          ) return false
        case _ =>
      }
      // Check cache before expensive compiler check
      var inner = subtypeCache.get(subtype)
      if (inner == null) {
        inner = new java.util.IdentityHashMap()
        subtypeCache.put(subtype, inner)
      }
      val cached = inner.get(supertype)
      if (cached != null) return cached.booleanValue()
      val result = subtype <:< supertype
      inner.put(supertype, java.lang.Boolean.valueOf(result))
      result
    }

    override def isSameAs(a: UntypedType, b: UntypedType): Boolean = {
      // Fast path: reference equality
      if (a eq b) return true
      // Dealias for fast comparison (dealias is cheap — just follows alias chain)
      val ad = a.dealias
      val bd = b.dealias
      if (ad eq bd) return true
      val adSym = ad.typeSymbol
      val bdSym = bd.typeSymbol
      // Fast negative: different non-NoSymbol dealiased symbols means definitely different types.
      // We do NOT use a fast positive here because same symbol doesn't guarantee same type
      // (e.g. singleton/literal types like `true` vs `Boolean`, or path-dependent types like
      // `WeekDay.Value` vs `Planet.Value`).
      if (adSym != bdSym && adSym != NoSymbol && bdSym != NoSymbol) return false
      // Check cache before expensive compiler check
      var inner = sameTypeCache.get(a)
      if (inner == null) {
        inner = new java.util.IdentityHashMap()
        sameTypeCache.put(a, inner)
      }
      val cached = inner.get(b)
      if (cached != null) return cached.booleanValue()
      val result = a =:= b
      inner.put(b, java.lang.Boolean.valueOf(result))
      result
    }

    override def companionObject(untyped: UntypedType): Option[(UntypedType, UntypedExpr)] =
      if (untyped.typeSymbol.isModuleClass) None
      else
        for {
          companion <- companionSymbol(untyped).toOption.flatMap(Option(_))
          if companion != NoSymbol && companion.isModule
          companionClass <- Option(companion.asModule.moduleClass)
          if companionClass != NoSymbol && companionClass.isType
        } yield (subtypeTypeOf(untyped, companionClass.asType), q"$companion")

    override def directChildren(instanceTpe: UntypedType): Option[ListMap[String, UntypedType]] = {
      val A = instanceTpe.typeSymbol

      if (isEnumeration(instanceTpe)) {
        // Determine if we have the object type or the Value type
        val enumObjectTypeOpt: Option[UntypedType] = if (A.isModuleClass) {
          // We have the object type directly
          Some(instanceTpe)
        } else {
          // We have the Value type - find the enum object from the type's prefix
          instanceTpe match {
            case TypeRef(prefix, _, _) if prefix.typeSymbol != NoSymbol && prefix.typeSymbol.isModuleClass =>
              Some(prefix)
            case _ =>
              // Fallback: try owner chain
              val owner = A.owner
              if (owner == NoSymbol || !owner.isModuleClass) None
              else Some(owner.asClass.toType)
          }
        }

        enumObjectTypeOpt.flatMap { enumObjectType =>
          // Get the Value type member from scala.Enumeration
          val enumClass = c.mirror.staticClass("scala.Enumeration")
          val valueClassSymbol = enumClass.toType.decls.find(s => s.isClass && s.name == TypeName("Value"))

          valueClassSymbol.flatMap { valueSym =>
            // Get enumeration values from the object's decls
            // Use baseClasses check instead of <:< because path-dependent types
            // (Enumeration.this.Value vs WeekDay.Value) make <:< unreliable.
            // Use .resultType to unwrap NullaryMethodType (Enumeration vals are implemented as methods).
            val children = enumObjectType.decls
              .filter(_.isTerm)
              .map(_.asTerm)
              .filter(t => t.isStable && !t.isPrivate)
              .filter { term =>
                val tpe = term.typeSignature.resultType
                tpe.baseClasses.contains(valueSym)
              }
              .toVector
              .sorted(symbolOrdering)
              .map { term =>
                // Create singleton type (e.g. WeekDay.Mon.type) for proper type representation
                symbolName(term) -> c.universe.internal.singleType(enumObjectType, term)
              }

            if (children.isEmpty) None
            else Some(ListMap.from(children))
          }
        }
      } else if (isJavaEnum(instanceTpe)) {
        Some(
          ListMap.from(
            instanceTpe.companion.decls
              .filter(_.isJavaEnum)
              .map(termSymbol => termSymbol.name.toString -> termSymbol.asTerm.typeSignature)
          )
        )
      } else if (isJavaEnumValue(instanceTpe)) {
        None
      } else if (isSealed(instanceTpe)) {
        forceTypeSymbolInitialization(A)

        def extractRecursively(t: TypeSymbol): Vector[TypeSymbol] =
          if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toVector.map(_.asType).flatMap(extractRecursively)
          else Vector(t)

        Some(
          ListMap.from(
            // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
            extractRecursively(A.asType).distinct
              .sorted(symbolOrdering)
              .map(subtypeSymbol => subtypeName(subtypeSymbol) -> subtypeTypeOf(instanceTpe, subtypeSymbol))
          )
        )
      } else None
    }

    override def dealias(untyped: UntypedType): UntypedType =
      untyped.dealias

    override def typeConstructor(untyped: UntypedType): UntypedType =
      untyped.dealias.typeConstructor

    override def typeArguments(untyped: UntypedType): List[UntypedType] =
      untyped.typeArgs

    override def applyTypeArgs(untyped: UntypedType, args: List[UntypedType]): UntypedType =
      appliedType(untyped.typeConstructor, args)

    override def sameTypeConstructorAs(a: UntypedType, b: UntypedType): Boolean =
      a.dealias.typeConstructor.typeSymbol == b.dealias.typeConstructor.typeSymbol

    override def annotations(untyped: UntypedType): List[UntypedExpr] =
      untyped.typeSymbol.annotations.map(ann => c.untypecheck(ann.tree))
    override def annotationTypes(untyped: UntypedType): List[UntypedType] =
      untyped.typeSymbol.annotations.map(_.tree.tpe)
  }
}
