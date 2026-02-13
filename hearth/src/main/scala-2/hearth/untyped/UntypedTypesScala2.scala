package hearth
package untyped

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

      def symbolAvailable(symbol: Symbol, scope: Accessible): Boolean = {
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

    override def fromClass(clazz: java.lang.Class[?]): UntypedType = {
      val staticClass = c.mirror.staticClass(clazz.getName)

      def typeSignature = Option(staticClass.typeSignature).filterNot(_ == NoType)
      def asType = scala.util.Try(staticClass.asType.toType).toOption.filterNot(_ == NoType)

      typeSignature.orElse(asType).getOrElse {
        // $COVERAGE-OFF$
        hearthAssertionFailed(
          s"""Cannot find a type signature for Class ${clazz.getName}.""".stripMargin
        )
        // $COVERAGE-ON$
      }
    }

    override def isOpaqueType(instanceTpe: UntypedType): Boolean = false

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
      A != NoSymbol &&
      (isJavaEnum(instanceTpe) || (A.isAbstract && !Type.jvmBuiltInTypes
        .exists(tpe => instanceTpe =:= tpe.Underlying.asUntyped)))
    }
    override def isFinal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && (A.isFinal || A.isModuleClass || isArray(instanceTpe))
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

    override def isSubtypeOf(subtype: UntypedType, supertype: UntypedType): Boolean = subtype <:< supertype
    override def isSameAs(a: UntypedType, b: UntypedType): Boolean = a =:= b

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

      if (isJavaEnum(instanceTpe)) {
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

    override def annotations(untyped: UntypedType): List[UntypedExpr] =
      untyped.typeSymbol.annotations.map { ann =>
        c.untypecheck(ann.tree) // TODO: check if this is necessary
      }
  }
}
