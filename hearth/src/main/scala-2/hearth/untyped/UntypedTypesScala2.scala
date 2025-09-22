package hearth
package untyped

import hearth.fp.ignore
import scala.collection.immutable.ListMap

trait UntypedTypesScala2 extends UntypedTypes { this: MacroCommonsScala2 =>

  import c.universe.*
  import Type.platformSpecific.*

  final override type UntypedType = c.Type

  object UntypedType extends UntypedTypeModule {

    object platformSpecific {

      // Borrowed from jsoniter-scala: https://github.com/plokhotnyuk/jsoniter-scala/blob/b14dbe51d3ae6752e5a9f90f1f3caf5bceb5e4b0/jsoniter-scala-macros/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L462
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
          // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
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

    override def fromClass(clazz: java.lang.Class[?]): UntypedType = c.mirror.staticClass(clazz.getName).typeSignature

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // We use =:= to check whether A is known to be exactly of the build-in type or is it some upper bound.
      A != NoSymbol &&
      (isJavaEnum(instanceTpe) || (A.isAbstract && !Type.builtInTypes
        .exists(tpe => instanceTpe =:= fromTyped(using tpe.Underlying))))
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
      A != NoSymbol && A.isClass && A.asClass.isModuleClass
    }
    override def isVal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      isObject(instanceTpe) && A.isStatic && A.isFinal // ???
    }

    override def isAvailable(instanceTpe: UntypedType, scope: Accessible): Boolean =
      scope match {
        case Everywhere =>
          val A = instanceTpe.typeSymbol
          A != NoSymbol && A.isPublic
        case AtCallSite =>
          try {
            // Try to access the type in the current context.
            // If it's not accessible, this will throw an exception.
            // TODO: test this assumption
            ignore(c.typecheck(q"null.asInstanceOf[$instanceTpe]", silent = true))
            true
          } catch {
            case _: Throwable => false
          }
      }

    override def isSubtypeOf(subtype: UntypedType, supertype: UntypedType): Boolean = subtype <:< supertype
    override def isSameAs(a: UntypedType, b: UntypedType): Boolean = a =:= b

    override def companionObject(untyped: UntypedType): Option[(UntypedType, UntypedExpr)] =
      if (untyped.typeSymbol.isModuleClass) None
      else
        companionSymbol(untyped).toOption.map(companion => (subtypeTypeOf(untyped, companion.asType), q"$companion"))

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

        try
          Some(
            ListMap.from(
              // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
              extractRecursively(A.asType).distinct
                .sorted(symbolOrdering)
                .map(subtypeSymbol => subtypeName(subtypeSymbol) -> subtypeTypeOf(instanceTpe, subtypeSymbol))
            )
          )
        catch {
          case err: Throwable =>
            println(s"""Unexpected error:
                       |error: ${err.getMessage}
                       |tpe:   ${instanceTpe.prettyPrint}
                       |toString:        ${instanceTpe.toString}
                       |isJavaEnum:      ${isJavaEnum(instanceTpe)}
                       |isJavaEnumValue: ${isJavaEnumValue(instanceTpe)}
                       |""".stripMargin)
            None
        }
      } else None
    }

    override def annotations(untyped: UntypedType): List[UntypedExpr] =
      untyped.typeSymbol.annotations.map { ann =>
        c.untypecheck(ann.tree) // TODO: check if this is necessary
      }
  }
}
