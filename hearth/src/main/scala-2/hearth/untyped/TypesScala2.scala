package hearth
package untyped

import hearth.compat.*
import hearth.fp.ignore
import scala.collection.compat.*
import scala.collection.immutable.ListMap

trait TypesScala2 extends Types { this: MacroCommonsScala2 =>

  import c.universe.*
  import Type.platformSpecific.*

  final override type UntypedType = c.Type

  object UntypedType extends UntypedTypeModule {

    object platformSpecific {

      def subtypeName(typeSymbol: TypeSymbol): String = typeSymbol.name.toString

      /** Applies type arguments from supertype to subtype if there are any */
      def subtypeTypeOf(instanceTpe: UntypedType, subtypeSymbol: TypeSymbol): UntypedType = {
        val sEta = subtypeSymbol.toType.etaExpand

        sEta.finalResultType.substituteTypes(
          sEta.baseType(instanceTpe.typeSymbol).typeArgs.map(_.typeSymbol),
          instanceTpe.typeArgs
        )
      }
    }
    import platformSpecific.*

    override def fromTyped[A: Type]: UntypedType = c.weakTypeOf[A]
    override def toTyped[A](untyped: UntypedType): Type[A] = c.WeakTypeTag(untyped)

    override def isAbstract(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      // We use =:= to check whether A is known to be exactly of the build-in type or is it some upper bound.
      A != NoSymbol && A.isAbstract && !Type.builtInTypes.exists(tpe => instanceTpe =:= fromTyped(using tpe.Underlying))
    }
    override def isFinal(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && A.isFinal
    }

    override def isClass(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && A.isClass
    }

    override def isSealed(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A != NoSymbol && A.isClass && A.asClass.isSealed
    }
    override def isJavaEnum(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A.isJavaEnum && javaEnumRegexpFormat.matches(instanceTpe.toString)
    }
    override def isJavaEnumValue(instanceTpe: UntypedType): Boolean = {
      val A = instanceTpe.typeSymbol
      A.isJavaEnum && !javaEnumRegexpFormat.matches(instanceTpe.toString)
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
      } else if (isSealed(instanceTpe)) {
        forceTypeSymbolInitialization(A)

        def extractRecursively(t: TypeSymbol): Vector[TypeSymbol] =
          if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toVector.map(_.asType).flatMap(extractRecursively)
          else Vector(t)

        Some(
          ListMap.from(
            // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
            extractRecursively(A.asType).distinct
              .sortBy(_.pos)
              .map(subtypeSymbol => subtypeName(subtypeSymbol) -> subtypeTypeOf(instanceTpe, subtypeSymbol))
          )
        )
      } else None
    }
  }
}
