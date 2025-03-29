package hearth

trait TypesScala3 extends Types { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Type[A] = scala.quoted.Type[A]

  object Type extends TypeModule {

    override def simpleName[A: Type]: String =
      (TypeRepr.of[A].dealias.typeSymbol.name: String).replaceAll("\\$", "")
    override def prettyPrint[A: Type]: String = {
      // In Scala 3 typeRepr.dealias dealiases only the "main" type but not types applied as type parameters,
      // while in Scala 2 macros it dealiases everything - to keep the same behavior between them we need to
      // apply recursive dealiasing ourselves.
      def dealiasAll(tpe: TypeRepr): TypeRepr =
        tpe match {
          case AppliedType(tycon, args) => AppliedType(dealiasAll(tycon), args.map(dealiasAll(_)))
          case _                        => tpe.dealias
        }

      val repr = dealiasAll(TypeRepr.of[A])

      scala.util
        .Try {
          val symbolFullName = (repr.typeSymbol.fullName: String).replaceAll("\\$", "")
          val colorlessReprName = repr.show(using Printer.TypeReprCode)
          val colorfulReprName = repr.show(using Printer.TypeReprAnsiCode)

          // Classes defined inside a "class" or "def" have package.name.ClassName removed from the type,
          // so we have to prepend it ourselves to keep behavior consistent with Scala 2.
          if symbolFullName != colorlessReprName then {
            val pkg_Len = symbolFullName.length - colorlessReprName.length
            (0 to pkg_Len).takeWhile(i => symbolFullName.endsWith(("_" * i) + colorlessReprName)).lastOption match {
              case Some(_Len) => symbolFullName.substring(0, pkg_Len - _Len) + colorfulReprName
              case None       => colorfulReprName
            }
          } else colorfulReprName
        }
        .getOrElse(repr.toString)
    }

    override def isSubtypeOf[A: Type, B: Type]: Boolean = TypeRepr.of[A] <:< TypeRepr.of[B]
    override def isSameAs[A: Type, B: Type]: Boolean = TypeRepr.of[A] =:= TypeRepr.of[B]
  }
}
