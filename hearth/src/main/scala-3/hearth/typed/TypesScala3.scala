package hearth
package typed

import hearth.fp.Id
import scala.collection.immutable.ListMap

trait TypesScala3 extends Types { this: MacroCommonsScala3 =>

  import quotes.*, quotes.reflect.*

  final override type Type[A] = scala.quoted.Type[A]

  object Type extends TypeModule {

    object platformSpecific {

      final class LiteralCodec[U: Type](lift: U => Constant) extends TypeCodec[U] {
        final def toType[A <: U](value: A): Type[A] =
          ConstantType(lift(value)).asType.asInstanceOf[Type[A]]
        final def fromType[A](A: Type[A]): Option[Existential.UpperBounded[U, Id]] =
          if A <:< Type[U] then quoted.Type
            .valueOfConstant[U](using A.asInstanceOf[Type[U]])
            .map(Existential.UpperBounded[U, Id, U](_))
          else None
      }
    }
    import platformSpecific.*

    override def shortName[A: Type]: String =
      if Type[A].isVal then {
        // Type symbol approach for case val would upcast it to the enum type.
        val name = TypeRepr.of[A].dealias.show(using Printer.TypeReprCode)
        name.lastIndexOf('.') match {
          case -1  => name
          case idx => name.substring(idx + 1)
        }
      } else (TypeRepr.of[A].dealias.typeSymbol.name: String).replaceAll("\\$", "")

    override def plainPrint[A: Type]: String = removeAnsiColors(prettyPrint[A])
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
          val colorlessReprNameWithoutParams = colorlessReprName.takeWhile(_ != '[')
          val colorfulReprName = repr.show(using Printer.TypeReprAnsiCode)

          // Classes defined inside a "class" or "def" have package.name.ClassName removed from the type,
          // so we have to prepend it ourselves to keep behavior consistent with Scala 2.
          //
          // TODO: Currently we only add prefix to the outermost type, type parameters are not fixed this way.
          val result = if symbolFullName != colorlessReprNameWithoutParams then {
            val pkg_Len = symbolFullName.length - colorlessReprNameWithoutParams.length
            (0 to pkg_Len)
              .takeWhile(i => symbolFullName.endsWith(("_" * i) + colorlessReprNameWithoutParams))
              .lastOption match {
              case Some(_Len) => symbolFullName.substring(0, pkg_Len - _Len) + colorfulReprName
              case None       => colorfulReprName
            }
          } else colorfulReprName

          // FIXME: This is a quick workaround for missing .type, we have to fix it properly for nested types.
          // TermRef represents a singleton type (term reference) which should also get .type suffix.
          // We use termSymbol to detect TermRef-like types since TermRef.unapply can throw ClassCastException.
          val isSingletonOfTerm = repr.termSymbol != Symbol.noSymbol && !repr.termSymbol.isNoSymbol
          if (Type[A].isObject || Type[A].isVal || isSingletonOfTerm) && !result.contains(".type") then result + ".type"
          else result
        }
        .getOrElse(repr.toString)
    }

    override lazy val NullCodec: TypeCodec[Null] = LiteralCodec[Null](_ => NullConstant())
    override lazy val UnitCodec: TypeCodec[Unit] = LiteralCodec[Unit](_ => UnitConstant())
    override lazy val BooleanCodec: TypeCodec[Boolean] = LiteralCodec[Boolean](BooleanConstant(_))
    override lazy val ByteCodec: TypeCodec[Byte] = LiteralCodec[Byte](ByteConstant(_))
    override lazy val ShortCodec: TypeCodec[Short] = LiteralCodec[Short](ShortConstant(_))
    override lazy val IntCodec: TypeCodec[Int] = LiteralCodec[Int](IntConstant(_))
    override lazy val LongCodec: TypeCodec[Long] = LiteralCodec[Long](LongConstant(_))
    override lazy val FloatCodec: TypeCodec[Float] = LiteralCodec[Float](FloatConstant(_))
    override lazy val DoubleCodec: TypeCodec[Double] = LiteralCodec[Double](DoubleConstant(_))
    override lazy val CharCodec: TypeCodec[Char] = LiteralCodec[Char](CharConstant(_))
    override lazy val StringCodec: TypeCodec[String] = LiteralCodec[String](StringConstant(_))
  }
}
