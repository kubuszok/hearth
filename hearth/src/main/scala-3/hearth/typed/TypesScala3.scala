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
          val colorlessReprNameWithoutParams = colorlessReprName.takeWhile(_ != '[')
          val colorfulReprName = repr.show(using Printer.TypeReprAnsiCode)

          // Classes defined inside a "class" or "def" have package.name.ClassName removed from the type,
          // so we have to prepend it ourselves to keep behavior consistent with Scala 2.
          //
          // TODO: We only add prefix to the outermost type, type parameters are not fixed this way.
          if symbolFullName != colorlessReprNameWithoutParams then {
            val pkg_Len = symbolFullName.length - colorlessReprNameWithoutParams.length
            (0 to pkg_Len)
              .takeWhile(i => symbolFullName.endsWith(("_" * i) + colorlessReprNameWithoutParams))
              .lastOption match {
              case Some(_Len) => symbolFullName.substring(0, pkg_Len - _Len) + colorfulReprName
              case None       => colorfulReprName
            }
          } else colorfulReprName
        }
        .getOrElse(repr.toString)
    }

    override def annotations[A: Type]: List[Expr_??] = ??? // TODO

    override val primitiveTypes: List[??] = List(
      scala.quoted.Type.of[Boolean].as_??,
      scala.quoted.Type.of[Byte].as_??,
      scala.quoted.Type.of[Short].as_??,
      scala.quoted.Type.of[Int].as_??,
      scala.quoted.Type.of[Long].as_??,
      scala.quoted.Type.of[Float].as_??,
      scala.quoted.Type.of[Double].as_??,
      scala.quoted.Type.of[Char].as_??,
      scala.quoted.Type.of[Unit].as_??
    )
    override val buildInTypes: List[??] = primitiveTypes ++ List(
      scala.quoted.Type.of[String].as_??
    )

    override def isAbstract[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      // We use =:= to check whether A is known to be exactly of the build-in type or is it some upper bound.
      !A.isNoSymbol && (A.flags.is(Flags.Abstract) || A.flags.is(Flags.Trait)) &&
      !buildInTypes.exists(tpe => Type[A] =:= tpe.Underlying)
    }
    override def isFinal[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Final)
    }

    override def isClass[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      !A.isNoSymbol && A.isClassDef
    }

    override def isSealed[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Sealed)
    }
    override def isJavaEnum[A: Type]: Boolean =
      Type[A] <:< scala.quoted.Type.of[java.lang.Enum[?]] && isAbstract[A]
    override def isJavaEnumValue[A: Type]: Boolean =
      Type[A] <:< scala.quoted.Type.of[java.lang.Enum[?]] && !isAbstract[A]
    override def isCase[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Case)
    }
    override def isObject[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      !A.isNoSymbol && A.flags.is(Flags.Module)
    }
    override def isVal[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      def attempt(sym: Symbol): Boolean =
        A.flags.is(Flags.Enum) && (A.flags.is(Flags.JavaStatic) || A.flags.is(Flags.StableRealizable))
      !A.isNoSymbol && attempt(UntypedType.fromTyped[A].typeSymbol) || attempt(UntypedType.fromTyped[A].termSymbol)
    }

    override def isPublic[A: Type]: Boolean = {
      val A = TypeRepr.of[A].typeSymbol
      !(A.flags.is(Flags.Private) || A.flags.is(Flags.PrivateLocal) || A.flags.is(Flags.Protected) ||
        A.privateWithin.isDefined || A.protectedWithin.isDefined)
    }
    override def isAvailableHere[A: Type]: Boolean =
      try {
        // Try to access the type in the current context.
        // If it's not accessible, this will throw an exception.
        // TODO: test this assumption
        val _ = '{ null.asInstanceOf[A] }
        true
      } catch {
        case _: Throwable => false
      }

    override def isSubtypeOf[A: Type, B: Type]: Boolean = TypeRepr.of[A] <:< TypeRepr.of[B]
    override def isSameAs[A: Type, B: Type]: Boolean = TypeRepr.of[A] =:= TypeRepr.of[B]

    override val BooleanCodec: TypeCodec[Boolean] = LiteralCodec[Boolean](BooleanConstant(_))
    override val IntCodec: TypeCodec[Int] = LiteralCodec[Int](IntConstant(_))
    override val LongCodec: TypeCodec[Long] = LiteralCodec[Long](LongConstant(_))
    override val FloatCodec: TypeCodec[Float] = LiteralCodec[Float](FloatConstant(_))
    override val DoubleCodec: TypeCodec[Double] = LiteralCodec[Double](DoubleConstant(_))
    override val CharCodec: TypeCodec[Char] = LiteralCodec[Char](CharConstant(_))
    override val StringCodec: TypeCodec[String] = LiteralCodec[String](StringConstant(_))
  }
}
