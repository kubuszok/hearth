package hearth
package typed

import hearth.fp.Id

trait TypesScala2 extends Types { this: MacroCommonsScala2 =>

  import c.universe.*

  final override type Type[A] = c.WeakTypeTag[A]

  object Type extends TypeModule {

    object platformSpecific {

      // It is surprisingly ridiculous but I've found no other way of telling whether I am looking at enum abstract
      // class or its value, since EVERYTHING else looks the same: parent is not abstract, everyone is static,
      // everyone has the same baseClasses, everyone reports to have public primaryConstructor (which is <none>).
      // The only different in behavior is that one prints com.my.Enum and another com.my.Enum(MyValue).
      val javaEnumRegexpFormat = raw"^(.+)\((.+)\)$$".r

      // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
      // and <https://github.com/scalalandio/chimney/issues/562> and similar.
      def forceTypeSymbolInitialization[A: Type]: Unit = forceTypeSymbolInitialization(
        UntypedType.fromTyped[A].typeSymbol
      )
      def forceTypeSymbolInitialization(s: Symbol): Unit = s.typeSignature

      final class LiteralCodec[U: Type] extends TypeCodec[U] {
        def toType[A <: U](value: A): Type[A] =
          UntypedType.toTyped(c.universe.internal.constantType(Constant(value.asInstanceOf[AnyVal])))
        def fromType[A](A: Type[A]): Option[Existential.UpperBounded[U, Id]] =
          if (A <:< Type[U]) {
            scala.util
              .Try(
                A.tpe
                  .asInstanceOf[scala.reflect.internal.Types#ConstantType]
                  .value // Constant
                  .value // scala.Any
                  .asInstanceOf[U]
              )
              .toOption
              .map(Existential.UpperBounded[U, Id, U](_))
          } else None
      }
    }
    import platformSpecific.*

    override def shortName[A: Type]: String = {
      val tpe = Type[A].tpe
      tpe.toString match {
        case javaEnumRegexpFormat(_, valueName) if tpe.typeSymbol.isJavaEnum => valueName
        case _                                                               => tpe.dealias.typeSymbol.name.toString
      }
    }
    override def prettyPrint[A: Type]: String = {
      def helper(tpe: c.Type): String =
        tpe.toString match {
          case javaEnumRegexpFormat(enumName, valueName) if tpe.typeSymbol.isJavaEnum => s"$enumName.$valueName"
          case _ =>
            val tpes = tpe.typeArgs.map(helper)
            val tpeArgs = if (tpes.isEmpty) "" else s"[${tpes.mkString(", ")}]"
            tpe.dealias.typeSymbol.fullName + tpeArgs
        }

      Console.MAGENTA + helper(Type[A].tpe) + Console.RESET
    }

    override def annotations[A: Type]: List[Expr_??] = ??? // TODO

    override val primitiveTypes: List[??] = List(
      weakTypeOf[Boolean].as_??,
      weakTypeOf[Byte].as_??,
      weakTypeOf[Short].as_??,
      weakTypeOf[Int].as_??,
      weakTypeOf[Long].as_??,
      weakTypeOf[Float].as_??,
      weakTypeOf[Double].as_??,
      weakTypeOf[Char].as_??
    )
    override val buildInTypes: List[??] = primitiveTypes ++ List(
      weakTypeOf[String].as_??,
      weakTypeOf[Unit].as_??
    )

    override val BooleanCodec: TypeCodec[Boolean] = new LiteralCodec[Boolean]
    override val ByteCodec: TypeCodec[Byte] = new LiteralCodec[Byte]
    override val ShortCodec: TypeCodec[Short] = new LiteralCodec[Short]
    override val IntCodec: TypeCodec[Int] = new LiteralCodec[Int]
    override val LongCodec: TypeCodec[Long] = new LiteralCodec[Long]
    override val FloatCodec: TypeCodec[Float] = new LiteralCodec[Float]
    override val DoubleCodec: TypeCodec[Double] = new LiteralCodec[Double]
    override val CharCodec: TypeCodec[Char] = new LiteralCodec[Char]
    override val StringCodec: TypeCodec[String] = new LiteralCodec[String]
    override val UnitCodec: TypeCodec[Unit] = new LiteralCodec[Unit]
    override val NullCodec: TypeCodec[Null] = new LiteralCodec[Null]
  }
}
