package hearth
package typed

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

      // TODO: check when this crap has to be called :/

      // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
      // and <https://github.com/scalalandio/chimney/issues/562> and similar.
      def forceTypeSymbolInitialization[A: Type]: Unit = forceTypeSymbolInitialization(
        UntypedType.fromTyped[A].typeSymbol
      )
      def forceTypeSymbolInitialization(s: Symbol): Unit = s.typeSignature

      abstract class LiteralImpl[U: Type] extends Literal[U] {
        def apply[A <: U](value: A): Type[A] =
          UntypedType.toTyped(c.universe.internal.constantType(Constant(value.asInstanceOf[AnyVal])))
        def unapply[A](A: Type[A]): Option[Existential.UpperBounded[U, Id]] =
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

    override def isAbstract[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A != NoSymbol && A.isAbstract
    }
    override def isFinal[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A != NoSymbol && A.isFinal
    }

    override def isSealed[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A != NoSymbol && A.isClass && A.asClass.isSealed
    }
    override def isJavaEnum[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A.isJavaEnum && javaEnumRegexpFormat.pattern
        .matcher(UntypedType.fromTyped[A].toString)
        .matches() // 2.12 doesn't have .matches
    }
    override def isJavaEnumValue[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A.isJavaEnum && !javaEnumRegexpFormat.pattern
        .matcher(UntypedType.fromTyped[A].toString)
        .matches() // 2.12 doesn't have .matches
    }

    override def isCase[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A != NoSymbol && A.isClass && A.asClass.isCaseClass
    }
    override def isObject[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A != NoSymbol && A.isClass && A.asClass.isModuleClass
    }
    override def isVal[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      isObject[A] && A.isStatic && A.isFinal // ???
    }

    override def isClass[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A != NoSymbol && A.isClass
    }

    override def isPublic[A: Type]: Boolean = {
      val A = UntypedType.fromTyped[A].typeSymbol
      A != NoSymbol && A.isPublic
    }
    override def isAvailableHere[A: Type]: Boolean = ??? // TODO

    override def isSubtypeOf[A: Type, B: Type]: Boolean = weakTypeOf[A] <:< weakTypeOf[B]
    override def isSameAs[A: Type, B: Type]: Boolean = weakTypeOf[A] =:= weakTypeOf[B]

    object BooleanLiteral extends LiteralImpl[Boolean] with BooleanLiteralModule
    object IntLiteral extends LiteralImpl[Int] with IntLiteralModule
    object LongLiteral extends LiteralImpl[Long] with LongLiteralModule
    object FloatLiteral extends LiteralImpl[Float] with FloatLiteralModule
    object DoubleLiteral extends LiteralImpl[Double] with DoubleLiteralModule
    object CharLiteral extends LiteralImpl[Char] with CharLiteralModule
    object StringLiteral extends LiteralImpl[String] with StringLiteralModule
  }
}
