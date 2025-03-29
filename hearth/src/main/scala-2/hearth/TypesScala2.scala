package hearth

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
    }
    import platformSpecific.*

    override def simpleName[A: Type]: String = {
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

    override def isSubtypeOf[A: Type, B: Type]: Boolean = weakTypeOf[A] <:< weakTypeOf[B]
    override def isSameAs[A: Type, B: Type]: Boolean = weakTypeOf[A] =:= weakTypeOf[B]
  }
}
