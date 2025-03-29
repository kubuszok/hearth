package hearth

trait Types { this: MacroCommons =>

  type Type[A]

  val Type: TypeModule
  trait TypeModule { this: Type.type =>

    def simpleName[A](tpe: Type[A]): String
    def prettyPrint[A](tpe: Type[A]): String
  }

  implicit class TypeMethods[A](private val tpe: Type[A]) {
    def prettyPrint: String = Type.prettyPrint(tpe)
  }

  // TODO:
  type ?? = Any
}
