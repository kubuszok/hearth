package hearth

trait Types { this: MacroCommons =>

  /** Platform-specific type representation (`c.WeakTypeTag[A]` in 2, `scala.quoted.Type[A]` in 3) */
  type Type[A]

  val Type: TypeModule
  trait TypeModule { this: Type.type =>

    /** Summons `Type` instance */
    final def apply[A](implicit A: Type[A]): Type[A] = A

    def simpleName[A: Type]: String
    def prettyPrint[A: Type]: String

    def isSubtypeOf[A: Type, B: Type]: Boolean
    def isSameAs[A: Type, B: Type]: Boolean
  }

  implicit class TypeMethods[A](private val tpe: Type[A]) {

    def simpleName: String = Type.simpleName(using tpe)
    def prettyPrint: String = Type.prettyPrint(using tpe)

    def <:<[B](tpe2: Type[B]): Boolean = Type.isSubtypeOf(using tpe, tpe2)
    def =:=[B](tpe2: Type[B]): Boolean = Type.isSameAs(using tpe, tpe2)

    def as_?? : ?? = Existential[Type, A](tpe)(using tpe)
    def as_??>:[L <: A]: ??>:[L] = Existential.LowerBounded[L, Type, A](tpe)(using tpe)
    def as_??<:[U >: A]: ??<:[U] = Existential.UpperBounded[U, Type, A](tpe)(using tpe)
    def as_<:??<:[L <: A, U >: A]: L <:??<: U = Existential.Bounded[L, U, Type, A](tpe)(using tpe)
  }

  // Aliases to make the (very common) existential types shorter

  final type ?? = Existential[Type]
  final type ??>:[L] = Existential.LowerBounded[L, Type]
  final type ??<:[U] = Existential.UpperBounded[U, Type]
  final type <:??<:[L, U >: L] = Existential.Bounded[L, U, Type]

  implicit class BoundedExistentialTypeMethods[L, U >: L](private val tpe: L <:??<: U) {

    def simpleName: String = Type.simpleName(using tpe.Underlying)
    def prettyPrint: String = Type.prettyPrint(using tpe.Underlying)
  }
}
