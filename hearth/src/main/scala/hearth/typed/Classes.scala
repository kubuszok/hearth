package hearth
package typed

trait Classes { this: MacroCommons =>

  final class CaseClass[A](
      val tpe: Type[A],
      val primaryConstructor: Method.NoInstance[A]
  ) {

    lazy val otherConstructors: List[Method.NoInstance[A]] = tpe.constructors.filter(_ != primaryConstructor)

    lazy val caseFields: List[Existential[Method[A, *]]] =
      null.asInstanceOf[List[Existential[Method[A, *]]]] // TODO: priority 1
    lazy val methods: List[Existential[Method[A, *]]] =
      null.asInstanceOf[List[Existential[Method[A, *]]]] // TODO: priority 1

    override def toString: String = s"CaseClass(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: CaseClass[?] => tpe =:= that.tpe
      case _                  => false
    }

    override def hashCode: Int = tpe.hashCode
  }
  object CaseClass {

    def unapply[A](tpe: Type[A]): Option[CaseClass[A]] = ???
  }

  final class Enum[A](
      val tpe: Type[A]
  ) {
    def children: List[Type[A]] = ???

    override def toString: String = s"Enum(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: Enum[?] => tpe =:= that.tpe
      case _             => false
    }

    override def hashCode: Int = tpe.hashCode
  }
  object Enum {

    def unapply = ???
  }

  final class JavaBean[A](
      val tpe: Type[A]
  ) {
    def methods: List[Existential[Method[A, *]]] = ???
  }
  object JavaBean {

    def unapply = ???
  }
}
