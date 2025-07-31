package hearth
package typed

import scala.collection.immutable.ListMap

trait Classes { this: MacroCommons =>

  final class CaseClass[A](
      val tpe: Type[A],
      val primaryConstructor: Method.NoInstance[A]
  ) {

    lazy val otherConstructors: List[Method.NoInstance[A]] = tpe.constructors.filter(_ != primaryConstructor)

    lazy val methods: List[Existential[Method[A, *]]] = tpe.methods
    lazy val caseFields: List[Existential[Method[A, *]]] = methods.filter(_.value.isCaseField)

    override def toString: String = s"CaseClass(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: CaseClass[?] => tpe =:= that.tpe
      case _                  => false
    }

    override def hashCode: Int = tpe.hashCode
  }
  object CaseClass {

    def unapply[A](tpe: Type[A]): Option[CaseClass[A]] =
      if (tpe.isCaseClass) tpe.primaryConstructor.map(new CaseClass(tpe, _))
      else None
    def parse[A: Type]: Option[CaseClass[A]] = unapply(Type[A])
  }

  final class Enum[A](
      val tpe: Type[A],
      val directChildren: ListMap[String, ??<:[A]]
  ) {

    lazy val exhaustiveChildren: Option[ListMap[String, ??<:[A]]] = tpe.exhaustiveChildren

    override def toString: String = s"Enum(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: Enum[?] => tpe =:= that.tpe
      case _             => false
    }

    override def hashCode: Int = tpe.hashCode
  }
  object Enum {

    def unapply[A](tpe: Type[A]): Option[Enum[A]] =
      if (tpe.isSealed) tpe.directChildren.map(children => new Enum(tpe, children))
      else None
    def parse[A: Type]: Option[Enum[A]] = unapply(Type[A])
  }

  final class JavaBean[A](
      val tpe: Type[A],
      val defaultConstructor: Method.NoInstance[A]
  ) {

    def methods: List[Existential[Method[A, *]]] = tpe.methods
  }
  object JavaBean {

    def unapply[A](tpe: Type[A]): Option[JavaBean[A]] =
      if (tpe.isJavaBean) tpe.defaultConstructor.map(new JavaBean(tpe, _))
      else None
    def parse[A: Type]: Option[JavaBean[A]] = unapply(Type[A])
  }
}
