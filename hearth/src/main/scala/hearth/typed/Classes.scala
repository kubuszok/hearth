package hearth
package typed

import scala.collection.immutable.ListMap

trait Classes { this: MacroCommons =>

  /** Represents a class.
    *
    * It's a convenient utility around the [[Type]] to compute: methods, constructors, parameters, their types, etc.
    * only once.
    *
    * @since 0.1.0
    */
  class Class[A](val tpe: Type[A]) {

    lazy val constructors: List[Method.NoInstance[A]] = tpe.constructors
    lazy val methods: List[Method.Of[A]] = tpe.methods

    def asCaseClass: Option[CaseClass[A]] = CaseClass.unapply(tpe)
    def asEnum: Option[Enum[A]] = Enum.unapply(tpe)
    def asJavaBean: Option[JavaBean[A]] = JavaBean.unapply(tpe)

    override def equals(other: Any): Boolean = other match {
      case that: Class[?] => tpe =:= that.tpe
      case _              => false
    }

    override def hashCode: Int = tpe.hashCode
  }
  object Class {

    def apply[A: Type]: Class[A] = new Class(Type[A])
  }

  /** Represents a case class.
    *
    * It's a specialization of a [[Class]] that's aware, that some of its methods are case fields.
    *
    * @since 0.1.0
    */
  final class CaseClass[A] private (
      tpe0: Type[A],
      val primaryConstructor: Method.NoInstance[A]
  ) extends Class[A](tpe0) {

    lazy val nonPrimaryConstructors: List[Method.NoInstance[A]] = constructors.filter(_ != primaryConstructor)
    lazy val caseFields: List[Method.Of[A]] = methods.filter(_.value.isCaseField)

    override def toString: String = s"CaseClass(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: CaseClass[?] => tpe =:= that.tpe
      case _                  => false
    }
  }
  object CaseClass {

    def unapply[A](tpe: Type[A]): Option[CaseClass[A]] =
      if (tpe.isCaseClass) tpe.primaryConstructor.map(new CaseClass(tpe, _))
      else None
    def parse[A: Type]: Option[CaseClass[A]] = unapply(Type[A])
  }

  /** Represents a sealed trait, Scala 3's enum or Java's enum.
    *
    * It's a specialization of a [[Class]] that's aware, that there is a known set of children subtypes.
    *
    * @since 0.1.0
    */
  final class Enum[A] private (
      tpe0: Type[A],
      val directChildren: ListMap[String, ??<:[A]]
  ) extends Class[A](tpe0) {

    lazy val exhaustiveChildren: Option[ListMap[String, ??<:[A]]] = tpe.exhaustiveChildren

    override def toString: String = s"Enum(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: Enum[?] => tpe =:= that.tpe
      case _             => false
    }
  }
  object Enum {

    def unapply[A](tpe: Type[A]): Option[Enum[A]] =
      if (tpe.isSealed) tpe.directChildren.map(children => new Enum(tpe, children))
      else None
    def parse[A: Type]: Option[Enum[A]] = unapply(Type[A])
  }

  /** Represents a Java bean.
    *
    * It's a specialization of a [[Class]] that's aware, that there is a default constructor and it's methods should
    * have Java Bean getters and setters.
    *
    * @since 0.1.0
    */
  final class JavaBean[A] private (
      tpe0: Type[A],
      val defaultConstructor: Method.NoInstance[A]
  ) extends Class[A](tpe0) {

    override def toString: String = s"JavaBean(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: JavaBean[?] => tpe =:= that.tpe
      case _                 => false
    }
  }
  object JavaBean {

    def unapply[A](tpe: Type[A]): Option[JavaBean[A]] =
      if (tpe.isJavaBean) tpe.defaultConstructor.map(new JavaBean(tpe, _))
      else None
    def parse[A: Type]: Option[JavaBean[A]] = unapply(Type[A])
  }
}
