package hearth
package typed

import hearth.fp.{DirectStyle, Parallel}
import hearth.fp.data.*
import hearth.fp.instances.*
import hearth.fp.syntax.*

import scala.collection.immutable.ListMap

trait Classes { this: MacroCommons =>

  /** Represents a class.
    *
    * It's a convenient utility around the [[Type]] to compute: methods, constructors, parameters, their types, etc.
    * only once.
    *
    * @since 0.1.0
    */
  class Class[A]()(implicit val tpe: Type[A]) {

    final lazy val constructors: List[Method.NoInstance[A]] = tpe.constructors
    final lazy val methods: List[Method.Of[A]] = tpe.methods
    final def method(name: String): List[Method.Of[A]] = methods.filter(_.value.name == name)

    final def asCaseClass: Option[CaseClass[A]] = CaseClass.unapply(tpe)
    final def asEnum: Option[Enum[A]] = Enum.unapply(tpe)
    final def asJavaBean: Option[JavaBean[A]] = JavaBean.unapply(tpe)

    override def equals(other: Any): Boolean = other match {
      case that: Class[?] => tpe =:= that.tpe
      case _              => false
    }

    override def hashCode: Int = tpe.hashCode
  }
  object Class {

    def apply[A: Type]: Class[A] = Type[A] match {
      case CaseClass(cc) => cc
      case Enum(e)       => e
      case JavaBean(jb)  => jb
      case _             => new Class()(Type[A])
    }
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
  ) extends Class[A]()(using tpe0) {

    lazy val nonPrimaryConstructors: List[Method.NoInstance[A]] = constructors.filter(_ != primaryConstructor)
    lazy val caseFields: List[Method.Of[A]] = methods.filter(_.value.isCaseField)

    trait ConstructField[F[_]] {
      def apply(field: Parameter): F[Expr[field.tpe.Underlying]]
    }

    def construct[F[_]: DirectStyle: Parallel](makeArgument: ConstructField[F]): F[Option[Expr[A]]] =
      if (!primaryConstructor.isAvailable(Everywhere)) Option.empty[Expr[A]].pure[F]
      else {
        val fieldResults = primaryConstructor.parameters.flatten.toList.parTraverse { case (name, parameter) =>
          DirectStyle[F].scoped { runSafe =>
            import parameter.tpe.Underlying
            name -> runSafe(makeArgument(parameter)).as_??
          }
        }
        DirectStyle[F].scoped { runSafe =>
          primaryConstructor(runSafe(fieldResults).toMap) match {
            case Right(value) => Some(value)
            case Left(error)  =>
              throw new AssertionError(s"Failed to call the primary constructor of ${tpe.prettyPrint}: $error")
          }
        }
      }

    def caseFieldValuesAt(instance: Expr[A]): ListMap[String, Expr_??] = ListMap.from(caseFields.map { field =>
      (field.value match {
        case method: Method.OfInstance[A, ?] if method.isNullary =>
          import method.Returned
          method(instance = instance, arguments = Map.empty) match {
            case Right(value) => method.name -> value.as_??
            case Left(error)  =>
              throw new AssertionError(
                s"Failed to get the value of the field ${field.value.name} of ${tpe.prettyPrint}: $error"
              )
          }
        case method => throw new AssertionError(s"Field ${method.name} is not nullary")
      }): @scala.annotation.nowarn
    })

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
  ) extends Class[A]()(using tpe0) {

    lazy val exhaustiveChildren: Option[NonEmptyMap[String, ??<:[A]]] = tpe.exhaustiveChildren

    def matchOn[F[_]: DirectStyle: Parallel, B: Type](
        value: Expr[A]
    )(handle: Expr_??<:[A] => F[Expr[B]]): F[Option[Expr[B]]] =
      directChildren.toList
        .parTraverse { case (name, child) =>
          DirectStyle[F].scoped { runSafe =>
            import child.Underlying as A0
            MatchCase.typeMatch[A0](name).map { matched =>
              runSafe(handle(matched.as_??<:[A]))
            }
          }
        }
        .map { list =>
          NonEmptyList.fromList(list).map { cases =>
            MatchCase.matchOn(value)(cases.toNonEmptyVector)
          }
        }

    override def toString: String = s"Enum(${Type.prettyPrint[A]})"

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
  ) extends Class[A]()(using tpe0) {

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
