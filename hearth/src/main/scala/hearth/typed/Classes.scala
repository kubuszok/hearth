package hearth
package typed

import hearth.fp.{Applicative, DirectStyle, Parallel}
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

    def construct[F[_]: DirectStyle: Applicative](makeArgument: CaseClass.ConstructField[F]): F[Option[Expr[A]]] =
      if (!primaryConstructor.isAvailable(Everywhere)) Option.empty[Expr[A]].pure[F]
      else {
        callConstructor(primaryConstructor.parameters.flatten.toList.traverse(buildFieldResults(makeArgument)))
      }
    def construct[F[_]: DirectStyle: Applicative](makeArgument: Parameter => F[Expr_??]): F[Option[Expr[A]]] =
      construct(CaseClass.ConstructField.apply[F](makeArgument))

    def parConstruct[F[_]: DirectStyle: Parallel](makeArgument: CaseClass.ConstructField[F]): F[Option[Expr[A]]] =
      if (!primaryConstructor.isAvailable(Everywhere)) Option.empty[Expr[A]].pure[F]
      else {
        callConstructor(primaryConstructor.parameters.flatten.toList.parTraverse(buildFieldResults(makeArgument)))
      }
    def parConstruct[F[_]: DirectStyle: Parallel](makeArgument: Parameter => F[Expr_??]): F[Option[Expr[A]]] =
      construct(CaseClass.ConstructField.apply[F](makeArgument))

    private def buildFieldResults[F[_]: DirectStyle](
        makeArgument: CaseClass.ConstructField[F]
    ): ((String, Parameter)) => F[(String, Expr_??)] = { case (name, parameter) =>
      DirectStyle[F].scoped { runSafe =>
        import parameter.tpe.Underlying
        name -> runSafe(makeArgument(parameter)).as_??
      }
    }

    private def callConstructor[F[_]: DirectStyle](fieldResults: F[List[(String, Expr_??)]]): F[Option[Expr[A]]] =
      DirectStyle[F].scoped { runSafe =>
        primaryConstructor(runSafe(fieldResults).toMap) match {
          case Right(value) => Some(value)
          case Left(error)  =>
            throw new AssertionError(s"Failed to call the primary constructor of ${tpe.prettyPrint}: $error")
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
        case method =>
          throw new AssertionError(
            s"Field ${method.name} of ${tpe.prettyPrint} is not nullary instance method: arity=${method.arity}, invocation=${method.asUntyped.invocation}"
          )
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
      if (tpe.isCase) tpe.primaryConstructor.map(new CaseClass(tpe, _))
      else None
    def parse[A: Type]: Option[CaseClass[A]] = unapply(Type[A])

    @FunctionalInterface
    trait ConstructField[F[_]] {
      def apply(field: Parameter): F[Expr[field.tpe.Underlying]]
    }
    object ConstructField {

      /** Constructs a [[ConstructField]] from a function that produces an [[Expr_??]] for a given field.
        *
        * Works around the limitation of Scala 2, that Single Abstract Methods cannot be constructed from what should be
        * dependant duntion types (introduced only in Scala 3).
        *
        * @since 0.1.0
        */
      def apply[F[_]: Applicative](makeArgument: Parameter => F[Expr_??]): ConstructField[F] =
        new CaseClass.ConstructField[F] {

          def apply(field: Parameter): F[Expr[field.tpe.Underlying]] = makeArgument(field).map { eexpr =>
            import eexpr.{Underlying as ExprType, value as expr}
            import field.tpe.Underlying as FieldType
            expr.upcast[FieldType]
          }
        }
    }
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

    def matchOn[F[_]: DirectStyle: Applicative, B: Type](
        value: Expr[A]
    )(handle: Expr_??<:[A] => F[Expr[B]]): F[Option[Expr[B]]] =
      directChildren.toList
        .traverse { case (name, child) =>
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

    def parMatchOn[F[_]: DirectStyle: Parallel, B: Type](
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
      else if (tpe.isEnumeration) tpe.directChildren.map(children => new Enum(tpe, children))
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

    lazy val beanGetters: List[Existential[Method.OfInstance[A, *]]] = methods
      .collect {
        case getter if getter.value.isJavaGetter => getter.value.asInstanceOf[Method[A, ?]]
      }
      .collect { case getter: Method.OfInstance[A, ?] @unchecked =>
        Existential[Method.OfInstance[A, *], getter.Returned](getter)(using getter.Returned)
      }
    lazy val beanSetters: List[Method.OfInstance[A, Unit]] = methods
      .collect {
        case setter if setter.value.isJavaSetter => setter.value.asInstanceOf[Method[A, Unit]]
      }
      .collect { case setter: Method.OfInstance[A, Unit] @unchecked =>
        setter
      }

    def constructWithoutSetters: Option[Expr[A]] =
      if (!defaultConstructor.isAvailable(Everywhere)) Option.empty[Expr[A]]
      else
        Some(
          defaultConstructor
            .apply(Map.empty)
            .getOrElse(throw new AssertionError(s"Failed to call the default constructor of ${tpe.prettyPrint}"))
            .upcast[A]
        )

    def constructWithSetters[F[_]: DirectStyle: Applicative](setField: JavaBean.SetField[F]): F[Option[Expr[A]]] =
      constructWithoutSetters.traverse[F, Expr[A]] { constructorExpr =>
        ValDefs
          .createVal(constructorExpr)
          .traverse { constructorResult =>
            beanSetters
              .traverse(applySetters(constructorResult, setField))
              .map(combineSetterResults(constructorResult))
          }
          .map(_.close)
      }
    def constructWithSetters[F[_]: DirectStyle: Applicative](
        setField: (String, Parameter) => F[Expr_??]
    ): F[Option[Expr[A]]] =
      constructWithSetters(JavaBean.SetField.apply[F](setField))

    def parConstructWithSetters[F[_]: DirectStyle: Parallel](setField: JavaBean.SetField[F]): F[Option[Expr[A]]] =
      constructWithoutSetters.traverse[F, Expr[A]] { constructorExpr =>
        ValDefs
          .createVal(constructorExpr)
          .parTraverse { constructorResult =>
            beanSetters
              .traverse(applySetters(constructorResult, setField))
              .map(combineSetterResults(constructorResult))
          }
          .map(_.close)
      }
    def parConstructWithSetters[F[_]: DirectStyle: Parallel](
        setField: (String, Parameter) => F[Expr_??]
    ): F[Option[Expr[A]]] =
      constructWithSetters(JavaBean.SetField.apply[F](setField))

    private def applySetters[F[_]: DirectStyle](constructorResult: Expr[A], setField: JavaBean.SetField[F])(
        setter: Method.OfInstance[A, Unit]
    ): F[Expr[Unit]] = {
      val (name, param) = setter.parameters.flatten.head
      import setter.Returned
      import param.tpe.Underlying
      DirectStyle[F].scoped { runSafe =>
        val value = runSafe(setField(name, param))
        setter.apply(constructorResult, Map(name -> value.as_??)) match {
          case Right(unit) => unit.upcast(using Returned, Type.of[Unit])
          case Left(error) =>
            throw new AssertionError(
              s"Failed to call the setter ${setter.name} of ${tpe.prettyPrint}: $error"
            )
        }
      }
    }

    private def combineSetterResults[A0: Type](constructorResult: Expr[A0])(setterResults: List[Expr[Unit]]): Expr[A0] =
      setterResults.toVector
        .foldRight(constructorResult) { (setterResult, acc) =>
          // TODO: On Scala 2, it looks ugly when printed, see ClassesJvmSpec - but it's harmless.
          Expr.quote {
            Expr.splice(setterResult)
            Expr.splice(acc)
          }
        }

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

    @FunctionalInterface
    trait SetField[F[_]] {
      def apply(name: String, input: Parameter): F[Expr[input.tpe.Underlying]]
    }
    object SetField {

      /** Constructs a [[SetField]] from a function that produces an [[Expr_??]] for a given field.
        *
        * Works around the limitation of Scala 2, that Single Abstract Methods cannot be constructed from what should be
        * dependant duntion types (introduced only in Scala 3).
        *
        * @since 0.1.0
        */
      def apply[F[_]: Applicative](makeArgument: (String, Parameter) => F[Expr_??]): SetField[F] =
        new JavaBean.SetField[F] {
          def apply(name: String, input: Parameter): F[Expr[input.tpe.Underlying]] = makeArgument(name, input).map {
            eexpr =>
              import eexpr.{Underlying as ExprType, value as expr}
              import input.tpe.Underlying as FieldType
              expr.upcast[FieldType]
          }
        }
    }
  }
}
