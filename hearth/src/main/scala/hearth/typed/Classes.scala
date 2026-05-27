package hearth
package typed

import hearth.fp.{Applicative, DirectStyle, Parallel}
import hearth.fp.data.*
import hearth.fp.instances.*
import hearth.fp.syntax.*

import scala.collection.immutable.ListMap

trait Classes { this: MacroCommons =>

  private def applyAndBuild(method: Method, arguments: Arguments): Either[String, Expr_??] =
    method match {
      case av: Method.ApplyValues =>
        av.apply(arguments) match {
          case r: Method.Result[?] =>
            import r.Returned
            r.build().map(_.as_??)
          case other => Left(s"Unexpected step after ApplyValues: ${other.getClass.getSimpleName}")
        }
      case r: Method.Result[?] =>
        import r.Returned
        r.build().map(_.as_??)
      case other => Left(s"Expected ApplyValues or Result, got ${other.getClass.getSimpleName}")
    }

  /** Represents a class.
    *
    * It's a convenient utility around the [[Type]] to compute: methods, constructors, parameters, their types, etc.
    * only once.
    *
    * @since 0.1.0
    */
  class Class[A]()(implicit val tpe: Type[A]) {

    final lazy val constructors: List[Method] = tpe.constructors
    final lazy val methods: List[Method] = tpe.methods
    final def method(name: String): List[Method] = methods.filter(_.name == name)

    final def asSingleton: Option[SingletonValue[A]] = SingletonValue.unapply(tpe)
    final def asNamedTuple: Option[NamedTuple[A]] = NamedTuple.unapply(tpe)
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
      case SingletonValue(s) => s
      case NamedTuple(nt)    => nt
      case CaseClass(cc)     => cc
      case Enum(e)           => e
      case JavaBean(jb)      => jb
      case _                 => new Class()(Type[A])
    }

    /** Parses a type as a [[Class]].
      *
      * Always returns [[ClassViewResult.Compatible]] since [[Class]] is the most general view.
      *
      * @since 0.3.0
      */
    def parse[A: Type]: ClassViewResult[Class[A]] = ClassViewResult.Compatible(apply[A])
  }

  /** Result of parsing a type into a class view.
    *
    * @since 0.3.0
    */
  sealed trait ClassViewResult[+V] extends Product with Serializable {
    def toOption: Option[V]
    def toEither: Either[String, V]
  }
  object ClassViewResult {

    /** The type is compatible with the requested class view.
      *
      * @since 0.3.0
      */
    final case class Compatible[V](value: V) extends ClassViewResult[V] {
      def toOption: Option[V] = Some(value)
      def toEither: Either[String, V] = Right(value)
    }

    /** The type is incompatible with the requested class view.
      *
      * @since 0.3.0
      */
    final case class Incompatible(reason: String) extends ClassViewResult[Nothing] {
      def toOption: Option[Nothing] = None
      def toEither: Either[String, Nothing] = Left(reason)
    }
  }

  /** Represents a singleton value: case objects, parameterless Scala 3 enum cases, normal objects, Java enum values, or
    * Scala Enumeration values.
    *
    * @since 0.3.0
    */
  final class SingletonValue[A] private (
      tpe0: Type[A],
      val singletonExpr: Expr[A]
  ) extends Class[A]()(using tpe0) {

    override def toString: String = s"SingletonValue(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: SingletonValue[?] => tpe =:= that.tpe
      case _                       => false
    }
  }
  object SingletonValue {

    def unapply[A](tpe: Type[A]): Option[SingletonValue[A]] =
      Expr.singletonOf[A](using tpe).map(new SingletonValue(tpe, _))

    /** Parses a type as a [[SingletonValue]].
      *
      * @since 0.3.0
      */
    def parse[A: Type]: ClassViewResult[SingletonValue[A]] =
      unapply(Type[A]) match {
        case Some(s) => ClassViewResult.Compatible(s)
        case None    => ClassViewResult.Incompatible(s"${Type.prettyPrint[A]} is not a singleton type")
      }
  }

  /** Represents a named tuple (Scala 3.7+ only).
    *
    * It's a specialization of a [[Class]] that's aware that the type is a named tuple, providing access to its fields
    * and a way to construct instances.
    *
    * @since 0.3.0
    */
  final class NamedTuple[A] private (
      tpe0: Type[A],
      private val primaryConstructor0: Method
  ) extends Class[A]()(using tpe0) {

    val primaryConstructor: Method = primaryConstructor0

    lazy val fields: List[(String, ??)] = primaryConstructor.totalParameters.flatten.toList.map { case (name, param) =>
      name -> param.tpe
    }

    def construct[F[_]: DirectStyle: Applicative](
        makeArgument: CaseClass.ConstructField[F],
        visibility: Accessible = Everywhere
    ): F[Option[Expr[A]]] =
      if (!primaryConstructor.isAvailable(visibility)) Option.empty[Expr[A]].pure[F]
      else
        DirectStyle[F].scoped { runSafe =>
          val fieldResults = runSafe(
            primaryConstructor.totalParameters.flatten.toList.traverse { case (name, parameter) =>
              DirectStyle[F].scoped { runSafe2 =>
                import parameter.tpe.Underlying
                name -> runSafe2(makeArgument(parameter)).as_??
              }
            }
          )
          applyAndBuild(primaryConstructor, fieldResults.toMap) match {
            case Right(value) =>
              import value.Underlying
              Some(value.value.upcast[A])
            case Left(error) =>
              throw new AssertionError(s"Failed to call the primary constructor of ${tpe.prettyPrint}: $error")
          }
        }

    override def toString: String = s"NamedTuple(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: NamedTuple[?] => tpe =:= that.tpe
      case _                   => false
    }
  }
  object NamedTuple {

    def unapply[A](tpe: Type[A]): Option[NamedTuple[A]] =
      if (tpe.isNamedTuple) tpe.primaryConstructor.map(new NamedTuple(tpe, _))
      else None

    /** Parses a type as a [[NamedTuple]].
      *
      * @since 0.3.0
      */
    def parse[A: Type]: ClassViewResult[NamedTuple[A]] =
      if (!Type.isNamedTuple[A])
        ClassViewResult.Incompatible(s"${Type.prettyPrint[A]} is not a named tuple")
      else
        Type[A].primaryConstructor match {
          case Some(ctor) => ClassViewResult.Compatible(new NamedTuple(Type[A], ctor))
          case None       =>
            ClassViewResult.Incompatible(s"${Type.prettyPrint[A]} is a named tuple but has no primary constructor")
        }
  }

  /** Represents a case class.
    *
    * It's a specialization of a [[Class]] that's aware, that some of its methods are case fields.
    *
    * Singletons (case objects, parameterless enum cases) are no longer handled by this class — use [[SingletonValue]]
    * instead.
    *
    * @since 0.1.0
    */
  final class CaseClass[A] private (
      tpe0: Type[A],
      private val primaryConstructor0: Method
  ) extends Class[A]()(using tpe0) {

    /** The primary constructor.
      *
      * @since 0.1.0
      */
    val primaryConstructor: Method = primaryConstructor0

    lazy val nonPrimaryConstructors: List[Method] =
      constructors.filter(_ != primaryConstructor)
    lazy val caseFields: List[Method] = methods.filter(_.isCaseField)

    def construct[F[_]: DirectStyle: Applicative](
        makeArgument: CaseClass.ConstructField[F],
        visibility: Accessible = Everywhere
    ): F[Option[Expr[A]]] =
      if (!primaryConstructor.isAvailable(visibility)) Option.empty[Expr[A]].pure[F]
      else
        callConstructor(primaryConstructor)(
          primaryConstructor.totalParameters.flatten.toList.traverse(buildFieldResults(makeArgument))
        )
    def construct[F[_]: DirectStyle: Applicative](makeArgument: Parameter => F[Expr_??]): F[Option[Expr[A]]] =
      construct(CaseClass.ConstructField.apply[F](makeArgument))

    def parConstruct[F[_]: DirectStyle: Parallel](
        makeArgument: CaseClass.ConstructField[F],
        visibility: Accessible = Everywhere
    ): F[Option[Expr[A]]] =
      if (!primaryConstructor.isAvailable(visibility)) Option.empty[Expr[A]].pure[F]
      else
        callConstructor(primaryConstructor)(
          primaryConstructor.totalParameters.flatten.toList.parTraverse(buildFieldResults(makeArgument))
        )
    def parConstruct[F[_]: DirectStyle: Parallel](makeArgument: Parameter => F[Expr_??]): F[Option[Expr[A]]] =
      parConstruct(CaseClass.ConstructField.apply[F](makeArgument))

    private def buildFieldResults[F[_]: DirectStyle](
        makeArgument: CaseClass.ConstructField[F]
    ): ((String, Parameter)) => F[(String, Expr_??)] = { case (name, parameter) =>
      DirectStyle[F].scoped { runSafe =>
        import parameter.tpe.Underlying
        name -> runSafe(makeArgument(parameter)).as_??
      }
    }

    private def callConstructor[F[_]: DirectStyle](
        ctor: Method
    )(fieldResults: F[List[(String, Expr_??)]]): F[Option[Expr[A]]] =
      DirectStyle[F].scoped { runSafe =>
        applyAndBuild(ctor, runSafe(fieldResults).toMap) match {
          case Right(value) =>
            import value.Underlying
            Some(value.value.upcast[A])
          case Left(error) =>
            throw new AssertionError(s"Failed to call the primary constructor of ${tpe.prettyPrint}: $error")
        }
      }

    def caseFieldValuesAt(
        instance: Expr[A],
        visibility: Accessible = Everywhere
    ): ListMap[String, Expr_??] = ListMap.from(caseFields.filter(_.isAvailable(visibility)).map { field =>
      (field match {
        case oi: Method.OnInstance if field.isNullary =>
          val afterInstance = oi.apply(instance.asInstanceOf[Expr[oi.Instance]])
          afterInstance match {
            case r: Method.Result[?] =>
              import r.Returned
              r.build() match {
                case Right(value) => field.name -> value.as_??
                case Left(error)  =>
                  throw new AssertionError(
                    s"Failed to get the value of the field ${field.name} of ${tpe.prettyPrint}: $error"
                  )
              }
            case other =>
              throw new AssertionError(
                s"Unexpected step after OnInstance for field ${field.name} of ${tpe.prettyPrint}: ${other.getClass.getSimpleName}"
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
      if (tpe.isCaseClass) tpe.primaryConstructor.map(new CaseClass(tpe, _))
      else None

    /** Parses a type as a [[CaseClass]].
      *
      * @since 0.3.0
      */
    def parse[A: Type]: ClassViewResult[CaseClass[A]] =
      unapply(Type[A]) match {
        case Some(cc) => ClassViewResult.Compatible(cc)
        case None     =>
          if (Type.isCaseObject[A] || Type.isCaseVal[A])
            ClassViewResult.Incompatible(
              s"${Type.prettyPrint[A]} is a singleton, use SingletonValue instead of CaseClass"
            )
          else if (!Type.isCase[A])
            ClassViewResult.Incompatible(s"${Type.prettyPrint[A]} is not a case class")
          else
            ClassViewResult.Incompatible(s"${Type.prettyPrint[A]} is a case type but has no primary constructor")
      }

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

  /** Represents a sealed trait, Scala 3's enum, Java's enum, or a disjoint union type (Scala 3 only).
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
            // Use eqValue for enum vals with erased types (Scala 3 parameterless enum cases, Java enum vals)
            // but NOT for case objects where typeMatch preserves exhaustivity checking
            val mc: MatchCase[Expr[A0]] =
              if (Type.isVal[A0] && !Type.isObject[A0])
                Expr.singletonOf[A0] match {
                  // $COVERAGE-OFF$ singletonOf returns Some only for Scala 3 enum vals, not testable on Scala 2
                  case Some(singleton) => MatchCase.eqValue[A0](singleton, name)
                  // $COVERAGE-ON$
                  case None => MatchCase.typeMatch[A0](name)
                }
              else MatchCase.typeMatch[A0](name)
            mc.map { matched =>
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
            // Use eqValue for enum vals with erased types (Scala 3 parameterless enum cases, Java enum vals)
            // but NOT for case objects where typeMatch preserves exhaustivity checking
            val mc: MatchCase[Expr[A0]] =
              if (Type.isVal[A0] && !Type.isObject[A0])
                Expr.singletonOf[A0] match {
                  // $COVERAGE-OFF$ singletonOf returns Some only for Scala 3 enum vals, not testable on Scala 2
                  case Some(singleton) => MatchCase.eqValue[A0](singleton, name)
                  // $COVERAGE-ON$
                  case None => MatchCase.typeMatch[A0](name)
                }
              else MatchCase.typeMatch[A0](name)
            mc.map { matched =>
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
      else if (tpe.isUnionType) tpe.directChildren.map(children => new Enum(tpe, children))
      else None

    /** Parses a type as an [[Enum]].
      *
      * @since 0.3.0
      */
    def parse[A: Type]: ClassViewResult[Enum[A]] =
      unapply(Type[A]) match {
        case Some(e) => ClassViewResult.Compatible(e)
        case None    =>
          if (Type.isSealed[A] || Type.isEnumeration[A] || Type.isUnionType[A])
            ClassViewResult.Incompatible(
              s"${Type.prettyPrint[A]} is sealed/enumeration/union but has no direct children"
            )
          else
            ClassViewResult.Incompatible(
              s"${Type.prettyPrint[A]} is not sealed, not an enumeration, and not a union type"
            )
      }
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
      val defaultConstructor: Method
  ) extends Class[A]()(using tpe0) {

    lazy val beanGetters: List[Method] = methods.filter(_.isJavaGetter)
    lazy val beanSetters: List[Method] = methods.filter(_.isJavaSetter)

    def constructWithoutSetters(visibility: Accessible = Everywhere): Option[Expr[A]] =
      if (!defaultConstructor.isAvailable(visibility)) Option.empty[Expr[A]]
      else {
        val result =
          applyAndBuild(defaultConstructor, Map.empty)
            .getOrElse(throw new AssertionError(s"Failed to call the default constructor of ${tpe.prettyPrint}"))
        import result.Underlying
        Some(result.value.upcast[A])
      }

    def constructWithSetters[F[_]: DirectStyle: Applicative](
        setField: JavaBean.SetField[F],
        visibility: Accessible = Everywhere
    ): F[Option[Expr[A]]] =
      constructWithoutSetters(visibility).traverse[F, Expr[A]] { constructorExpr =>
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

    def parConstructWithSetters[F[_]: DirectStyle: Parallel](
        setField: JavaBean.SetField[F],
        visibility: Accessible = Everywhere
    ): F[Option[Expr[A]]] =
      constructWithoutSetters(visibility).traverse[F, Expr[A]] { constructorExpr =>
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
        setter: Method
    ): F[Expr[Unit]] = {
      val (name, param) = setter.totalParameters.flatten.head
      import param.tpe.Underlying
      DirectStyle[F].scoped { runSafe =>
        val value = runSafe(setField(name, param))
        val afterInstance = setter match {
          case oi: Method.OnInstance => oi.apply(constructorResult.asInstanceOf[Expr[oi.Instance]])
          case other                 =>
            throw new AssertionError(
              s"Setter ${setter.name} of ${tpe.prettyPrint} is not an instance method: ${other.getClass.getSimpleName}"
            )
        }
        applyAndBuild(afterInstance, Map(name -> value.as_??)) match {
          case Right(unitResult) =>
            import unitResult.Underlying as UnitType
            unitResult.value.upcast(using implicitly[Type[UnitType]], Type.of[Unit])
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

    /** Parses a type as a [[JavaBean]].
      *
      * @since 0.3.0
      */
    def parse[A: Type]: ClassViewResult[JavaBean[A]] =
      unapply(Type[A]) match {
        case Some(jb) => ClassViewResult.Compatible(jb)
        case None     =>
          if (Type.isPlainOldJavaObject[A] && !Type.isObject[A])
            ClassViewResult.Incompatible(
              s"${Type.prettyPrint[A]} is a POJO but has no public default constructor"
            )
          else
            ClassViewResult.Incompatible(s"${Type.prettyPrint[A]} is not a plain old Java object")
      }

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
