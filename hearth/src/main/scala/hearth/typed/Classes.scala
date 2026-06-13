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

    /** Returns the values of all case fields of the given instance.
      *
      * By default ([[Anywhere]]) ALL case fields are returned, regardless of their visibility. Pass [[AtCallSite]] to
      * keep only the fields accessible at the macro expansion point, or [[Everywhere]] to keep only the fields
      * accessible from any scope (public).
      *
      * On Scala 2 a non-public case field `b` is read through the public synthetic accessor `b$access$1` that scalac
      * generates for it; the returned map is keyed by the declared field name (`b`) on both platforms, and visibility
      * filtering follows the declared field's visibility on both platforms as well.
      */
    def caseFieldValuesAt(
        instance: Expr[A],
        visibility: Accessible = Anywhere
    ): ListMap[String, Expr_??] = ListMap.from(caseFields.filter(_.isAvailable(visibility)).map { field =>
      (field match {
        case oi: Method.OnInstance if field.isNullary =>
          val afterInstance = oi.apply(instance.asInstanceOf[Expr[oi.Instance]])
          afterInstance match {
            case r: Method.Result[?] =>
              import r.Returned
              r.build() match {
                case Right(value) => CaseClass.declaredCaseFieldName(field.name) -> value.as_??
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

    /** On Scala 2 the case accessor of a non-public case field `b` is the public synthetic `b$access$N` method.
      * Normalizes such names back to the declared field name, so that e.g. [[CaseClass.caseFieldValuesAt]] is keyed
      * identically on Scala 2 and Scala 3.
      */
    private[Classes] val syntheticCaseAccessorName = "(.+)\\$access\\$\\d+".r
    private[Classes] def declaredCaseFieldName(name: String): String = name match {
      case syntheticCaseAccessorName(fieldName) => fieldName
      case _                                    => name
    }

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

    /** Children in the order their match cases have to be emitted.
      *
      * For sealed hierarchies/enumerations the declaration order is kept (cases are disjoint, order is irrelevant). For
      * union types the order matters: value/identity matches (precise) are emitted first, then TypeTest extractor cases
      * (total, user-provided discriminators for members a class test cannot tell apart), and runtime class tests last -
      * a class-tested value can only reach its case after every TypeTest-discriminated member had the chance to consume
      * it, so overlapping erasures cannot dispatch to the wrong branch.
      */
    private lazy val childrenInMatchOrder: List[(String, ??<:[A])] =
      if (!Type.isUnionType[A]) directChildren.toList
      else
        // $COVERAGE-OFF$ union types exist only on Scala 3
        directChildren.toList.sortBy { case (_, child) =>
          import child.Underlying as A0
          if (Type.isVal[A0] || Type.isObject[A0]) 0 // matched by value/identity
          else if (Type.unionMemberRequiresTypeTest[A, A0]) 1 // matched by user-provided TypeTest
          else 2 // matched by runtime class test
        } // sortBy is stable: members of the same group keep their declaration order
    // $COVERAGE-ON$

    private def matchCaseFor[A0: Type](name: String): MatchCase[Expr[A0]] =
      if (Type.isUnionType[A] && Type.unionMemberRequiresTypeTest[A, A0])
        // $COVERAGE-OFF$ TypeTest-discriminated union members exist only on Scala 3
        MatchCase.typeTestMatch[A, A0](name)
      // $COVERAGE-ON$
      // Use eqValue for enum vals with erased types (Scala 3 parameterless enum cases, Java enum vals)
      // but NOT for case objects where typeMatch preserves exhaustivity checking
      else if (Type.isVal[A0] && !Type.isObject[A0])
        Expr.singletonOf[A0] match {
          // $COVERAGE-OFF$ singletonOf returns Some only for Scala 3 enum vals, not testable on Scala 2
          case Some(singleton) => MatchCase.eqValue[A0](singleton, name)
          // $COVERAGE-ON$
          case None => MatchCase.typeMatch[A0](name)
        }
      else MatchCase.typeMatch[A0](name)

    def matchOn[F[_]: DirectStyle: Applicative, B: Type](
        value: Expr[A]
    )(handle: Expr_??<:[A] => F[Expr[B]]): F[Option[Expr[B]]] =
      childrenInMatchOrder
        .traverse { case (name, child) =>
          DirectStyle[F].scoped { runSafe =>
            import child.Underlying as A0
            matchCaseFor[A0](name).map { matched =>
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
      childrenInMatchOrder
        .parTraverse { case (name, child) =>
          DirectStyle[F].scoped { runSafe =>
            import child.Underlying as A0
            matchCaseFor[A0](name).map { matched =>
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
          if (Type.isSealed[A] || Type.isEnumeration[A] || Type.isUnionType[A]) {
            // For refused unions we might know an actionable reason (e.g. members that need a TypeTest) -
            // unionRefusalReason is Scala 3-only and always None on Scala 2.
            val reason = Type.unionRefusalReason[A].fold("")(r => s": $r")
            ClassViewResult.Incompatible(
              s"${Type.prettyPrint[A]} is sealed/enumeration/union but has no direct children$reason"
            )
          } else
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

  sealed trait AnonymousInstanceError extends Product with Serializable {
    def message: String
  }
  object AnonymousInstanceError {

    final case class TypeIsFinal(tpe: ??) extends AnonymousInstanceError {
      def message: String = s"Cannot create anonymous instance of final type ${tpe.plainPrint}"
    }

    final case class TypeIsSealed(tpe: ??) extends AnonymousInstanceError {
      def message: String = s"Cannot create anonymous instance of sealed type ${tpe.plainPrint}"
    }

    final case class TypeInaccessible(tpe: ??) extends AnonymousInstanceError {
      def message: String = s"Type ${tpe.plainPrint} is not accessible at the call site"
    }

    final case class ConstructorInaccessible(tpe: ??, constructor: Method) extends AnonymousInstanceError {
      def message: String =
        s"No accessible constructor for ${tpe.plainPrint}: ${constructor.asUntyped.plainPrint(tpe.asUntyped)}"
    }

    final case class MultipleClassParents(types: List[??]) extends AnonymousInstanceError {
      def message: String =
        s"At most one class parent allowed, got: ${types.map(_.plainPrint).mkString(", ")}"
    }

    final case class MissingRequiredOverride(method: Method) extends AnonymousInstanceError {
      def message: String =
        s"Missing required override: ${method.name} (${method.asUntyped.plainPrint(method.untypedInstanceType)})"
    }

    final case class CannotOverrideFinalMethod(method: Method) extends AnonymousInstanceError {
      def message: String =
        s"Cannot override final method: ${method.name} (${method.asUntyped.plainPrint(method.untypedInstanceType)})"
    }

    final case class DiamondConflict(methodName: String, sources: List[??]) extends AnonymousInstanceError {
      def message: String =
        s"Diamond conflict for method '$methodName' — conflicting implementations from: ${sources.map(_.plainPrint).mkString(", ")}"
    }
  }

  sealed trait MethodClassification extends Product with Serializable
  object MethodClassification {
    case object MustOverride extends MethodClassification
    case object MayOverride extends MethodClassification
    case object CannotOverride extends MethodClassification
    case object DiamondConflict extends MethodClassification
  }

  final case class ClassifiedMethod(
      method: Method,
      classification: MethodClassification,
      declaredIn: ??
  )

  final case class OverrideContext(
      self: Expr_??,
      method: UntypedMethod,
      parameters: List[Expr_??],
      returnType: ??
  )

  @FunctionalInterface
  trait OverrideBody {
    def apply(ctx: OverrideContext): Expr_??
  }

  final class AnonymousInstance[A] private (
      tpe0: Type[A],
      val classParent: Option[(??, List[Method])],
      val traitParents: List[??],
      val classifiedMethods: List[ClassifiedMethod]
  ) extends Class[A]()(using tpe0) {

    lazy val mustOverride: List[ClassifiedMethod] =
      classifiedMethods.filter(_.classification == MethodClassification.MustOverride)
    lazy val mayOverride: List[ClassifiedMethod] =
      classifiedMethods.filter(_.classification == MethodClassification.MayOverride)
    lazy val cannotOverride: List[ClassifiedMethod] =
      classifiedMethods.filter(_.classification == MethodClassification.CannotOverride)
    lazy val diamondConflicts: List[ClassifiedMethod] =
      classifiedMethods.filter(_.classification == MethodClassification.DiamondConflict)

    def construct(
        constructor: Option[Method],
        constructorArgs: Arguments,
        overrides: Map[UntypedMethod, OverrideBody]
    ): Either[NonEmptyVector[String], Expr[A]] = {
      val errors = Vector.newBuilder[String]

      val overriddenFinals = overrides.keys.filter(_.isFinal).toList
      overriddenFinals.foreach { m =>
        errors += AnonymousInstanceError.CannotOverrideFinalMethod(m.asTyped[A]).message
      }

      val missingRequired = mustOverride.filterNot(cm => overrides.contains(cm.method.asUntyped))
      missingRequired.foreach { cm =>
        errors += AnonymousInstanceError.MissingRequiredOverride(cm.method).message
      }

      val unresolvedDiamonds = diamondConflicts.filterNot(cm => overrides.contains(cm.method.asUntyped))
      unresolvedDiamonds.foreach { cm =>
        errors += AnonymousInstanceError.DiamondConflict(cm.method.name, List(cm.declaredIn)).message
      }

      classParent match {
        case Some(_) if constructor.isEmpty =>
          errors += "Class parent requires a constructor selection but none was provided"
        case _ => ()
      }

      val allErrors = errors.result()
      NonEmptyVector.fromVector(allErrors) match {
        case Some(nev) => Left(nev)
        case None      =>
          val instanceTpe = UntypedType.fromTyped[A]
          val ctorArgs: List[List[UntypedExpr]] = constructor match {
            case Some(ctor) =>
              UntypedArguments.fromTyped(constructorArgs).adaptToParams(instanceTpe, None, ctor.asUntyped)
            case None => Nil
          }

          val parentTypes: List[UntypedType] = {
            val classType = classParent.map(_._1.asUntyped).toList
            val traitTypes = traitParents.map(_.asUntyped)
            classType ++ traitTypes
          }
          val effectiveParents = if (parentTypes.isEmpty) List(instanceTpe) else parentTypes

          val overrideList = overrides.toList.map { case (untypedMethod, body) =>
            val typedMethod = untypedMethod.asTyped[A]
            val returnTpe: ?? = typedMethod.knownReturning.getOrElse(Type.of[Any].as_??)
            UntypedType.UntypedOverride(
              untypedMethod,
              (selfExpr: UntypedExpr, params: List[UntypedExpr]) => {
                val ctx = OverrideContext(
                  self = selfExpr.as_??,
                  method = untypedMethod,
                  parameters = params.map(_.as_??),
                  returnType = returnTpe
                )
                UntypedExpr.fromTyped(body(ctx).value)
              }
            )
          }

          UntypedType
            .unsafeNewSubtype(instanceTpe, effectiveParents, constructor.map(_.asUntyped), ctorArgs, overrideList)
            .map { untypedResult =>
              val exprResult = untypedResult.as_??
              import exprResult.Underlying
              exprResult.value.upcast[A]
            }
      }
    }

    override def toString: String = s"AnonymousInstance(${tpe.plainPrint})"

    override def equals(other: Any): Boolean = other match {
      case that: AnonymousInstance[?] => tpe =:= that.tpe
      case _                          => false
    }
  }

  object AnonymousInstance {

    def parse[A: Type]: ClassViewResult[AnonymousInstance[A]] =
      parseWithMixins[A](Nil)

    def parseWithMixins[A: Type](mixins: List[??]): ClassViewResult[AnonymousInstance[A]] = {
      val tpe = Type[A]

      if (tpe.isFinal)
        ClassViewResult.Incompatible(AnonymousInstanceError.TypeIsFinal(tpe.as_??).message)
      else if (tpe.isSealed)
        ClassViewResult.Incompatible(AnonymousInstanceError.TypeIsSealed(tpe.as_??).message)
      else if (!tpe.isAvailable(AtCallSite))
        ClassViewResult.Incompatible(AnonymousInstanceError.TypeInaccessible(tpe.as_??).message)
      else {
        val allParentTypes: List[??] = tpe.as_?? :: mixins
        val classParents = allParentTypes.filter(p => !p.asUntyped.isTrait)
        val traitParentsList = allParentTypes.filter(p => p.asUntyped.isTrait)

        if (classParents.size > 1)
          ClassViewResult.Incompatible(AnonymousInstanceError.MultipleClassParents(classParents).message)
        else {
          val classParentValidation: Either[String, Option[(??, List[Method])]] = classParents.headOption match {
            case None     => Right(None)
            case Some(cp) =>
              val ctors = UntypedMethod.constructors(cp.asUntyped).map(_.asTyped[A])
              val accessibleCtors = ctors.filter(_.isAvailable(AtCallSite))
              if (accessibleCtors.isEmpty)
                Left(
                  ctors.headOption
                    .map(c => AnonymousInstanceError.ConstructorInaccessible(cp, c).message)
                    .getOrElse(s"${cp.plainPrint} has no constructors")
                )
              else
                Right(Some((cp, accessibleCtors)))
          }

          classParentValidation match {
            case Left(error)            => ClassViewResult.Incompatible(error)
            case Right(classParentInfo) =>
              val classifiedMethods = classifyMethods[A](allParentTypes)
              ClassViewResult.Compatible(
                new AnonymousInstance[A](tpe, classParentInfo, traitParentsList, classifiedMethods)
              )
          }
        }
      }
    }

    private def classifyMethods[A: Type](parentTypes: List[??]): List[ClassifiedMethod] = {
      val allMethods: List[(Method, ??)] = parentTypes.flatMap { parentType =>
        val methods = UntypedMethod.methods(parentType.asUntyped).map(_.asTyped[A])
        methods
          .filterNot(_.isConstructor)
          .filterNot(_.isSynthetic)
          .map(m => (m, parentType))
      }

      val grouped: Map[String, List[(Method, ??)]] = allMethods.groupBy(_._1.name)

      grouped.toList.sortBy(_._1).flatMap { case (_, methodsWithParent) =>
        val representative = methodsWithParent.head
        val method = representative._1
        val declaredIn = representative._2

        if (method.isFinal) {
          List(ClassifiedMethod(method, MethodClassification.CannotOverride, declaredIn))
        } else if (method.isAbstract) {
          val concreteImpls = methodsWithParent.filterNot(_._1.isAbstract)
          if (concreteImpls.isEmpty) {
            List(ClassifiedMethod(method, MethodClassification.MustOverride, declaredIn))
          } else if (hasDistinctDeclaredImpls(concreteImpls)) {
            List(ClassifiedMethod(method, MethodClassification.DiamondConflict, declaredIn))
          } else {
            List(ClassifiedMethod(concreteImpls.head._1, MethodClassification.MayOverride, concreteImpls.head._2))
          }
        } else {
          val concreteImpls = methodsWithParent.filterNot(_._1.isAbstract)
          if (concreteImpls.size > 1 && hasDistinctDeclaredImpls(concreteImpls)) {
            List(ClassifiedMethod(method, MethodClassification.DiamondConflict, declaredIn))
          } else {
            List(ClassifiedMethod(method, MethodClassification.MayOverride, declaredIn))
          }
        }
      }
    }

    private def hasDistinctDeclaredImpls(impls: List[(Method, ??)]): Boolean = {
      val declared = impls.filter(_._1.isDeclared)
      if (declared.size > 1) {
        val distinctSources = declared.map(_._2).distinctBy(_.asUntyped.plainPrint)
        distinctSources.size > 1
      } else if (declared.isEmpty) {
        false
      } else {
        false
      }
    }
  }
}
