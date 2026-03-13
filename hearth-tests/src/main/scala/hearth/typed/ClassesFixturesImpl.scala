package hearth
package typed

import hearth.data.Data
import hearth.fp.effect.MIO

/** Fixtures for testing [[ClassesSpec]]. */
trait ClassesFixturesImpl { this: MacroCommons =>

  def testClass[A: Type](excluding: VarArgs[String]): Expr[Data] = {
    val excluded = excluding.toIterable.map {
      case Expr(excluding) => excluding
      case unmatched       =>
        Environment.reportErrorAndAbort(
          s"Excluded methods names must be a sequence of strings literals, got ${unmatched.prettyPrint}"
        )
    }.toSet

    val clazz = Class[A]

    val common = Data.map(
      "constructors" -> Data(clazz.constructors.map(renderConstructor(_))),
      "methods" -> Data(clazz.methods.filterNot(m => excluded(m.value.name)).sortBy(_.value.name).map(renderMethod(_)))
    )
    val asCaseClass = clazz.asCaseClass
      .map(cc =>
        Data.map(
          "primaryConstructor" -> renderConstructor(cc.primaryConstructor),
          "nonPrimaryConstructors" -> Data(cc.nonPrimaryConstructors.map(renderConstructor(_))),
          "caseFields" -> Data(cc.caseFields.map(renderMethod(_)))
        )
      )
      .getOrElse(Data("<no case class>"))
    val asEnum = clazz.asEnum
      .map(e =>
        Data.map(
          "directChildren" -> Data(
            e.directChildren
              .map { case (name, child) => s"$name: ${child.Underlying.plainPrint}" }
              .mkString("(", ", ", ")")
          ),
          "exhaustiveChildren" -> Data(
            e.exhaustiveChildren.fold("<no exhaustive children>")(
              _.toList
                .map { case (name, child) =>
                  s"$name: ${child.Underlying.plainPrint}"
                }
                .mkString("(", ", ", ")")
            )
          )
        )
      )
      .getOrElse(Data("<no enum>"))
    val asSingleton = clazz.asSingleton
      .map(s => Data.map("singletonExpr" -> Data(s.singletonExpr.plainPrint)))
      .getOrElse(Data("<no singleton>"))
    val asNamedTuple = clazz.asNamedTuple
      .map(nt =>
        Data.map(
          "fields" -> Data(nt.fields.map { case (name, tpe) => s"$name: ${tpe.plainPrint}" }.mkString("(", ", ", ")"))
        )
      )
      .getOrElse(Data("<no named tuple>"))
    val asJavaBean = clazz.asJavaBean
      .map(jb =>
        Data.map(
          "defaultConstructor" -> renderConstructor(jb.defaultConstructor)
        )
      )
      .getOrElse(Data("<no java bean>"))

    Expr(
      Data.map(
        "commons" -> common,
        "asSingleton" -> asSingleton,
        "asNamedTuple" -> asNamedTuple,
        "asCaseClass" -> asCaseClass,
        "asEnum" -> asEnum,
        "asJavaBean" -> asJavaBean
      )
    )
  }

  private def renderConstructor[A](constructor: Method.NoInstance[A]): Data =
    renderParameters(constructor.parameters)

  private def renderMethod[A](method: Method.Of[A]): Data = Data.map(
    "name" -> Data(method.value.name),
    "parameters" -> renderParameters(method.value.parameters)
  )

  private def renderParameters[A](parameters: Parameters): Data =
    Data(
      parameters
        .map(params =>
          params.view.map { case (name, param) => s"$name: ${param.tpe.plainPrint}" }.mkString("(", ", ", ")")
        )
        .mkString
    )

  private val booleanType: Type[Boolean] = Type.of[Boolean]
  private val intType: Type[Int] = Type.of[Int]
  private val stringType: Type[String] = Type.of[String]
  private val productType: Type[Product] = Type.of[Product]

  def testCaseClassConstructAndParConstruct[A: Type]: Expr[String] = Expr {
    CaseClass.parse[A].toOption.fold("<no case class>") { caseClass =>
      val makeArgument: Parameter => MIO[Expr_??] = field => {
        import field.tpe.Underlying as FieldType
        implicit val IntType: Type[Int] = intType
        implicit val StringType: Type[String] = stringType
        if (FieldType <:< Type[Int]) MIO.pure(Expr(0).as_??)
        else if (FieldType <:< Type[String]) MIO.pure(Expr(field.name).as_??)
        else MIO.fail(new Exception(s"Field $field.name has wrong type: ${field.tpe.plainPrint}"))
      }
      val sequential = caseClass.construct(makeArgument).map { result =>
        result.fold("<failed to construct sequentail>")(_.plainPrint)
      }
      val parallel = caseClass.parConstruct(makeArgument).map { result =>
        result.fold("<failed to construct parallel>")(_.plainPrint)
      }
      sequential
        .parMap2(parallel) { (sequential, parallel) =>
          s"sequential: $sequential, parallel: $parallel"
        }
        .unsafe
        .runSync
        ._2
        .fold(errors => s"<failed to construct: ${errors.mkString(", ")}>", identity)
    }
  }

  def testSingletonExpr[A: Type]: Expr[String] = Expr {
    SingletonValue.parse[A].toOption.fold("<no singleton>") { singleton =>
      s"singletonExpr: ${singleton.singletonExpr.plainPrint}"
    }
  }

  def testCaseClassCaseFieldValuesAt[A: Type](expr: Expr[A]): Expr[String] = Expr {
    CaseClass.parse[A].toOption.fold("<no case class>") { caseClass =>
      caseClass
        .caseFieldValuesAt(expr)
        .toList
        .sortBy(_._1)
        .map { case (name, value) =>
          s"$name: ${value.plainPrint}"
        }
        .mkString("(", ", ", ")")
    }
  }

  def testEnumMatchOnAndParMatchOn[A: Type](expr: Expr[A]): Expr[String] =
    Enum.parse[A].toOption.fold(Expr("<no enum>")) { enumm =>
      implicit val StringType: Type[String] = stringType
      val handle: Expr_??<:[A] => MIO[Expr[String]] = matched => {
        import matched.{Underlying as Subtype, value as matchedExpr}
        MIO.pure(Expr(s"subtype name: ${Subtype.plainPrint}, expr: ${matchedExpr.plainPrint}"))
      }
      val sequential = enumm
        .matchOn(expr)(handle)
        .map(_.getOrElse(Expr("<failed to perform exhaustive match>")))
      val parallel = enumm
        .parMatchOn(expr)(handle)
        .map(_.getOrElse(Expr("<failed to perform exhaustive match>")))
      sequential
        .parMap2(parallel) { (sequential, parallel) =>
          Expr.quote {
            s"sequential: ${Expr.splice(sequential)}, parallel: ${Expr.splice(parallel)}"
          }
        }
        .unsafe
        .runSync
        ._2
        .fold(errors => Expr(s"<failed to construct: ${errors.mkString(", ")}>"), identity)
    }

  /** Diagnostic method for issue #226: reports type info for enum children.
    *
    * For each child of an enum, reports:
    *   - child name and type
    *   - whether it's a val, object, case class
    *   - what singletonOf produces
    */
  def testDependentEnumDiagnostic[A: Type]: Expr[String] = Expr {
    Enum.parse[A].toOption.fold("<no enum>") { enumm =>
      val childInfo = enumm.directChildren.toList.map { case (name, child) =>
        import child.Underlying as A0
        val isVal = Type.isVal[A0]
        val isObject = Type.isObject[A0]
        val isCaseClass = Type.isCaseClass[A0]
        val isCaseObject = Type.isCaseObject[A0]
        val isCaseVal = Type.isCaseVal[A0]
        val singletonExpr = Expr.singletonOf[A0]
        val hasSingleton = singletonExpr.isDefined
        val singletonPrint = singletonExpr.fold("<none>")(_.plainPrint)
        val typePrint = Type.plainPrint[A0]
        val prettyPrint = Type.prettyPrint[A0]
        s"child=$name: type=$typePrint, pretty=$prettyPrint, isVal=$isVal, isObject=$isObject, " +
          s"isCaseClass=$isCaseClass, isCaseObject=$isCaseObject, isCaseVal=$isCaseVal, " +
          s"hasSingleton=$hasSingleton, singletonPrint=$singletonPrint"
      }
      childInfo.mkString("\n")
    }
  }

  def testCaseClassDefaultValues[A: Type]: Expr[String] = Expr {
    CaseClass.parse[A].toOption.fold("<no case class>") { caseClass =>
      caseClass.primaryConstructor.parameters.flatten.toList
        .map { case (name, param) =>
          val defaultStr = if (param.hasDefault) {
            param.defaultValue match {
              case Some(_) => "resolved"
              case None    => "<default missing>"
            }
          } else "<no default>"
          s"$name: hasDefault=${param.hasDefault}, default=$defaultStr"
        }
        .mkString(", ")
    }
  }

  def testNamedTupleConstructAndFields[A: Type]: Expr[String] = Expr {
    NamedTuple.parse[A].toOption.fold("<no named tuple>") { namedTuple =>
      val fieldsStr = namedTuple.fields
        .map { case (name, tpe) => s"$name: ${tpe.plainPrint}" }
        .mkString("(", ", ", ")")

      val makeArgument: CaseClass.ConstructField[MIO] =
        CaseClass.ConstructField[MIO] { field =>
          import field.tpe.Underlying as FieldType
          implicit val IntType: Type[Int] = intType
          implicit val StringType: Type[String] = stringType
          if (FieldType <:< Type[Int]) MIO.pure(Expr(0).as_??)
          else if (FieldType <:< Type[String]) MIO.pure(Expr(field.name).as_??)
          else MIO.fail(new Exception(s"Field ${field.name} has wrong type: ${field.tpe.plainPrint}"))
        }
      val constructed = namedTuple.construct(makeArgument).map { result =>
        result.fold("<failed to construct>")(_.plainPrint)
      }
      constructed.unsafe.runSync._2
        .fold(
          errors => s"fields: $fieldsStr, construct: <failed: ${errors.mkString(", ")}>",
          constructStr => s"fields: $fieldsStr, construct: $constructStr"
        )
    }
  }

  /** Constructs a case class via [[CaseClass.construct]], extracts field values via [[CaseClass.caseFieldValuesAt]],
    * and evaluates everything at runtime.
    *
    * Returns a string like `"a=42"` or `"a=42, b=hello"`.
    */
  def testCaseClassConstructRoundTrip[A: Type]: Expr[String] = {
    implicit val StringType: Type[String] = stringType
    CaseClass.parse[A].toOption.fold(Expr("<no case class>")) { caseClass =>
      val makeArgument: CaseClass.ConstructField[MIO] =
        CaseClass.ConstructField[MIO] { field =>
          import field.tpe.Underlying as FieldType
          implicit val IntType: Type[Int] = intType
          implicit val StringType: Type[String] = stringType
          if (FieldType <:< Type[Int]) MIO.pure(Expr(42).as_??)
          else if (FieldType <:< Type[String]) MIO.pure(Expr("hello").as_??)
          else MIO.fail(new Exception(s"Unsupported type: ${field.tpe.plainPrint}"))
        }
      caseClass.construct(makeArgument).unsafe.runSync._2 match {
        case Right(Some(constructedExpr)) =>
          val fieldValues = caseClass.caseFieldValuesAt(constructedExpr)
          val parts: List[Expr[String]] = fieldValues.toList.map { case (name, fieldExpr) =>
            import fieldExpr.{Underlying, value}
            val nameExpr = Expr(name)
            Expr.quote {
              Expr.splice(nameExpr) + "=" + Expr.splice(value).toString
            }
          }
          if (parts.isEmpty) Expr("<no fields>")
          else
            parts.reduceLeft { (acc, part) =>
              Expr.quote(Expr.splice(acc) + ", " + Expr.splice(part))
            }
        case Right(None)  => Expr("<not constructible>")
        case Left(errors) => Expr(s"<failed: ${errors.mkString(", ")}>")
      }
    }
  }

  /** Evaluates the singleton expression at runtime and returns its `.toString`.
    */
  def testSingletonRoundTrip[A: Type]: Expr[String] =
    SingletonValue.parse[A].toOption.fold(Expr("<no singleton>")) { singleton =>
      Expr.quote {
        Expr.splice(singleton.singletonExpr).toString
      }
    }

  /** Constructs a Java bean via [[JavaBean.constructWithSetters]] and evaluates `.toString` at runtime.
    */
  def testJavaBeanConstructRoundTrip[A: Type]: Expr[String] = {
    implicit val StringType: Type[String] = stringType
    JavaBean.parse[A].toOption.fold(Expr("<no java bean>")) { javaBean =>
      val setField: (String, Parameter) => MIO[Expr_??] = (_, input) => {
        import input.tpe.Underlying as FieldType
        implicit val BooleanType: Type[Boolean] = booleanType
        implicit val IntType: Type[Int] = intType
        implicit val StringType: Type[String] = stringType
        if (FieldType <:< Type[Boolean]) MIO.pure(Expr(true).as_??)
        else if (FieldType <:< Type[Int]) MIO.pure(Expr(42).as_??)
        else if (FieldType <:< Type[String]) MIO.pure(Expr("hello").as_??)
        else MIO.fail(new Exception(s"Unsupported type: ${input.tpe.plainPrint}"))
      }
      javaBean.constructWithSetters(setField).unsafe.runSync._2 match {
        case Right(Some(constructedExpr)) =>
          Expr.quote {
            Expr.splice(constructedExpr).toString
          }
        case Right(None)  => Expr("<not constructible>")
        case Left(errors) => Expr(s"<failed: ${errors.mkString(", ")}>")
      }
    }
  }

  /** Constructs a named tuple via [[NamedTuple.construct]], extracts field values via `Product.productElement`, and
    * evaluates everything at runtime.
    *
    * Returns a string like `"name=hello, age=42"`.
    */
  @scala.annotation.nowarn("msg=is never used")
  def testNamedTupleConstructRoundTrip[A: Type]: Expr[String] = {
    implicit val IntType: Type[Int] = intType
    implicit val StringType: Type[String] = stringType
    implicit val ProductType: Type[Product] = productType
    NamedTuple.parse[A].toOption.fold(Expr("<no named tuple>")) { namedTuple =>
      val makeArgument: CaseClass.ConstructField[MIO] =
        CaseClass.ConstructField[MIO] { field =>
          import field.tpe.Underlying as FieldType
          implicit val IntType: Type[Int] = intType
          implicit val StringType: Type[String] = stringType
          if (FieldType <:< Type[Int]) MIO.pure(Expr(42).as_??)
          else if (FieldType <:< Type[String]) MIO.pure(Expr("hello").as_??)
          else MIO.fail(new Exception(s"Unsupported type: ${field.tpe.plainPrint}"))
        }
      namedTuple.construct(makeArgument).unsafe.runSync._2 match {
        case Right(Some(constructedExpr)) =>
          val fields = namedTuple.primaryConstructor.parameters.flatten.toList
          val parts: List[Expr[String]] = fields.map { case (name, param) =>
            val nameExpr = Expr(name)
            val idx = Expr(param.index)
            Expr.quote {
              val product = Expr.splice(constructedExpr).asInstanceOf[Product]
              Expr.splice(nameExpr) + "=" + product.productElement(Expr.splice(idx)).toString
            }
          }
          if (parts.isEmpty) Expr("<no fields>")
          else
            parts.reduceLeft { (acc, part) =>
              Expr.quote(Expr.splice(acc) + ", " + Expr.splice(part))
            }
        case Right(None)  => Expr("<not constructible>")
        case Left(errors) => Expr(s"<failed: ${errors.mkString(", ")}>")
      }
    }
  }

  def testJavaBeanConstructWithSettersAndParConstructWithSetters[A: Type]: Expr[String] = Expr {
    JavaBean.parse[A].toOption.fold("<no java bean>") { javaBean =>
      val setField: (String, Parameter) => MIO[Expr_??] = (name, input) => {
        import input.tpe.Underlying as FieldType
        implicit val BooleanType: Type[Boolean] = booleanType
        implicit val IntType: Type[Int] = intType
        implicit val StringType: Type[String] = stringType
        if (FieldType <:< Type[Boolean]) MIO.pure(Expr(true).as_??)
        else if (FieldType <:< Type[Int]) MIO.pure(Expr(0).as_??)
        else if (FieldType <:< Type[String]) MIO.pure(Expr(name).as_??)
        else MIO.fail(new Exception(s"Field $name has wrong type: ${input.tpe.plainPrint}"))
      }
      val sequential = javaBean.constructWithSetters(setField).map { result =>
        result.fold("<failed to construct with setters>")(_.plainPrint)
      }
      val parallel = javaBean.parConstructWithSetters(setField).map { result =>
        result.fold("<failed to construct with setters>")(_.plainPrint)
      }
      sequential
        .parMap2(parallel) { (sequential, parallel) =>
          s"sequential:\n$sequential\nparallel:\n$parallel"
        }
        .unsafe
        .runSync
        ._2
        .fold(errors => s"<failed to construct: ${errors.mkString(", ")}>", identity)
    }
  }
}
