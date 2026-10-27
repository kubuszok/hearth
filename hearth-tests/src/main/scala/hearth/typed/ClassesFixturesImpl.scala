package hearth
package typed

import hearth.data.Data
import hearth.fp.effect.MIO

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
      "methods" -> Data(clazz.methods.filterNot(m => excluded(m.name)).sortBy(_.name).map(renderMethod(_)))
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

  private def renderConstructor[A](constructor: Method): Data =
    renderParameters(constructor.totalParameters)

  private def renderMethod[A](method: Method): Data = Data.map(
    "name" -> Data(method.name),
    "parameters" -> renderParameters(method.totalParameters)
  )

  private def renderParameters[A](parameters: Parameters): Data =
    Data(
      parameters
        .map(params =>
          params.view.map { case (name, param) => s"$name: ${param.tpe.plainPrint}" }.mkString("(", ", ", ")")
        )
        .mkString
    )

  /** The type-name prefix (`Type.prettyPrint[A]`) of each reason is stripped, as it is printed differently on Scala 2
    * and Scala 3.
    */
  def testClassViewParseReasons[A: Type]: Expr[Data] = {
    val prefix = removeAnsiColors(Type.prettyPrint[A]) + " "
    def reasonOf[V](result: ClassViewResult[V]): Data = result match {
      case ClassViewResult.Compatible(_)     => Data("<compatible>")
      case ClassViewResult.Incompatible(why) =>
        val cleaned = removeAnsiColors(why)
        Data(if (cleaned.startsWith(prefix)) cleaned.drop(prefix.length) else cleaned)
    }
    Expr(
      Data.map(
        "asSingleton" -> reasonOf(SingletonValue.parse[A]),
        "asNamedTuple" -> reasonOf(NamedTuple.parse[A]),
        "asCaseClass" -> reasonOf(CaseClass.parse[A]),
        "asEnum" -> reasonOf(Enum.parse[A]),
        "asJavaBean" -> reasonOf(JavaBean.parse[A]),
        // Class.parse is the most general view and is always Compatible.
        "asClass" -> reasonOf(Class.parse[A])
      )
    )
  }

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

  def testCaseClassCaseFieldValuesAtCallSite[A: Type](expr: Expr[A]): Expr[String] = Expr {
    CaseClass.parse[A].toOption.fold("<no case class>") { caseClass =>
      caseClass
        .caseFieldValuesAt(expr, visibility = AtCallSite)
        .toList
        .sortBy(_._1)
        .map { case (name, value) =>
          s"$name: ${value.plainPrint}"
        }
        .mkString("(", ", ", ")")
    }
  }

  def testCaseClassCaseFieldValuesAtEverywhere[A: Type](expr: Expr[A]): Expr[String] = Expr {
    CaseClass.parse[A].toOption.fold("<no case class>") { caseClass =>
      caseClass
        .caseFieldValuesAt(expr, visibility = Everywhere)
        .toList
        .sortBy(_._1)
        .map { case (name, value) =>
          s"$name: ${value.plainPrint}"
        }
        .mkString("(", ", ", ")")
    }
  }

  /** `Anywhere` has to be passed explicitly, since the default is `AtCallSite`. */
  def testCaseClassCaseFieldValuesAtAnywhere[A: Type](expr: Expr[A]): Expr[String] = Expr {
    CaseClass.parse[A].toOption.fold("<no case class>") { caseClass =>
      caseClass
        .caseFieldValuesAt(expr, visibility = Anywhere)
        .toList
        .sortBy(_._1)
        .map { case (name, value) =>
          s"$name: ${value.plainPrint}"
        }
        .mkString("(", ", ", ")")
    }
  }

  /** Renders only the parameter-name shape (e.g. `(i)(s)`), so the result is the same on Scala 2 and Scala 3. */
  def testCaseClassCopyMethod[A: Type]: Expr[Data] = {
    def describe(visibility: Accessible)(cc: CaseClass[A]): Data =
      cc.copyMethod(visibility) match {
        case None       => Data("<none>")
        case Some(copy) => Data(copy.asUntyped.parameters.map(_.keys.mkString("(", ", ", ")")).mkString)
      }
    Expr(
      CaseClass.parse[A].toOption.fold(Data("<no case class>")) { cc =>
        Data.map(
          "atCallSite" -> describe(AtCallSite)(cc),
          "everywhere" -> describe(Everywhere)(cc),
          "anywhere" -> describe(Anywhere)(cc)
        )
      }
    )
  }

  /** Only `Int` fields get a value, so the remaining ones exercise `copy`'s default arguments (the original values). */
  def testCaseClassCopyRoundTrip[A: Type](instance: Expr[A]): Expr[String] = {
    implicit val IntType: Type[Int] = intType
    CaseClass.parse[A].toOption.flatMap(cc => cc.copyMethod().map(cc -> _)) match {
      case None            => Expr("<no copy>")
      case Some((_, copy)) =>
        copy.fold(
          onInstance = _ => instance.as_??,
          onTypes = _ => Map.empty,
          onValues = av =>
            av.parameters.flatten.flatMap { case (name, parameter) =>
              import parameter.tpe.Underlying as FieldType
              if (FieldType <:< Type[Int]) List(name -> Expr(99).as_??) else Nil
            }.toMap
        ) match {
          case Right(result) =>
            import result.{Underlying, value}
            Expr.quote(Expr.splice(value).toString)
          case Left(error) => Expr(s"<failed: $error>")
        }
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

  def testEnumParseDiagnostic[A: Type]: Expr[String] = Expr(
    Enum
      .parse[A]
      .toEither
      .fold(
        error => removeAnsiColors(error),
        enumm => s"parsed with children: ${enumm.directChildren.keys.mkString(", ")}"
      )
  )

  /** Handlers built from nested [[Expr.quote]]s splicing the matched expression must not leak splice ownership across
    * case boundaries.
    */
  def testEnumParMatchOnNestedQuote[A: Type](expr: Expr[A], suffix: Expr[String]): Expr[String] =
    Enum.parse[A].toOption.fold(Expr("<no enum>")) { enumm =>
      implicit val StringType: Type[String] = stringType
      val handle: Expr_??<:[A] => MIO[Expr[String]] = matched => {
        import matched.{Underlying as Subtype, value as matchedExpr}
        MIO.pure(Expr.quote {
          Expr.splice(matchedExpr).toString + Expr.splice(suffix)
        })
      }
      enumm
        .parMatchOn(expr)(handle)
        .map(_.getOrElse(Expr("<failed to perform exhaustive match>")))
        .unsafe
        .runSync
        ._2
        .fold(errors => Expr(s"<failed to construct: ${errors.mkString(", ")}>"), identity)
    }

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
        val prettyPrint = removeAnsiColors(Type.prettyPrint[A0])
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

  private val seqIntType: Type[Seq[Int]] = Type.of[Seq[Int]]

  def testVarargCaseClassConstruct[A: Type](expected: Expr[A]): Expr[Data] = {
    implicit val SeqIntType: Type[Seq[Int]] = seqIntType
    CaseClass.parse[A].toOption.fold(Expr(Data("<no case class>"))) { caseClass =>
      val signature = caseClass.primaryConstructor.totalParameters.flatten
        .map { case (name, param) => s"$name: ${param.tpe.plainPrint}${if (param.isVararg) " (vararg)" else ""}" }
        .mkString("(", ", ", ")")
      val fields = caseClass.caseFields
        .map(field => s"${field.name}: ${field.knownReturning.fold("<unknown>")(_.plainPrint)}")
        .mkString("(", ", ", ")")
      val makeArgument: Parameter => MIO[Expr_??] = param =>
        if (param.isVararg) MIO.pure(Expr.quote(Seq(1, 2, 3)).as_??)
        else MIO.fail(new Exception(s"Expected only a vararg parameter, got ${param.name}: ${param.tpe.plainPrint}"))
      caseClass.construct(makeArgument).unsafe.runSync._2 match {
        case Right(Some(constructedExpr)) =>
          val signatureExpr = Expr(signature)
          val fieldsExpr = Expr(fields)
          Expr.quote {
            Data.map(
              "primaryConstructor" -> Data(Expr.splice(signatureExpr)),
              "caseFields" -> Data(Expr.splice(fieldsExpr)),
              "constructedEqualsExpected" -> Data(Expr.splice(constructedExpr) == Expr.splice(expected))
            )
          }
        case Right(None)  => Expr(Data("<not constructible>"))
        case Left(errors) => Expr(Data(s"<failed: ${errors.mkString(", ")}>"))
      }
    }
  }

  def testSingletonRoundTrip[A: Type]: Expr[String] =
    SingletonValue.parse[A].toOption.fold(Expr("<no singleton>")) { singleton =>
      Expr.quote {
        Expr.splice(singleton.singletonExpr).toString
      }
    }

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

  // --- Class views edge cases ---

  def testClassViewKind[A: Type]: Expr[String] = Expr(Class[A] match {
    case _: SingletonValue[?] => "SingletonValue"
    case _: NamedTuple[?]     => "NamedTuple"
    case _: CaseClass[?]      => "CaseClass"
    case _: Enum[?]           => "Enum"
    case _: JavaBean[?]       => "JavaBean"
    case _                    => "Class"
  })

  def testClassMethodOverloads[A: Type](name: Expr[String]): Expr[Data] = {
    val methodName = Expr
      .unapply(name)
      .getOrElse(Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${name.prettyPrint}"))
    Expr(Data(Class[A].method(methodName).map(method => renderParameters(method.totalParameters))))
  }

  def testCaseClassSummary[A: Type]: Expr[Data] = Expr(
    CaseClass.parse[A].toOption.fold(Data("<no case class>")) { cc =>
      Data.map(
        "primaryConstructor" -> renderConstructor(cc.primaryConstructor),
        "nonPrimaryConstructors" -> Data(cc.nonPrimaryConstructors.map(renderConstructor(_))),
        "caseFields" -> Data(
          cc.caseFields.map(field => Data(s"${field.name}: ${field.knownReturning.fold("<unknown>")(_.plainPrint)}"))
        )
      )
    }
  )

  private def constructValueFor(field: Parameter): MIO[Expr_??] = {
    import field.tpe.Underlying as FieldType
    implicit val IntType: Type[Int] = intType
    implicit val StringType: Type[String] = stringType
    implicit val BooleanType: Type[Boolean] = booleanType
    if (FieldType =:= Type[Int]) MIO.pure(Expr(42).as_??)
    else if (FieldType =:= Type[String]) MIO.pure(Expr("hello").as_??)
    else if (FieldType =:= Type[Boolean]) MIO.pure(Expr(true).as_??)
    else MIO.fail(new Exception(s"Unsupported type of ${field.name}: ${field.tpe.plainPrint}"))
  }

  private def joinStrings(parts: List[Expr[String]])(implicit StringType: Type[String]): Expr[String] =
    parts match {
      case Nil          => Expr("<no fields>")
      case head :: tail =>
        tail.foldLeft(head)((acc, part) => Expr.quote(Expr.splice(acc) + ", " + Expr.splice(part)))
    }

  /** Constructs `A` sequentially and in parallel (Int = 42, String = "hello", Boolean = true), and reads the case
    * fields of the constructed value back, all evaluated at runtime.
    */
  def testCaseClassRuntimeRoundTrip[A: Type]: Expr[String] = {
    implicit val StringType: Type[String] = stringType
    val makeArgument: Parameter => MIO[Expr_??] = constructValueFor
    CaseClass.parse[A].toOption.fold(Expr("<no case class>")) { cc =>
      val sequential = cc.construct(makeArgument).unsafe.runSync._2
      val parallel = cc.parConstruct(makeArgument).unsafe.runSync._2
      (sequential, parallel) match {
        case (Right(Some(seqExpr)), Right(Some(parExpr))) =>
          val fields = joinStrings(cc.caseFieldValuesAt(seqExpr).toList.map { case (name, fieldExpr) =>
            import fieldExpr.{Underlying, value}
            val nameExpr = Expr(name)
            Expr.quote(Expr.splice(nameExpr) + "=" + Expr.splice(value).toString)
          })
          Expr.quote {
            Expr.splice(seqExpr).toString + " | " + Expr.splice(parExpr).toString + " | " + Expr.splice(fields)
          }
        case (Right(None), Right(None)) => Expr("<not constructible>")
        case (seqResult, parResult)     =>
          def render(result: Either[hearth.fp.data.NonEmptyVector[Throwable], Option[Expr[A]]]): String =
            result.fold(_.toList.map(_.getMessage).mkString(", "), _.fold("<none>")(_ => "<some>"))
          Expr(s"<failed: sequential: ${render(seqResult)}, parallel: ${render(parResult)}>")
      }
    }
  }

  def testCaseClassConstructVisibility[A: Type]: Expr[Data] = {
    val makeArgument: CaseClass.ConstructField[MIO] = CaseClass.ConstructField[MIO](constructValueFor)
    def constructAt(visibility: Accessible)(cc: CaseClass[A]): Data =
      cc.construct(makeArgument, visibility).unsafe.runSync._2 match {
        case Right(Some(_)) => Data("<constructed>")
        case Right(None)    => Data("<inaccessible>")
        case Left(errors)   => Data(s"<failed: ${errors.toList.map(_.getMessage).mkString(", ")}>")
      }
    Expr(CaseClass.parse[A].toOption.fold(Data("<no case class>")) { cc =>
      Data.map(
        "atCallSite" -> constructAt(AtCallSite)(cc),
        "everywhere" -> constructAt(Everywhere)(cc),
        "anywhere" -> constructAt(Anywhere)(cc)
      )
    })
  }

  def testCaseClassConstructErrorAggregation[A: Type]: Expr[Data] = {
    val failing: Parameter => MIO[Expr_??] = field => MIO.fail(new Exception(s"no value for ${field.name}"))
    def render(result: Either[hearth.fp.data.NonEmptyVector[Throwable], Option[Expr[A]]]): Data =
      result.fold(errors => Data(errors.toList.map(e => Data(e.getMessage))), _ => Data("<no errors>"))
    Expr(CaseClass.parse[A].toOption.fold(Data("<no case class>")) { cc =>
      Data.map(
        "construct" -> render(cc.construct(failing).unsafe.runSync._2),
        "parConstruct" -> render(cc.parConstruct(failing).unsafe.runSync._2)
      )
    })
  }

  def testEnumChildren[A: Type]: Expr[Data] = Expr(
    Enum.parse[A].toOption.fold(Data("<no enum>")) { enumm =>
      Data.map(
        "directChildren" -> Data(enumm.directChildren.toList.map { case (name, child) =>
          Data(s"$name: ${child.Underlying.plainPrint}")
        }),
        "exhaustiveChildren" -> enumm.exhaustiveChildren.fold(Data("<no exhaustive children>"))(children =>
          Data(children.toList.map { case (name, child) => Data(s"$name: ${child.Underlying.plainPrint}") })
        )
      )
    }
  )

  /** Every branch prints the child type it was generated for and the matched value, so the runtime result shows which
    * case caught the value.
    */
  def testEnumMatchOnRuntime[A: Type](expr: Expr[A]): Expr[String] =
    Enum.parse[A].toOption.fold(Expr("<no enum>")) { enumm =>
      implicit val StringType: Type[String] = stringType
      val handle: Expr_??<:[A] => MIO[Expr[String]] = matched => {
        import matched.{Underlying as Subtype, value as matchedExpr}
        val childName = Expr(Subtype.plainPrint)
        MIO.pure(Expr.quote(Expr.splice(childName) + ": " + Expr.splice(matchedExpr).toString))
      }
      val sequential = enumm.matchOn(expr)(handle).map(_.getOrElse(Expr("<no match>")))
      val parallel = enumm.parMatchOn(expr)(handle).map(_.getOrElse(Expr("<no match>")))
      sequential
        .parMap2(parallel)((seq, par) => Expr.quote(Expr.splice(seq) + " / " + Expr.splice(par)))
        .unsafe
        .runSync
        ._2
        .fold(errors => Expr(s"<failed: ${errors.toList.map(_.getMessage).mkString(", ")}>"), identity)
    }

  def testJavaBeanSummary[A: Type]: Expr[Data] = Expr(
    JavaBean.parse[A].toOption.fold(Data("<no java bean>")) { bean =>
      Data.map(
        "defaultConstructor" -> renderConstructor(bean.defaultConstructor),
        "beanGetters" -> Data(bean.beanGetters.map(_.name).sorted.map(Data(_))),
        "beanSetters" -> Data(bean.beanSetters.sortBy(_.name).map(m => renderMethod(m)))
      )
    }
  )

  def testJavaBeanConstructWithoutSettersRoundTrip[A: Type]: Expr[String] = {
    implicit val StringType: Type[String] = stringType
    JavaBean.parse[A].toOption.fold(Expr("<no java bean>")) { bean =>
      bean.constructWithoutSetters() match {
        case Some(constructed) => Expr.quote(Expr.splice(constructed).toString)
        case None              => Expr("<inaccessible>")
      }
    }
  }

  def testJavaBeanConstructVisibility[A: Type]: Expr[Data] = Expr(
    JavaBean.parse[A].toOption.fold(Data("<no java bean>")) { bean =>
      def at(visibility: Accessible): Data =
        Data(bean.constructWithoutSetters(visibility).fold("<inaccessible>")(_ => "<constructed>"))
      Data.map("atCallSite" -> at(AtCallSite), "everywhere" -> at(Everywhere), "anywhere" -> at(Anywhere))
    }
  )

  def testJavaBeanSetterErrorAggregation[A: Type]: Expr[Data] = {
    val failing: (String, Parameter) => MIO[Expr_??] = (name, _) => MIO.fail(new Exception(s"no value for $name"))
    def render(result: Either[hearth.fp.data.NonEmptyVector[Throwable], Option[Expr[A]]]): Data =
      result.fold(errors => Data(errors.toList.map(_.getMessage).sorted.map(Data(_))), _ => Data("<no errors>"))
    Expr(JavaBean.parse[A].toOption.fold(Data("<no java bean>")) { bean =>
      Data.map(
        "constructWithSetters (number of errors)" -> Data(
          bean.constructWithSetters(failing).unsafe.runSync._2.fold(_.toList.size, _ => 0)
        ),
        "parConstructWithSetters" -> render(bean.parConstructWithSetters(failing).unsafe.runSync._2),
        "parConstructWithSetters(SetField)" -> render(
          bean.parConstructWithSetters(JavaBean.SetField[MIO](failing)).unsafe.runSync._2
        )
      )
    })
  }
}
