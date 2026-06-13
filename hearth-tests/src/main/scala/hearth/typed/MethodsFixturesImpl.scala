package hearth
package typed

import hearth.data.Data

/** Fixtures for testing [[MethodsSpec]]. */
trait MethodsFixturesImpl { this: MacroCommons =>

  def testConstructorsExtraction[A: Type]: Expr[Data] = Expr(
    Data.map(
      "primaryConstructor" -> Type[A].primaryConstructor
        .map(renderConstructor(_))
        .getOrElse(Data("<no primary constructor>")),
      "defaultConstructor" -> Type[A].defaultConstructor
        .map(renderConstructor(_))
        .getOrElse(Data("<no default constructor>")),
      "constructors" -> Data(Type[A].constructors.map(renderConstructor(_)))
    )
  )

  private def renderConstructor(constructor: Method): Data =
    renderParameters(constructor.totalParameters)

  def testMethodsExtraction[A: Type](excluding: VarArgs[String]): Expr[Data] = {
    val excluded = excluding.toIterable.map {
      case Expr(excluding) => excluding
      case unmatched       =>
        Environment.reportErrorAndAbort(
          s"Excluded methods names must be a sequence of strings literals, got ${unmatched.prettyPrint}"
        )
    }.toSet
    val methods = Type[A].methods
    val filtered = methods.filterNot(m => excluded(m.name))
    Expr(renderMethods(filtered))
  }

  def testMethodDefaults[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      val methods = Type[A].methods.filter(_.name == name)
      val rendered = methods.map { method =>
        val paramsData = method.totalParameters.toList.flatMap { params =>
          params.toList.map { case (paramName, param) =>
            Data.map(
              "name" -> Data(paramName),
              "hasDefault" -> Data(param.hasDefault)
            )
          }
        }
        Data.map(
          "name" -> Data(method.name),
          "arity" -> Data(method.arity),
          "parameters" -> Data.list(paramsData*)
        )
      }
      Expr(Data.list(rendered*))
    case other =>
      Environment.reportErrorAndAbort(
        s"Method name must be a string literal, got ${other.prettyPrint}"
      )
  }

  def testMethodProperties[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(methodName) =>
      val method = Type[A].methods.filter(_.name == methodName)
      Expr(renderMethods(method))
    case _ =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
  }

  private def renderMethods(methods: List[Method]): Data =
    Data(
      methods
        .groupMapReduce(_.name) { method =>
          val signature = method.totalParameters
            .map { params =>
              params.map { case (_, p) => s"${p.tpe.shortName}" }.mkString("(", ", ", ")")
            }
            .mkString(" ")
          val position =
            method.position
              .map(_.prettyPrintLong)
              .map(value => value.drop(value.indexOf("hearth-tests")))
              .toString
          val props = Data.map(
            "invocation" -> Data(method.asUntyped.invocation.toString),
            "hasTypeParameters" -> Data(method.asUntyped.hasTypeParameters),
            "position" -> Data(position),
            "annotations" -> Data.list(method.annotations.map(e => Data(removeAnsiColors(e.prettyPrint)))*),
            "isConstructor" -> Data(method.isConstructor),
            "isVal" -> Data(method.isVal),
            "isVar" -> Data(method.isVar),
            "isLazy" -> Data(method.isLazy),
            "isDef" -> Data(method.isDef),
            "isFinal" -> Data(method.isFinal),
            "isAbstract" -> Data(method.isAbstract),
            "isOverride" -> Data(method.isOverride),
            "isImplicit" -> Data(method.isImplicit),
            "isDeclared" -> Data(method.isDeclared),
            "isSynthetic" -> Data(method.isSynthetic),
            "isInherited" -> Data(method.isInherited),
            "isAvailable(Everywhere)" -> Data(method.isAvailable(Everywhere)),
            "isAvailable(AtCallSite)" -> Data(method.isAvailable(AtCallSite)),
            "arity" -> Data(method.arity),
            "isNullary" -> Data(method.isNullary),
            "isUnary" -> Data(method.isUnary),
            "isBinary" -> Data(method.isBinary),
            "isConstructorArgument" -> Data(method.isConstructorArgument),
            "isCaseField" -> Data(method.isCaseField),
            "isScalaGetter" -> Data(method.isScalaGetter),
            "isScalaSetter" -> Data(method.isScalaSetter),
            "isScalaAccessor" -> Data(method.isScalaAccessor),
            "isJavaGetter" -> Data(method.isJavaGetter),
            "isJavaSetter" -> Data(method.isJavaSetter),
            "isJavaAccessor" -> Data(method.isJavaAccessor),
            "isAccessor" -> Data(method.isAccessor),
            "scalaAccessorName" -> Data(method.scalaAccessorName.getOrElse("<no scala accessor name>")),
            "javaAccessorName" -> Data(method.javaAccessorName.getOrElse("<no java accessor name>")),
            "accessorName" -> Data(method.accessorName.getOrElse("<no accessor name>"))
          )
          Vector(signature -> props)
        }(_ ++ _)
        .toList
        .flatMap {
          case (_, Vector())                      => Nil
          case (name, Vector((signature, props))) => List(s"$name$signature" -> props)
          case (name, methods)                    => List(name -> Data(methods.toMap))
        }
        .toMap
    )

  private def renderParameters(parameters: Parameters): Data =
    Data(
      parameters.view
        .map(params =>
          Data(
            params.view.map { case (name, parameter) => s"$name: ${parameter.tpe.plainPrint}" }.mkString("(", ", ", ")")
          )
        )
        .mkString
    )

  def testParameterProperties[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      val methods = Type[A].methods.filter(_.name == name)
      val rendered = methods.flatMap { method =>
        method.totalParameters.flatten.map { case (paramName, param) =>
          paramName -> Data.map(
            "isImplicit" -> Data(param.isImplicit),
            "hasDefault" -> Data(param.hasDefault),
            "isByName" -> Data(param.isByName),
            "isVararg" -> Data(param.isVararg)
          )
        }
      }
      Expr(Data(rendered.toMap))
    case _ =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
  }

  /** Reports only availability of the matching methods — usable for types from precompiled libraries, where pinning
    * positions or full signatures would not be stable.
    */
  def testMethodVisibility[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      val methods = Type[A].methods.filter(_.name == name)
      val rendered = methods.map { method =>
        method.name -> Data.map(
          "isAvailable(Everywhere)" -> Data(method.isAvailable(Everywhere)),
          "isAvailable(AtCallSite)" -> Data(method.isAvailable(AtCallSite))
        )
      }
      Expr(Data(rendered.toMap))
    case _ =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
  }

  def testMethodOrdering[A: Type]: Expr[Data] = {
    val methods = Type[A].methods
    val names = methods.map(_.name)
    Expr(Data(names.map(Data(_))))
  }

  def testMethodExpectations[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      val methods = Type[A].methods.filter(_.name == name)
      val rendered = methods.map { method =>
        val expectationStrings = method.expectations.map {
          case MethodExpectation.NeedsInstance   => Data("NeedsInstance")
          case MethodExpectation.NeedsTypes(tps) =>
            val typeParamsStr = tps
              .map(_.map { tp =>
                val upper = UntypedTypeParameter.upperBoundPrint(tp.asUntyped)
                val lower = UntypedTypeParameter.lowerBoundPrint(tp.asUntyped)
                val isTopBound =
                  upper == "scala.Any" || upper.startsWith("scala.Any[") || upper.contains("@") || upper.startsWith("[")
                val isBottomBound = lower == "scala.Nothing" || lower.contains("@") || lower.startsWith("[")
                if (!isTopBound && !isBottomBound) s"${tp.name} <: $upper >: $lower"
                else if (!isTopBound) s"${tp.name} <: $upper"
                else if (!isBottomBound) s"${tp.name} >: $lower"
                else tp.name
              }.mkString(", "))
              .mkString("[", "][", "]")
            Data(s"NeedsTypes$typeParamsStr")
          case MethodExpectation.NeedsValues(ps) =>
            val paramsStr =
              ps.map(
                _.map { case (n, p) =>
                  val tpePlain = {
                    val raw = scala.util.Try(p.tpe.plainPrint).getOrElse(p.tpe.toString)
                    val innerClassPattern = """(.+)\._\d+\.`_\d+\.type`\.(\w+)""".r
                    raw match {
                      case innerClassPattern(outer, inner) => s"$outer#$inner"
                      case other                           => other
                    }
                  }
                  s"$n: $tpePlain"
                }.mkString(", ")
              ).mkString("(", ")(", ")")
            Data(s"NeedsValues$paramsStr")
        }
        Data.map(
          "name" -> Data(method.name),
          "expectations" -> Data.list(expectationStrings*),
          "knownReturning" -> Data(method.knownReturning.map(_.plainPrint).getOrElse("<none>")),
          "toString" -> Data(method.toString)
        )
      }
      Expr(Data.list(rendered*))
    case other =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${other.prettyPrint}")
  }

  /** Exercises `Method.toString` AFTER the builder chain has been partially consumed, so the applied-state rendering
    * branch is hit (instance step -> "on ...", value step -> "applied (...)", plus the inferred "returning ..."
    * suffix).
    */
  def testMethodAppliedToString[A: Type](instance: Expr[A])(methodName: Expr[String]): Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val StringType: Type[String] = this.StringType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method = Type[A].methods.filter(_.name == name) match {
      case method :: Nil => method
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case _             => Environment.reportErrorAndAbort(s"Method $name is ambiguous")
    }
    method match {
      case oi: Method.OnInstance =>
        // After applying the instance the next step carries an AppliedStep.Instance, so toString renders "on <expr>".
        val afterInstance = oi.applyUntyped(instance.asUntyped)
        val afterValues = afterInstance match {
          case av: Method.ApplyValues =>
            val args: Arguments = av.parameters.flatten.map { case (pName, param) =>
              import param.tpe.Underlying
              if (Underlying <:< Type.of[Int]) pName -> Expr(1).as_??
              else if (Underlying <:< Type.of[String]) pName -> Expr("x").as_??
              else Environment.reportErrorAndAbort(s"Unsupported parameter type for $pName: ${param.tpe.plainPrint}")
            }.toMap
            av.apply(args).toString
          case other => other.toString
        }
        Expr(
          Data.map(
            "beforeApplication" -> Data(removeAnsiColors(method.toString)),
            "afterInstance" -> Data(removeAnsiColors(afterInstance.toString)),
            "afterInstanceAndValues" -> Data(removeAnsiColors(afterValues))
          )
        )
      case other =>
        Environment.reportErrorAndAbort(s"Method $name is not an instance method, got ${other.getClass.getSimpleName}")
    }
  }

  /** Exercises constructor `toString` after applying its value arguments — hits the "applied (...)" + "returning ..."
    * rendering of the applied state for a no-instance (constructor) chain.
    */
  def testConstructorAppliedToString[A: Type]: Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val StringType: Type[String] = this.StringType
    Type[A].primaryConstructor match {
      case Some(ctor) =>
        val afterValues = ctor match {
          case av: Method.ApplyValues =>
            val args: Arguments = av.parameters.flatten.map { case (pName, param) =>
              import param.tpe.Underlying
              if (Underlying <:< Type.of[Int]) pName -> Expr(1).as_??
              else if (Underlying <:< Type.of[String]) pName -> Expr("x").as_??
              else Environment.reportErrorAndAbort(s"Unsupported parameter type for $pName: ${param.tpe.plainPrint}")
            }.toMap
            av.apply(args).toString
          case other => other.toString
        }
        Expr(
          Data.map(
            "beforeApplication" -> Data(removeAnsiColors(ctor.toString)),
            "afterValues" -> Data(removeAnsiColors(afterValues))
          )
        )
      case None => Expr(Data("<no primary constructor>"))
    }
  }

  def testConstructorExpectations[A: Type]: Expr[Data] = {
    val ctors = Type[A].constructors
    val rendered = ctors.map { ctor =>
      val expectationStrings = ctor.expectations.map {
        case MethodExpectation.NeedsInstance   => Data("NeedsInstance")
        case MethodExpectation.NeedsTypes(tps) =>
          val typeParamsStr = tps.map(_.map(_.name).mkString(", ")).mkString("[", "][", "]")
          Data(s"NeedsTypes$typeParamsStr")
        case MethodExpectation.NeedsValues(ps) =>
          val paramsStr =
            ps.map(_.map { case (n, p) => s"$n: ${p.tpe.plainPrint}" }.mkString(", ")).mkString("(", ")(", ")")
          Data(s"NeedsValues$paramsStr")
      }
      Data.map(
        "expectations" -> Data.list(expectationStrings*),
        "knownReturning" -> Data(ctor.knownReturning.map(_.plainPrint).getOrElse("<none>")),
        "toString" -> Data(ctor.toString)
      )
    }
    Expr(Data.list(rendered*))
  }

  private val IntType: Type[Int] = Type.of[Int]
  private val StringType: Type[String] = Type.of[String]
  private val ProductType: Type[Product] = Type.of[Product]
  private val SeqIntType: Type[Seq[Int]] = Type.of[Seq[Int]]
  private val SeqStringType: Type[Seq[String]] = Type.of[Seq[String]]
  private val ExampleAnnotationType: Type[hearth.examples.methods.ExampleAnnotation] =
    Type.of[hearth.examples.methods.ExampleAnnotation]
  private val ExampleAnnotation2Type: Type[hearth.examples.methods.ExampleAnnotation2] =
    Type.of[hearth.examples.methods.ExampleAnnotation2]
  private val ParentAnnotationType: Type[hearth.examples.methods.ParentAnnotation] =
    Type.of[hearth.examples.methods.ParentAnnotation]
  private val ChildAnnotationType: Type[hearth.examples.methods.ChildAnnotation] =
    Type.of[hearth.examples.methods.ChildAnnotation]
  private val FieldNameType: Type[hearth.examples.methods.fieldName] =
    Type.of[hearth.examples.methods.fieldName]
  private val FieldFlagsType: Type[hearth.examples.methods.fieldFlags] =
    Type.of[hearth.examples.methods.fieldFlags]

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

  def testConstructNamedTuple[A: Type]: Expr[Data] = {
    implicit val StringType: Type[String] = this.StringType
    implicit val IntType: Type[Int] = this.IntType
    Type[A].primaryConstructor match {
      case Some(constructor) =>
        val arguments: Arguments = constructor.totalParameters.flatten.map { case (name, param) =>
          import param.tpe.Underlying
          val value: Expr_?? =
            if (Underlying <:< Type.of[String]) Expr("Alice").as_??
            else if (Underlying <:< Type.of[Int]) Expr(42).as_??
            else
              Environment.reportErrorAndAbort(
                s"testConstructNamedTuple: unsupported parameter type ${param.tpe.Underlying.prettyPrint}"
              )
          name -> value
        }.toMap
        applyAndBuild(constructor, arguments) match {
          case Right(result) =>
            import result.Underlying as R
            Expr.quote(Data(Expr.splice(result.value.asInstanceOf[Expr[R]]).toString))
          case Left(error) => Expr(Data(s"FAILED: $error"))
        }
      case None => Expr(Data("<no primary constructor>"))
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  def testNamedTupleFieldExtraction[A: Type](instance: Expr[A]): Expr[Data] = {
    implicit val StringType: Type[String] = this.StringType
    implicit val IntType: Type[Int] = this.IntType
    implicit val ProductType: Type[Product] = this.ProductType
    Type[A].primaryConstructor match {
      case Some(constructor) =>
        val fields = constructor.totalParameters.flatten.toList
        val parts: List[Expr[String]] = fields.map { case (name, param) =>
          val idx = Expr(param.index)
          Expr.quote {
            val product = Expr.splice(instance).asInstanceOf[Product]
            Expr.splice(Expr(name)) + "=" + product.productElement(Expr.splice(idx)).toString
          }
        }
        val combined = parts.reduceLeft { (acc, part) =>
          Expr.quote(Expr.splice(acc) + ", " + Expr.splice(part))
        }
        Expr.quote(Data(Expr.splice(combined)))
      case None => Expr(Data("<no primary constructor>"))
    }
  }

  def testMethodPrettyPrint[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      val methods = Type[A].methods.filter(_.name == name)
      val rendered = methods.map { method =>
        Data.map(
          "plainPrint" -> Data(method.plainPrint),
          "prettyPrint" -> Data(method.prettyPrint),
          "prettyPrintStripped" -> Data(removeAnsiColors(method.prettyPrint))
        )
      }
      Expr(Data.list(rendered*))
    case other =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${other.prettyPrint}")
  }

  def testCallInstanceViaFold[A: Type](instance: Expr[A])(methodName: Expr[String])(
      params: VarArgs[Int]
  ): Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val StringType: Type[String] = this.StringType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method = Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil => method
      case _             => Environment.reportErrorAndAbort(s"Method $name is ambiguous")
    }
    val providedParams = params.toVector
    var paramIdx = 0
    val result = method.fold(
      onInstance = _ => instance.as_??,
      onTypes = _ => Map.empty,
      onValues = av =>
        av.parameters.flatten.flatMap { case (pName, param) =>
          import param.tpe.Underlying
          if (param.hasDefault && paramIdx >= providedParams.size) {
            paramIdx += 1
            None
          } else if (Underlying <:< Type.of[Int]) {
            val v = providedParams(paramIdx)
            paramIdx += 1
            Some(pName -> v.as_??)
          } else if (Underlying <:< Type.of[String]) {
            paramIdx += 1
            Some(pName -> Expr("test").as_??)
          } else {
            Environment.reportErrorAndAbort(s"Unsupported type for $pName: ${param.tpe.plainPrint}")
          }
        }.toMap
    )
    result match {
      case Right(expr) =>
        import expr.Underlying as R
        Expr.quote(Data(Expr.splice(expr.value.asInstanceOf[Expr[R]]).toString))
      case Left(error) => Expr(Data(s"FAILED: $error"))
    }
  }

  def testCallConstructorViaFold[A: Type](params: VarArgs[Int]): Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val StringType: Type[String] = this.StringType
    Type[A].primaryConstructor match {
      case Some(constructor) =>
        val providedParams = params.toVector
        var paramIdx = 0
        val result = constructor.fold(
          onInstance = _ => Environment.reportErrorAndAbort("Constructors should not need instance"),
          onTypes = _ => Map.empty,
          onValues = av =>
            av.parameters.flatten.flatMap { case (pName, param) =>
              import param.tpe.Underlying
              if (param.hasDefault && paramIdx >= providedParams.size) {
                paramIdx += 1
                None
              } else if (Underlying <:< Type.of[Int]) {
                val v = providedParams(paramIdx)
                paramIdx += 1
                Some(pName -> v.as_??)
              } else if (Underlying <:< Type.of[String]) {
                paramIdx += 1
                Some(pName -> Expr("test").as_??)
              } else {
                Environment.reportErrorAndAbort(s"Unsupported type for $pName: ${param.tpe.plainPrint}")
              }
            }.toMap
        )
        result match {
          case Right(expr) =>
            import expr.Underlying as R
            Expr.quote(Data(Expr.splice(expr.value.asInstanceOf[Expr[R]]).toString))
          case Left(error) => Expr(Data(s"FAILED: $error"))
        }
      case None => Expr(Data("<no primary constructor>"))
    }
  }

  def testCallInstanceViaFoldF[A: Type](instance: Expr[A])(methodName: Expr[String])(
      params: VarArgs[Int]
  ): Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val StringType: Type[String] = this.StringType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method = Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil => method
      case _             => Environment.reportErrorAndAbort(s"Method $name is ambiguous")
    }
    val providedParams = params.toVector
    var paramIdx = 0
    val result: Option[Either[String, Expr_??]] = method.foldF[Option](
      onInstance = _ => Some(instance.as_??),
      onTypes = _ => Some(Map.empty),
      onValues = av => {
        val resolved = av.parameters.flatten.foldLeft[Option[List[(String, Expr_??)]]](Some(List.empty)) {
          case (None, _)                   => None
          case (Some(acc), (pName, param)) =>
            import param.tpe.Underlying
            if (param.hasDefault && paramIdx >= providedParams.size) {
              paramIdx += 1
              Some(acc)
            } else if (Underlying <:< Type.of[Int] && paramIdx < providedParams.size) {
              val v = providedParams(paramIdx)
              paramIdx += 1
              Some(acc :+ (pName -> v.as_??))
            } else if (Underlying <:< Type.of[String]) {
              paramIdx += 1
              Some(acc :+ (pName -> Expr("test").as_??))
            } else {
              None // unsupported parameter type or not enough arguments - short-circuit the whole foldF
            }
        }
        resolved.map(_.toMap)
      }
    )
    result match {
      case Some(Right(expr)) =>
        import expr.Underlying as R
        Expr.quote(Data(Expr.splice(expr.value.asInstanceOf[Expr[R]]).toString))
      case Some(Left(error)) => Expr(Data(s"FAILED: $error"))
      case None              => Expr(Data("<not applicable>"))
    }
  }

  def testCallInstanceViaFoldMissingArgs[A: Type](instance: Expr[A])(methodName: Expr[String]): Expr[Data] = {
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method = Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil => method
      case _             => Environment.reportErrorAndAbort(s"Method $name is ambiguous")
    }
    val result =
      try
        method.fold(
          onInstance = _ => instance.as_??,
          onTypes = _ => Map.empty,
          onValues = _ => Map.empty // wrong application: no arguments provided at all
        ) match {
          case Right(_)    => "UNEXPECTED SUCCESS"
          case Left(error) => s"LEFT: $error"
        }
      catch {
        case e: HearthRequirementError => s"REQUIREMENT FAILED: ${removeAnsiColors(e.description)}"
      }
    Expr(Data(result))
  }

  private def buildPartialArguments(parameters: Parameters, providedParams: Vector[Expr[Int]]): Arguments = {
    implicit val IntType: Type[Int] = this.IntType
    parameters.flatten.zipWithIndex.flatMap { case ((name, param), index) =>
      import param.tpe.Underlying
      if (!(Underlying <:< Type.of[Int]))
        Environment.reportErrorAndAbort(s"unsupported parameter type ${param.tpe.Underlying.prettyPrint}")
      providedParams.lift(index) match {
        case Some(value)              => Some(name -> value.as_??)
        case None if param.hasDefault => None
        case _                        => Environment.reportErrorAndAbort(s"missing parameter for $name (no default)")
      }
    }.toMap
  }

  def testConstructWithDefaults[A: Type](params: VarArgs[Int]): Expr[Data] =
    Type[A].primaryConstructor match {
      case Some(constructor) =>
        val arguments = buildPartialArguments(constructor.totalParameters, params.toVector)
        val instanceTpe = UntypedType.fromTyped[A]
        val result = constructor.asUntyped.unsafeApplyNoInstance(instanceTpe)(UntypedArguments.fromTyped(arguments))
        Expr.quote(Data(Expr.splice(UntypedExpr.toTyped[A](result)).toString))
      case None => Expr(Data("<no primary constructor>"))
    }

  def testCallNoInstanceMethodWithDefaults[A: Type](methodName: Expr[String])(params: VarArgs[Int]): Expr[Data] = {
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil =>
        method match {
          case _: Method.OnInstance =>
            Environment.reportErrorAndAbort(s"Method $name is not a no-instance method")
          case _ =>
            val arguments = buildPartialArguments(method.totalParameters, params.toVector)
            val instanceTpe = UntypedType.fromTyped[A]
            val result =
              method.asUntyped.unsafeApplyNoInstance(instanceTpe)(UntypedArguments.fromTyped(arguments))
            val rt = method.knownReturning.getOrElse(method.Instance.as_??)
            import rt.Underlying as Returned
            Expr.quote(Data(Expr.splice(UntypedExpr.toTyped[Returned](result)).toString))
        }
      case _ => Environment.reportErrorAndAbort(s"Method $name is ambiguous")
    }
  }

  def testCallInstanceMethodWithDefaults[A: Type](instance: Expr[A])(methodName: Expr[String])(
      params: VarArgs[Int]
  ): Expr[Data] = {
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil =>
        method match {
          case oi: Method.OnInstance =>
            val arguments = buildPartialArguments(method.totalParameters, params.toVector)
            val instanceTpe = UntypedType.fromTyped[A]
            val result =
              method.asUntyped
                .unsafeApplyInstance(instanceTpe)(instance.asUntyped, UntypedArguments.fromTyped(arguments))
            val rt = method.knownReturning.getOrElse(oi.Instance.as_??)
            import rt.Underlying as Returned
            Expr.quote(Data(Expr.splice(UntypedExpr.toTyped[Returned](result)).toString))
          case _ =>
            Environment.reportErrorAndAbort(s"Method $name is not an instance method")
        }
      case _ => Environment.reportErrorAndAbort(s"Method $name is ambiguous")
    }
  }

  def testCallNoInstanceIntMethod[A: Type](methodName: Expr[String])(params: VarArgs[Int]): Expr[Int] = {
    implicit val IntType: Type[Int] = this.IntType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method: Method = Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil =>
        method.knownReturning match {
          case Some(rt) =>
            import rt.Underlying as Returned
            if (!(Returned <:< Type.of[Int])) Environment.reportErrorAndAbort(s"Method $name returns not an Int")
            else method
          case None => Environment.reportErrorAndAbort(s"Method $name has unknown return type")
        }
      case _ => Environment.reportErrorAndAbort(s"Method $name is not unique")
    }
    method match {
      case _: Method.OnInstance =>
        Environment.reportErrorAndAbort(s"Method $name is not a no-instance method")
      case _ =>
        val providedParams = params.toVector
        val arguments = method.totalParameters.flatten.zipWithIndex.flatMap { case ((name, param), index) =>
          providedParams.lift(index) match {
            case _ if !(param.tpe.Underlying <:< Type.of[Int]) =>
              Environment.reportErrorAndAbort(s"Parameter $name has wrong type: ${param.tpe.plainPrint} is not an Int")
            case Some(value)              => Some(name -> value.as_??)
            case None if param.hasDefault => None
            case _ => Environment.reportErrorAndAbort(s"Missing parameter for $name (not default value as well)")
          }
        }.toMap
        applyAndBuild(method, arguments) match {
          case Right(result) =>
            result.value.asInstanceOf[Expr[Int]]
          case Left(error) => Environment.reportErrorAndAbort(s"Failed to call method $name: $error")
        }
    }
  }

  def testCallInstanceIntMethod[A: Type](
      instance: Expr[A]
  )(methodName: Expr[String])(params: VarArgs[Int]): Expr[Int] = {
    implicit val IntType: Type[Int] = this.IntType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method: Method = Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil =>
        method.knownReturning match {
          case Some(rt) =>
            import rt.Underlying as Returned
            if (!(Returned <:< Type.of[Int])) Environment.reportErrorAndAbort(s"Method $name returns not an Int")
            else method
          case None => Environment.reportErrorAndAbort(s"Method $name has unknown return type")
        }
      case _ => Environment.reportErrorAndAbort(s"Method $name is not unique")
    }
    method match {
      case _: Method.OnInstance =>
        val providedParams = params.toVector
        val arguments = method.totalParameters.flatten.zipWithIndex.flatMap { case ((name, param), index) =>
          providedParams.lift(index) match {
            case _ if !(param.tpe.Underlying <:< Type.of[Int]) =>
              Environment.reportErrorAndAbort(s"Parameter $name has wrong type: ${param.tpe.plainPrint} is not an Int")
            case Some(value)              => Some(name -> value.as_??)
            case None if param.hasDefault => None
            case _ => Environment.reportErrorAndAbort(s"Missing parameter for $name (not default value as well)")
          }
        }.toMap
        val instanceTpe = UntypedType.fromTyped[A]
        val result = method.asUntyped
          .unsafeApplyInstance(instanceTpe)(instance.asUntyped, UntypedArguments.fromTyped(arguments))
        result.asTyped[Int]
      case _ =>
        Environment.reportErrorAndAbort(s"Method $name is not an instance method")
    }
  }

  def testCallVarargIntMethod[A: Type](instance: Expr[A])(methodName: Expr[String])(params: VarArgs[Int]): Expr[Int] = {
    implicit val SeqIntType: Type[Seq[Int]] = this.SeqIntType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method = Type[A].methods.filter(_.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil => method
      case _             => Environment.reportErrorAndAbort(s"Method $name is ambiguous")
    }
    val seqExpr: Expr[Seq[Int]] = params.toVector.foldLeft(Expr.quote(Seq.empty[Int])) { (acc, value) =>
      Expr.quote(Expr.splice(acc) :+ Expr.splice(value))
    }
    val result = method.fold(
      onInstance = _ => instance.as_??,
      onTypes = _ => Map.empty,
      onValues = av =>
        av.parameters.flatten.map { case (pName, param) =>
          if (param.isVararg) pName -> seqExpr.as_??
          else Environment.reportErrorAndAbort(s"Expected a vararg parameter, got $pName: ${param.tpe.plainPrint}")
        }.toMap
    )
    result match {
      case Right(expr) => expr.value.asInstanceOf[Expr[Int]]
      case Left(error) => Environment.reportErrorAndAbort(s"Failed to call method $name: $error")
    }
  }

  def testConstructVarargCtor[A: Type](params: VarArgs[String]): Expr[Data] = {
    implicit val SeqStringType: Type[Seq[String]] = this.SeqStringType
    Type[A].primaryConstructor match {
      case Some(ctor) =>
        val seqExpr: Expr[Seq[String]] = params.toVector.foldLeft(Expr.quote(Seq.empty[String])) { (acc, value) =>
          Expr.quote(Expr.splice(acc) :+ Expr.splice(value))
        }
        val arguments: Arguments = ctor.totalParameters.flatten.map { case (pName, param) =>
          if (param.isVararg) pName -> seqExpr.as_??
          else Environment.reportErrorAndAbort(s"Expected a vararg parameter, got $pName: ${param.tpe.plainPrint}")
        }.toMap
        applyAndBuild(ctor, arguments) match {
          case Right(result) =>
            import result.Underlying as R
            Expr.quote(Data(Expr.splice(result.value.asInstanceOf[Expr[R]]).toString))
          case Left(error) => Expr(Data(s"FAILED: $error"))
        }
      case None => Expr(Data("<no primary constructor>"))
    }
  }

  private def destructureAnnotationValues(ann: Expr_??): String =
    Annotations.constructorArguments(ann) match {
      case Some(args) => args.map(_.plainPrint).mkString(", ")
      case None       => "<not a method call>"
    }

  def testAnnotationDestructuring[A: Type]: Expr[Data] = {
    val typeAnnotations = Type.annotations[A].flatMap { ann =>
      import ann.Underlying as AnnType
      val typeMatch =
        if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation2]) "ExampleAnnotation2"
        else if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation]) "ExampleAnnotation"
        else "other"
      if (typeMatch == "other") None
      else
        Some(
          Data.map(
            "source" -> Data("type"),
            "annotationType" -> Data(typeMatch),
            "destructuredArgs" -> Data(destructureAnnotationValues(ann))
          )
        )
    }

    val methodAnnotations = Type[A].methods.flatMap { method =>
      method.annotations.flatMap { ann =>
        import ann.Underlying as AnnType
        val typeMatch =
          if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation2]) "ExampleAnnotation2"
          else if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation]) "ExampleAnnotation"
          else "other"
        if (typeMatch == "other") None
        else
          Some(
            Data.map(
              "source" -> Data(s"method:${method.name}"),
              "annotationType" -> Data(typeMatch),
              "destructuredArgs" -> Data(destructureAnnotationValues(ann))
            )
          )
      }
    }

    Expr(Data.list((typeAnnotations ++ methodAnnotations)*))
  }

  private def renderParameterAnnotations(parameters: Parameters): Data = {
    val rendered = parameters.flatten.map { case (paramName, param) =>
      val annotations = param.annotations.flatMap { ann =>
        import ann.Underlying as AnnType
        val isExampleAnnotation = ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation]
        val isExampleAnnotation2 = ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation2]
        if (!isExampleAnnotation && !isExampleAnnotation2) None
        else
          Some(
            Data.map(
              "isExampleAnnotation" -> Data(isExampleAnnotation),
              "isExampleAnnotation2" -> Data(isExampleAnnotation2),
              "destructuredArgs" -> Data(destructureAnnotationValues(ann))
            )
          )
      }
      Data.map(
        "name" -> Data(paramName),
        "annotations" -> Data.list(annotations*)
      )
    }
    Data.list(rendered*)
  }

  def testParameterAnnotations[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      Type[A].methods.filter(_.name == name) match {
        case method :: Nil => Expr(renderParameterAnnotations(method.totalParameters))
        case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
        case _             => Environment.reportErrorAndAbort(s"Method $name is not unique")
      }
    case _ =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
  }

  def testConstructorParameterAnnotations[A: Type]: Expr[Data] =
    Type[A].primaryConstructor match {
      case Some(constructor) => Expr(renderParameterAnnotations(constructor.totalParameters))
      case None              => Expr(Data("<no primary constructor>"))
    }

  def testAnnotationsOfType[A: Type](methodName: Expr[String]): Expr[Data] = {
    implicit val exampleAnnotationType: Type[hearth.examples.methods.ExampleAnnotation] = this.ExampleAnnotationType
    implicit val exampleAnnotation2Type: Type[hearth.examples.methods.ExampleAnnotation2] = this.ExampleAnnotation2Type
    implicit val parentAnnotationType: Type[hearth.examples.methods.ParentAnnotation] = this.ParentAnnotationType
    implicit val childAnnotationType: Type[hearth.examples.methods.ChildAnnotation] = this.ChildAnnotationType

    def decodeArgs(ann: Expr_??): Data =
      Annotations.constructorArguments(ann) match {
        case Some(args) =>
          Data.list(args.map { arg =>
            arg.value.semiEval.fold(errors => Data(s"<failed: ${errors.head}>"), value => Data(value.toString))
          }*)
        case None => Data("<not a constructor call>")
      }

    val typeSection = Data.map(
      "hasExampleAnnotation" -> Data(Type[A].hasAnnotationOfType[hearth.examples.methods.ExampleAnnotation]),
      "hasExampleAnnotation2" -> Data(Type[A].hasAnnotationOfType[hearth.examples.methods.ExampleAnnotation2]),
      "exampleAnnotation2Args" -> Data.list(
        Type[A].annotationsOfType[hearth.examples.methods.ExampleAnnotation2].map(ann => decodeArgs(ann.as_??))*
      ),
      "parentAnnotations" -> Data(Type[A].annotationsOfType[hearth.examples.methods.ParentAnnotation].size),
      "childAnnotations" -> Data(Type[A].annotationsOfType[hearth.examples.methods.ChildAnnotation].size)
    )

    def describeParameter(paramName: String, param: Parameter): Data = Data.map(
      "name" -> Data(paramName),
      "hasExampleAnnotation" -> Data(param.hasAnnotationOfType[hearth.examples.methods.ExampleAnnotation]),
      "exampleAnnotation2Args" -> Data.list(
        param.annotationsOfType[hearth.examples.methods.ExampleAnnotation2].map(ann => decodeArgs(ann.as_??))*
      ),
      "parentAnnotations" -> Data(param.annotationsOfType[hearth.examples.methods.ParentAnnotation].size),
      "childAnnotations" -> Data(param.annotationsOfType[hearth.examples.methods.ChildAnnotation].size)
    )

    val methodSection = Expr.unapply(methodName).filter(_.nonEmpty) match {
      case None       => Data("<no method requested>")
      case Some(name) =>
        Type[A].methods.filter(_.name == name) match {
          case method :: Nil =>
            Data.map(
              "hasExampleAnnotation" -> Data(method.hasAnnotationOfType[hearth.examples.methods.ExampleAnnotation]),
              "hasExampleAnnotation2" -> Data(method.hasAnnotationOfType[hearth.examples.methods.ExampleAnnotation2]),
              "exampleAnnotation2Args" -> Data.list(
                method.annotationsOfType[hearth.examples.methods.ExampleAnnotation2].map(ann => decodeArgs(ann.as_??))*
              ),
              "parentAnnotations" -> Data(method.annotationsOfType[hearth.examples.methods.ParentAnnotation].size),
              "childAnnotations" -> Data(method.annotationsOfType[hearth.examples.methods.ChildAnnotation].size),
              "parameters" -> Data.list(method.totalParameters.flatten.map { case (paramName, param) =>
                describeParameter(paramName, param)
              }*)
            )
          case Nil => Data("<method not found>")
          case _   => Data("<method ambiguous>")
        }
    }

    val constructorSection = Type[A].primaryConstructor match {
      case Some(constructor) =>
        Data.list(constructor.totalParameters.flatten.map { case (paramName, param) =>
          describeParameter(paramName, param)
        }*)
      case None => Data("<no primary constructor>")
    }

    Expr(
      Data.map(
        "type" -> typeSection,
        "method" -> methodSection,
        "constructor" -> constructorSection
      )
    )
  }

  def testAnnotationValueDecoded[A: Type]: Expr[Data] = {
    implicit val exampleAnnotation2Type: Type[hearth.examples.methods.ExampleAnnotation2] = this.ExampleAnnotation2Type
    Type[A].annotationsOfType[hearth.examples.methods.ExampleAnnotation2] match {
      case ann :: Nil =>
        // `ann` is a properly typed `Expr[ExampleAnnotation2]`, so it can be evaluated as a whole at macro time...
        val wholeAnnotation =
          ann.semiEval.fold(errors => s"<failed: ${errors.head}>", instance => instance.value.toString)
        // ... and its constructor arguments can be extracted and decoded one by one.
        val firstArgument = Annotations
          .constructorArguments(ann)
          .flatMap(_.headOption)
          .map(arg => arg.value.semiEval.fold(errors => s"<failed: ${errors.head}>", value => value.toString))
          .getOrElse("<no constructor arguments>")
        Expr(Data.map("wholeAnnotation" -> Data(wholeAnnotation), "firstArgument" -> Data(firstArgument)))
      case other => Expr(Data(s"<expected exactly one ExampleAnnotation2, got ${other.size}>"))
    }
  }

  /** Reproducer for issue #283: read literal annotation arguments (String/Boolean/Double, not just Int) off a plain
    * (non-case) annotation class with `val` constructor parameters, attached to case class constructor parameters.
    *
    * `fieldNames` uses the exact composition recommended to users (annotationsOfType + decodedConstructorArguments);
    * `fieldFlags` decodes every argument, exercising Boolean and Double literals.
    */
  def testFieldNameReproducer[A: Type]: Expr[Data] = {
    implicit val fieldNameType: Type[hearth.examples.methods.fieldName] = this.FieldNameType
    implicit val fieldFlagsType: Type[hearth.examples.methods.fieldFlags] = this.FieldFlagsType

    Type[A].primaryConstructor match {
      case Some(ctor) =>
        val params = ctor.totalParameters.flatten.map { case (paramName, param) =>
          val names = param
            .annotationsOfType[hearth.examples.methods.fieldName]
            .flatMap { ann =>
              Annotations
                .decodedConstructorArguments(ann)
                .flatMap(_.headOption.flatMap(_.toOption))
                .collect { case s: String => s }
            }
          val flags = param.annotationsOfType[hearth.examples.methods.fieldFlags].map { ann =>
            Annotations.decodedConstructorArguments(ann) match {
              case Some(args) =>
                Data.list(args.map(_.fold(error => Data(s"<failed: $error>"), value => Data(value.toString)))*)
              case None => Data("<not a constructor call>")
            }
          }
          Data.map(
            "name" -> Data(paramName),
            "fieldNames" -> Data.list(names.map(Data(_))*),
            "fieldFlags" -> Data.list(flags*)
          )
        }
        Expr(Data.list(params*))
      case None => Expr(Data("<no primary constructor>"))
    }
  }

  /** Splices annotation exprs into the macro OUTPUT (not just reading them at macro time): the generated code calls
    * `.value` on the spliced annotation instance at runtime. On Scala 3, annotation trees carry no source spans, so
    * this verifies that Hearth makes them spliceable (would otherwise fail `-Xcheck-macros` with "position not set").
    */
  def testSplicedAnnotationValue[A: Type](methodName: Expr[String]): Expr[Data] = {
    implicit val exampleAnnotation2Type: Type[hearth.examples.methods.ExampleAnnotation2] = this.ExampleAnnotation2Type

    def spliceFirst(annotations: List[Expr[hearth.examples.methods.ExampleAnnotation2]]): Expr[Int] =
      annotations match {
        case ann :: _ => Expr.quote(Expr.splice(ann).value)
        case Nil      => Expr(-1)
      }

    val typeValue = spliceFirst(Type[A].annotationsOfType[hearth.examples.methods.ExampleAnnotation2])

    val methodValue = Expr.unapply(methodName).filter(_.nonEmpty) match {
      case Some(name) =>
        Type[A].methods.filter(_.name == name) match {
          case method :: Nil => spliceFirst(method.annotationsOfType[hearth.examples.methods.ExampleAnnotation2])
          case _             => Expr(-2)
        }
      case None => Expr(-1)
    }

    val parameterValue = Type[A].primaryConstructor match {
      case Some(constructor) =>
        spliceFirst(constructor.totalParameters.flatten.toList.flatMap { case (_, param) =>
          param.annotationsOfType[hearth.examples.methods.ExampleAnnotation2]
        })
      case None => Expr(-1)
    }

    Expr.quote {
      Data.map(
        "typeValue" -> Data(Expr.splice(typeValue)),
        "methodValue" -> Data(Expr.splice(methodValue)),
        "parameterValue" -> Data(Expr.splice(parameterValue))
      )
    }
  }

  def testDefaultValueOnGenericMethod[A: Type](
      instance: Expr[A],
      methodName: Expr[String]
  ): Expr[Data] = {
    val _ = Expr.unapply(methodName) // suppress unused
    CaseClass.parse[A].toOption match {
      case None     => Expr(Data("not a case class"))
      case Some(cc) =>
        val defaultResults = cc.primaryConstructor.totalParameters.flatten.map { case (pName, param) =>
          param.defaultValue match {
            case Some(defaultMethod) =>
              val result = defaultMethod.fold(
                onInstance = _ => instance.as_??,
                onTypes = _ => Map.empty,
                onValues = _ => Map.empty
              )
              result match {
                case Right(expr) =>
                  Data.map("name" -> Data(pName), "default" -> Data(removeAnsiColors(expr.prettyPrint)))
                case Left(error) => Data.map("name" -> Data(pName), "error" -> Data(error))
              }
            case None =>
              Data.map("name" -> Data(pName), "default" -> Data("<none>"))
          }
        }
        Expr(Data.list(defaultResults*))
    }
  }

  def testFoldAnonymousInstanceMethod[A: Type](instance: Expr[A], methodName: Expr[String]): Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val StringType: Type[String] = this.StringType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    AnonymousInstance.parse[A] match {
      case ClassViewResult.Incompatible(reason) =>
        Expr(Data(s"INCOMPATIBLE: $reason"))
      case ClassViewResult.Compatible(ai) =>
        val method = ai.mustOverride.find(_.method.name == name) match {
          case Some(cm) => cm.method
          case None     => Environment.reportErrorAndAbort(s"Method $name not found in mustOverride")
        }
        val result = method.fold(
          onInstance = _ => instance.as_??,
          onTypes = _ => Map.empty,
          onValues = av =>
            av.parameters.flatten.flatMap { case (pName, param) =>
              import param.tpe.Underlying
              if (Underlying <:< Type.of[Int]) Some(pName -> Expr(42).as_??)
              else if (Underlying <:< Type.of[String]) Some(pName -> Expr("test").as_??)
              else None
            }.toMap
        )
        result match {
          case Right(expr) =>
            import expr.Underlying as R
            Expr.quote(Data(Expr.splice(expr.value.asInstanceOf[Expr[R]]).toString))
          case Left(error) => Expr(Data(s"FAILED: $error"))
        }
    }
  }
}
