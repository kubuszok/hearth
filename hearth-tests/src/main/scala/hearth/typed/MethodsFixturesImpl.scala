package hearth
package typed

import hearth.data.Data

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

  // An intersection type `A & B` has no single `typeSymbol` to list the methods of.
  def testExposesNameAndAge[A: Type]: Expr[Data] =
    Expr(Data(Method.unsortedMethodsNamed[A]("name").nonEmpty && Method.unsortedMethodsNamed[A]("age").nonEmpty))

  // `getters`/`island`/`settle` merely start with a prefix but have no upper-case char after it, so they must NOT be
  // JavaBean accessors.
  def testJavaAccessorNames[A: Type]: Expr[Data] =
    Expr(
      Data.map(
        Type[A].methods.view
          .filter(_.isDeclared) // skip inherited members like Object#getClass (a legitimate getter)
          .filter(m => m.name.startsWith("get") || m.name.startsWith("is") || m.name.startsWith("set"))
          .map(m => m.name -> Data(m.javaAccessorName.getOrElse("<none>")))
          .toSeq*
      )
    )

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

  def testMethodAvailability[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      Expr(Type[A].methods.find(_.name == name) match {
        case Some(method) =>
          Data.map(
            "Everywhere" -> Data(method.isAvailable(Everywhere)),
            "AtCallSite" -> Data(method.isAvailable(AtCallSite)),
            "Anywhere" -> Data(method.isAvailable(Anywhere))
          )
        case None => Data("<not listed>")
      })
    case _ => Environment.reportErrorAndAbort("Method name must be a literal")
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

  // An inherited public constructor `val` must be listed as a usable, inherited accessor (isVal + isAvailable +
  // isInherited), not as the subclass's private param-accessor plumbing.
  def testAccessorMetadata[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      Expr(Data(Type[A].methods.filter(_.name == name).map { m =>
        Data.map(
          "isVal" -> Data(m.isVal),
          "isInherited" -> Data(m.isInherited),
          "isAvailable(Everywhere)" -> Data(m.isAvailable(Everywhere))
        )
      }))
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
            "isVararg" -> Data(param.isVararg),
            "byNameUnderlying" -> Data(param.byNameUnderlying.map(_.plainPrint).getOrElse("<none>"))
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

  // The applied type arguments must be SUBSTITUTED into a value/implicit clause that follows the
  // type-parameter clause: applying `T := Int` to `pick[T](t: T)(implicit ord: Ordering[T])` should present
  // `t: scala.Int` and `ord: scala.math.Ordering[scala.Int]` to `onValues` (not the abstract `T`/`Ordering[T]`).
  def testFoldSubstitutesTypeArgs: Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val instanceType: Type[examples.methods.ProperKindedImplicit] =
      Type.of[examples.methods.ProperKindedImplicit]
    val method = Type[examples.methods.ProperKindedImplicit].methods
      .find(_.name == "pick")
      .getOrElse(Environment.reportErrorAndAbort("method `pick` not found"))
    val captured = scala.collection.mutable.ListBuffer.empty[String]
    val result = method.fold(
      onInstance = _ => Expr.quote(new examples.methods.ProperKindedImplicit).as_??,
      onTypes = at => at.typeParams.flatten.map(tp => tp -> Type.of[Int].as_??).toMap,
      onValues = av =>
        av.parameters.flatten.map { case (n, p) =>
          captured += s"$n: ${p.tpe.plainPrint}"
          import p.tpe.Underlying
          val value: Expr_?? =
            if (Underlying <:< Type.of[Int]) Expr(0).as_??
            else
              Expr
                .summonImplicitByType(p.tpe.asUntyped)
                .map(UntypedExpr.as_??(_))
                .getOrElse(Environment.reportErrorAndAbort(s"cannot summon ${p.tpe.plainPrint}"))
          n -> value
        }.toMap
    )
    val built = result match {
      case Right(_)    => "built"
      case Left(error) => s"FAILED: $error"
    }
    Expr(Data.map("params" -> Data.list(captured.toList.map(Data(_))*), "result" -> Data(built)))
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

  def testTypeAnnotations[A: Type]: Expr[Data] = {
    def annNames(anns: List[Expr_??]): Data = Data.list(anns.map { ann =>
      if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation2]) Data("ExampleAnnotation2")
      else if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation]) Data("ExampleAnnotation")
      else Data(ann.Underlying.plainPrint)
    }*)

    val fields = Type[A].primaryConstructor.toList.flatMap(_.totalParameters.flatten).map { case (paramName, param) =>
      import param.tpe.Underlying as FieldTpe
      Data.map(
        "name" -> Data(paramName),
        "typeAnnotations" -> annNames(Type.typeAnnotations[FieldTpe])
      )
    }

    Expr(
      Data.map(
        "own" -> annNames(Type.typeAnnotations[A]),
        "fields" -> Data.list(fields*)
      )
    )
  }

  // Type-position annotations must ALSO survive the caseFields / knownReturning path (each case-field
  // GETTER's return type), not only the primaryConstructor.totalParameters path exercised by testTypeAnnotations.
  def testTypeAnnotationsViaKnownReturning[A: Type]: Expr[Data] = {
    def annNames(anns: List[Expr_??]): Data = Data.list(anns.map { ann =>
      if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation2]) Data("ExampleAnnotation2")
      else if (ann.Underlying =:= Type.of[hearth.examples.methods.ExampleAnnotation]) Data("ExampleAnnotation")
      else Data(ann.Underlying.plainPrint)
    }*)

    val fields = CaseClass.parse[A].toOption.toList.flatMap(_.caseFields).flatMap { method =>
      method.knownReturning.map { rt =>
        import rt.Underlying as FieldTpe
        Data.map(
          "name" -> Data(method.name),
          "typeAnnotations" -> annNames(Type.typeAnnotations[FieldTpe])
        )
      }
    }

    Expr(Data.list(fields*))
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
        val wholeAnnotation =
          ann.semiEval.fold(errors => s"<failed: ${errors.head}>", instance => instance.value.toString)
        val firstArgument = Annotations
          .constructorArguments(ann)
          .flatMap(_.headOption)
          .map(arg => arg.value.semiEval.fold(errors => s"<failed: ${errors.head}>", value => value.toString))
          .getOrElse("<no constructor arguments>")
        Expr(Data.map("wholeAnnotation" -> Data(wholeAnnotation), "firstArgument" -> Data(firstArgument)))
      case other => Expr(Data(s"<expected exactly one ExampleAnnotation2, got ${other.size}>"))
    }
  }

  /** `fieldNames` uses the composition we recommend to users (annotationsOfType + decodedConstructorArguments). */
  def testFieldNamesFromAnnotations[A: Type]: Expr[Data] = {
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

  private def literalName(methodName: Expr[String]): String = Expr
    .unapply(methodName)
    .getOrElse(Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}"))

  private def namedMethods[A: Type](methodName: Expr[String]): List[Method] = {
    val name = literalName(methodName)
    Type[A].methods.filter(_.name == name)
  }

  private def variantName(method: Method): String = method match {
    case _: Method.OnInstance  => "OnInstance"
    case _: Method.ApplyTypes  => "ApplyTypes"
    case _: Method.ApplyValues => "ApplyValues"
    case _: Method.Result[?]   => "Result"
  }

  private def renderExpectationKinds(method: Method): String = method.expectations
    .map {
      case MethodExpectation.NeedsInstance   => "NeedsInstance"
      case MethodExpectation.NeedsTypes(tps) => tps.map(_.map(_.name).mkString(", ")).mkString("NeedsTypes[", "][", "]")
      case MethodExpectation.NeedsValues(ps) =>
        ps.map(_.keys.mkString(", ")).mkString("NeedsValues(", ")(", ")")
    }
    .mkString(" -> ")

  private def renderParameterNames(parameters: Parameters): String =
    if (parameters.isEmpty) "<none>" else parameters.map(_.keys.mkString(", ")).mkString("(", ")(", ")")

  // prettyPrint is only checked for agreeing with plainPrint once its ANSI codes are removed, because exact escape
  // sequences are not part of the contract.
  def testMethodSignatures[A: Type](methodName: Expr[String]): Expr[Data] =
    Expr(Data.list(namedMethods[A](methodName).map(renderSignatureConsistency(_))*))

  def testConstructorSignatures[A: Type]: Expr[Data] =
    Expr(Data.list(Type[A].constructors.map(renderSignatureConsistency(_)).sortBy(_.toString)*))

  private def renderSignatureConsistency(method: Method): Data = {
    val plain = method.plainPrint
    val pretty = method.prettyPrint
    val stripped = removeAnsiColors(pretty)
    if (stripped != plain) Data(s"$plain <prettyPrint without ANSI differs: $stripped>")
    else if (pretty == plain) Data(s"$plain <prettyPrint has no ANSI codes>")
    else Data(plain)
  }

  def testMethodFlags[A: Type](methodName: Expr[String]): Expr[Data] =
    Expr(Data.list(namedMethods[A](methodName).map(renderFlags(_))*))

  def testConstructorFlags[A: Type]: Expr[Data] =
    Expr(Data.list(Type[A].constructors.sortBy(_.plainPrint).map { ctor =>
      Data.map(
        "plainPrint" -> Data(ctor.plainPrint),
        "variant" -> Data(variantName(ctor)),
        "flags" -> renderFlags(ctor)
      )
    }*))

  private def renderFlags(method: Method): Data = Data.map(
    "isConstructor" -> Data(method.isConstructor),
    "isPrivate" -> Data(method.isPrivate),
    "isProtected" -> Data(method.isProtected),
    "privateWithin" -> Data(method.privateWithin.getOrElse("<none>")),
    "protectedWithin" -> Data(method.protectedWithin.getOrElse("<none>")),
    "isAvailable(Everywhere)" -> Data(method.isAvailable(Everywhere)),
    "isAvailable(AtCallSite)" -> Data(method.isAvailable(AtCallSite)),
    "isAvailable(Anywhere)" -> Data(method.isAvailable(Anywhere)),
    "isFinal" -> Data(method.isFinal),
    "isAbstract" -> Data(method.isAbstract),
    "isOverride" -> Data(method.isOverride),
    "isImplicit" -> Data(method.isImplicit),
    "isVal" -> Data(method.isVal),
    "isVar" -> Data(method.isVar),
    "isLazy" -> Data(method.isLazy),
    "isDef" -> Data(method.isDef),
    "isDeclared" -> Data(method.isDeclared),
    "isInherited" -> Data(method.isInherited),
    "isSynthetic" -> Data(method.isSynthetic)
  )

  def testMethodAccessorsAndArity[A: Type](methodName: Expr[String]): Expr[Data] =
    Expr(Data.list(namedMethods[A](methodName).map { method =>
      Data.map(
        "arity" -> Data(method.arity),
        "isNullary" -> Data(method.isNullary),
        "isUnary" -> Data(method.isUnary),
        "isBinary" -> Data(method.isBinary),
        "isNAry(3)" -> Data(method.isNAry(3)),
        "isScalaGetter" -> Data(method.isScalaGetter),
        "isScalaSetter" -> Data(method.isScalaSetter),
        "isScalaAccessor" -> Data(method.isScalaAccessor),
        "scalaAccessorName" -> Data(method.scalaAccessorName.getOrElse("<none>")),
        "isJavaGetter" -> Data(method.isJavaGetter),
        "isJavaSetter" -> Data(method.isJavaSetter),
        "isJavaAccessor" -> Data(method.isJavaAccessor),
        "javaAccessorName" -> Data(method.javaAccessorName.getOrElse("<none>")),
        "isAccessor" -> Data(method.isAccessor),
        "accessorName" -> Data(method.accessorName.getOrElse("<none>"))
      )
    }*))

  def testMethodParameters[A: Type](methodName: Expr[String]): Expr[Data] =
    Expr(Data.list(namedMethods[A](methodName).map(m => renderParameterDetails(m.totalParameters))*))

  def testConstructorParameters[A: Type]: Expr[Data] =
    Expr(Type[A].primaryConstructor match {
      case Some(ctor) => renderParameterDetails(ctor.totalParameters)
      case None       => Data("<no primary constructor>")
    })

  // The by-name parameter's own `tpe` is represented differently on Scala 2 and 3, so only its
  // `byNameUnderlying` is rendered.
  private def renderParameterDetails(parameters: Parameters): Data =
    Data.list(parameters.map { clause =>
      Data.list(clause.toList.map { case (name, param) =>
        Data.map(
          "name" -> Data(name),
          "param.name" -> Data(param.name),
          "index" -> Data(param.index),
          "tpe" -> Data(if (param.isByName) "<by-name>" else param.tpe.plainPrint),
          "hasDefault" -> Data(param.hasDefault),
          "defaultValue" -> Data(param.defaultValue match {
            case Some(default) =>
              s"returning ${default.knownReturning.map(_.plainPrint).getOrElse("<unknown>")}, " +
                s"expecting ${renderExpectationKinds(default)}"
            case None => "<none>"
          }),
          "isByName" -> Data(param.isByName),
          "byNameUnderlying" -> Data(param.byNameUnderlying.map(_.plainPrint).getOrElse("<none>")),
          "isVararg" -> Data(param.isVararg),
          "isImplicit" -> Data(param.isImplicit)
        )
      }*)
    }*)

  /** Walks the builder chain by hand, applying `Int` to every type parameter, the provided `Int`s (in order) to every
    * `Int` parameter, `"s"` to every `String` parameter, and skipping parameters with defaults once the provided `Int`s
    * run out. Every visited step is rendered, and the built call is evaluated at runtime.
    */
  def testChainWalk[A: Type](instance: Expr[A])(methodName: Expr[String])(params: VarArgs[Int]): Expr[Data] =
    namedMethods[A](methodName) match {
      case method :: Nil => walkChain(method, Some(instance.as_??), params.toVector)
      case Nil           => Environment.reportErrorAndAbort(s"Method ${literalName(methodName)} not found")
      case _             => Environment.reportErrorAndAbort(s"Method ${literalName(methodName)} is ambiguous")
    }

  def testConstructorChainWalk[A: Type](params: VarArgs[Int]): Expr[Data] =
    Type[A].primaryConstructor match {
      case Some(ctor) => walkChain(ctor, None, params.toVector)
      case None       => Expr(Data("<no primary constructor>"))
    }

  private def walkChain(method: Method, instance: Option[Expr_??], providedParams: Vector[Expr[Int]]): Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    implicit val StringType: Type[String] = this.StringType
    val instanceString = instance.map(_.plainPrint)
    def renderToString(m: Method): String = {
      val raw = removeAnsiColors(m.toString)
      instanceString.fold(raw)(i => raw.replace(i, "<instance>"))
    }
    var paramIdx = 0
    val steps = scala.collection.mutable.ListBuffer.empty[Data]
    def record(m: Method): Unit = {
      steps += Data.map(
        "step" -> Data(variantName(m)),
        "parameters" -> Data(renderParameterNames(m.parameters)),
        "totalParameters" -> Data(renderParameterNames(m.totalParameters)),
        "knownReturning" -> Data(m.knownReturning.map(_.plainPrint).getOrElse("<none>")),
        "toString" -> Data(renderToString(m))
      )
      ()
    }
    @scala.annotation.tailrec
    def loop(m: Method): Either[String, Expr_??] = m match {
      case oi: Method.OnInstance =>
        record(oi)
        loop(oi.applyUntyped(instance.getOrElse(Environment.reportErrorAndAbort("No instance provided")).asUntyped))
      case at: Method.ApplyTypes =>
        record(at)
        loop(at.apply(at.typeParams.flatten.map(tp => tp -> Type.of[Int].as_??).toMap))
      case av: Method.ApplyValues =>
        record(av)
        loop(av.apply(av.parameters.flatten.flatMap { case (pName, param) =>
          val tpe = param.byNameUnderlying.getOrElse(param.tpe)
          import tpe.Underlying
          if (Underlying <:< Type.of[Int] && paramIdx < providedParams.size) {
            paramIdx += 1
            Some(pName -> providedParams(paramIdx - 1).as_??)
          } else if (param.hasDefault) None
          else if (Underlying <:< Type.of[String]) Some(pName -> Expr("s").as_??)
          else Environment.reportErrorAndAbort(s"Unsupported parameter $pName: ${tpe.plainPrint}")
        }.toMap))
      case r: Method.Result[?] =>
        record(r)
        import r.Returned
        r.build().map(_.as_??)
    }
    // Builder failures that throw instead of returning `Left` are captured, so that the test can show them without
    // aborting the whole test file's compilation.
    val result =
      try loop(method)
      catch {
        case e: HearthRequirementError      => Left(s"THROWN: ${removeAnsiColors(e.description).linesIterator.next()}")
        case scala.util.control.NonFatal(e) =>
          Left(s"THROWN: ${e.getClass.getName}: ${String.valueOf(e.getMessage).linesIterator.next()}")
      }
    val stepsExpr = Expr(Data.list(steps.toList*))
    result match {
      case Right(expr) =>
        val resultExpr = runtimeToString(expr)
        Expr.quote(Data.map("steps" -> Expr.splice(stepsExpr), "result" -> Expr.splice(resultExpr)))
      case Left(error) =>
        val errorExpr = Expr(Data(s"FAILED: $error"))
        Expr.quote(Data.map("steps" -> Expr.splice(stepsExpr), "result" -> Expr.splice(errorExpr)))
    }
  }

  private def runtimeToString(expr: Expr_??): Expr[Data] = {
    import expr.Underlying as R
    Expr.quote(Data(Expr.splice(expr.value.asInstanceOf[Expr[R]]).toString))
  }

  def testBuildWithWrongArgumentTypes[A: Type](instance: Expr[A])(methodName: Expr[String]): Expr[Data] = {
    implicit val StringType: Type[String] = this.StringType
    val method = namedMethods[A](methodName) match {
      case method :: Nil => method
      case _             => Environment.reportErrorAndAbort(s"Method ${literalName(methodName)} is not unique")
    }
    val result =
      try
        method.fold(
          onInstance = _ => instance.as_??,
          onTypes = _ => Map.empty,
          onValues = av => av.parameters.flatten.map { case (pName, _) => pName -> Expr("wrong").as_?? }.toMap
        ) match {
          case Right(expr) => s"UNEXPECTED SUCCESS: ${expr.plainPrint}"
          case Left(error) => s"LEFT: ${removeAnsiColors(error)}"
        }
      catch {
        case e: HearthRequirementError      => s"REQUIREMENT FAILED: ${removeAnsiColors(e.description)}"
        case scala.util.control.NonFatal(e) => s"THROWN: ${e.getClass.getName}"
      }
    Expr(Data(result))
  }

  def testFoldFEffects[A: Type](instance: Expr[A])(methodName: Expr[String])(params: VarArgs[Int]): Expr[Data] = {
    implicit val IntType: Type[Int] = this.IntType
    type EitherString[X] = Either[String, X]
    val method = namedMethods[A](methodName) match {
      case method :: Nil => method
      case _             => Environment.reportErrorAndAbort(s"Method ${literalName(methodName)} is not unique")
    }
    val provided = params.toVector
    def arguments(av: Method.ApplyValues): Option[Arguments] = {
      val pairs = av.parameters.flatten.toList.zipWithIndex.map { case ((pName, param), idx) =>
        import param.tpe.Underlying
        if (Underlying <:< Type.of[Int]) provided.lift(idx).map(pName -> _.as_??) else None
      }
      if (pairs.forall(_.isDefined)) Some(pairs.flatten.toMap) else None
    }
    val viaEither = method.foldF[EitherString](
      onInstance = _ => Right(instance.as_??),
      onTypes = _ => Right(Map.empty),
      onValues = av => arguments(av).toRight("not enough Int arguments")
    )
    val viaTry = method.foldF[scala.util.Try](
      onInstance = _ => scala.util.Success(instance.as_??),
      onTypes = _ => scala.util.Success(Map.empty),
      onValues = av =>
        arguments(av).fold[scala.util.Try[Arguments]](scala.util.Failure(new Exception("not enough Int arguments")))(
          scala.util.Success(_)
        )
    )
    val eitherExpr: Expr[Data] = viaEither match {
      case Right(Right(expr)) => runtimeToString(expr)
      case Right(Left(error)) => Expr(Data(s"BUILD FAILED: $error"))
      case Left(error)        => Expr(Data(s"EFFECT FAILED: $error"))
    }
    val tryExpr: Expr[Data] = viaTry match {
      case scala.util.Success(Right(expr)) => runtimeToString(expr)
      case scala.util.Success(Left(error)) => Expr(Data(s"BUILD FAILED: $error"))
      case scala.util.Failure(error)       => Expr(Data(s"EFFECT FAILED: ${error.getMessage}"))
    }
    Expr.quote(Data.map("Either" -> Expr.splice(eitherExpr), "Try" -> Expr.splice(tryExpr)))
  }

  def testMethodListings[A: Type]: Expr[Data] = {
    def key(methods: List[Method]): List[String] = methods.map(m => m.plainPrint)
    val methods = Type[A].methods
    val unsorted = Type[A].unsortedMethods
    val names = unsorted.map(_.name).distinct
    val isDeclaredOrCtorArg: Method => Boolean = m => m.isDeclared || m.isConstructorArgument
    Expr(
      Data.map(
        "declared names in order" -> Data.list(methods.filter(isDeclaredOrCtorArg).map(m => Data(m.name))*),
        "methods contain the same methods as unsortedMethods" -> Data(key(methods).sorted == key(unsorted).sorted),
        "methods == Method.sort(unsortedMethods)" -> Data(key(methods) == key(Method.sort(unsorted))),
        "Method.sort(unsorted.filter(p)) == methods.filter(p)" ->
          Data(key(Method.sort(unsorted.filter(isDeclaredOrCtorArg))) == key(methods.filter(isDeclaredOrCtorArg))),
        "Method.sortBy(pairs)(_._2) == Method.sort" ->
          Data(Method.sortBy(unsorted.map(m => m.name -> m))(_._2).map(_._2.plainPrint) == key(methods)),
        "unsortedMethodsNamed(n) == unsortedMethods.filter(_.name == n)" -> Data(names.forall { n =>
          key(Type[A].unsortedMethodsNamed(n)) == key(unsorted.filter(_.name == n)) &&
          key(Method.unsortedMethodsNamed[A](n)) == key(unsorted.filter(_.name == n))
        }),
        "Method.methodsOf == Type.methods" -> Data(key(Method.methodsOf[A]) == key(methods)),
        "Method.unsortedMethodsOf == Type.unsortedMethods" -> Data(key(Method.unsortedMethodsOf[A]) == key(unsorted)),
        "Method.constructorsOf == Type.constructors" ->
          Data(key(Method.constructorsOf[A]) == key(Type[A].constructors)),
        "Method.primaryConstructorOf == Type.primaryConstructor" ->
          Data(key(Method.primaryConstructorOf[A].toList) == key(Type[A].primaryConstructor.toList))
      )
    )
  }

  def testHasMethodsNamed[A: Type](names: VarArgs[String]): Expr[Data] = {
    val literals = names.toIterable.map {
      case Expr(name) => name
      case other      => Environment.reportErrorAndAbort(s"Names must be string literals, got ${other.prettyPrint}")
    }.toList
    Expr(Data.map(literals.map(n => n -> Data(Type[A].unsortedMethodsNamed(n).nonEmpty))*))
  }

  def testGenericMethodChain[A: Type, R: Type, T: Type](instance: Expr[A])(methodName: Expr[String]): Expr[Data] =
    genericMethodChain[A, R](instance, methodName, Type[T].as_??)

  def testGenericMethodChainApplyingConstructorOf[A: Type, R: Type, T: Type](instance: Expr[A])(
      methodName: Expr[String]
  ): Expr[Data] =
    genericMethodChain[A, R](instance, methodName, UntypedType.typeConstructor(UntypedType.fromTyped[T]).as_??)

  /** On Scala 3 `Type.Ctor1.of[SomeF].asUntyped` is an eta-expanded type lambda, not the bare constructor. */
  def testGenericMethodChainApplyingCtor1Of[A: Type, R: Type](instance: Expr[A])(methodName: Expr[String]): Expr[Data] =
    genericMethodChain[A, R](instance, methodName, Type.Ctor1.of[examples.genericmethods.SomeF].asUntyped.as_??)

  private def genericMethodChain[A: Type, R: Type](
      instance: Expr[A],
      methodName: Expr[String],
      typeArg: ??
  ): Expr[Data] = {
    import examples.genericmethods.Config
    def renderType(t: ??): String = if (t.Underlying =:= Type[R]) "=:= R" else t.plainPrint
    def renderExpectations(m: Method): Data = Data.list(m.expectations.map {
      case MethodExpectation.NeedsInstance   => Data("NeedsInstance")
      case MethodExpectation.NeedsTypes(tps) =>
        Data(tps.map(_.map(_.name).mkString(", ")).mkString("NeedsTypes[", "][", "]"))
      case MethodExpectation.NeedsValues(ps) =>
        Data(
          ps.map(_.map { case (n, p) => s"$n: ${p.tpe.plainPrint}" }.mkString(", ")).mkString("NeedsValues(", ")(", ")")
        )
    }*)
    def renderStep(name: String, m: Method): Data = Data.map(
      "step" -> Data(name),
      "knownReturning" -> Data(m.knownReturning.map(renderType).getOrElse("<none>")),
      "expectations" -> renderExpectations(m)
    )

    val name = Expr.unapply(methodName).getOrElse(Environment.reportErrorAndAbort("method name must be a literal"))
    val method = Type[A].methods.find(_.name == name).getOrElse(Environment.reportErrorAndAbort(s"no method $name"))
    implicit val ConfigType: Type[Config] = Type.of[Config]
    implicit val StringType: Type[String] = Type.of[String]

    val steps = scala.collection.mutable.ListBuffer.empty[Data]
    @scala.annotation.tailrec
    def loop(m: Method): Unit = m match {
      case oi: Method.OnInstance =>
        steps += renderStep("OnInstance", oi)
        loop(oi.apply(instance.asInstanceOf[Expr[oi.Instance]]))
      case at: Method.ApplyTypes =>
        steps += renderStep("ApplyTypes", at)
        loop(at.apply(at.typeParams.flatten.map(tp => tp -> typeArg).toMap))
      case av: Method.ApplyValues =>
        steps += renderStep("ApplyValues", av)
        loop(av.apply(av.parameters.flatten.map { case (n, p) =>
          import p.tpe.Underlying
          val value: Expr_?? =
            if (Underlying =:= Type.of[Config]) Expr.quote(new Config).as_??
            else
              Expr
                .summonImplicitByType(p.tpe.asUntyped)
                .map(UntypedExpr.as_??(_))
                .getOrElse(Environment.reportErrorAndAbort(s"cannot summon ${p.tpe.plainPrint}"))
          n -> value
        }.toMap))
      case r: Method.Result[?] =>
        import r.Returned
        steps += renderStep("Result", r)
        steps += Data.map(
          "Result.Returned" -> Data(renderType(Type[r.Returned].as_??)),
          // the danger: if `Returned` is `Nothing`, it conforms to ANY expected result type
          "Returned <:< String" -> Data(Type[r.Returned] <:< Type.of[String]),
          "built" -> Data(r.build() match {
            case Right(e) => s"built: ${renderType(e.tpe.as_??)}"
            case Left(e)  => s"FAILED: $e"
          })
        )
    }
    loop(method)
    Expr(Data.list(steps.toList*))
  }
}
