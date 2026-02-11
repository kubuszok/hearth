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

  private def renderConstructor[A: Type](constructor: Method.NoInstance[A]): Data =
    renderParameters(constructor.parameters)

  def testMethodsExtraction[A: Type](excluding: VarArgs[String]): Expr[Data] = {
    val excluded = excluding.toIterable.map {
      case Expr(excluding) => excluding
      case unmatched       =>
        Environment.reportErrorAndAbort(
          s"Excluded methods names must be a sequence of strings literals, got ${unmatched.prettyPrint}"
        )
    }.toSet
    val methods = Type[A].methods
    val filtered = methods.filterNot(m => excluded(m.value.name))
    Expr(renderMethods(filtered))
  }

  def testMethodDefaults[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(name) =>
      val methods = Type[A].methods.filter(_.value.name == name)
      val rendered = methods.map { m =>
        import m.value as method
        val paramsData = method.parameters.toList.flatMap { params =>
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
      val method = Type[A].methods.filter(_.value.name == methodName)
      Expr(renderMethods(method))
    case _ =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
  }

  private def renderMethods[A](methods: List[Method.Of[A]]): Data =
    Data(
      methods
        .groupMapReduce(_.value.name) { m =>
          import m.value as method
          val signature = method.parameters
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

  private val IntType: Type[Int] = Type.of[Int]

  def testCallNoInstanceIntMethod[A: Type](methodName: Expr[String])(params: VarArgs[Int]): Expr[Int] = {
    implicit val IntType: Type[Int] = this.IntType
    val name = Expr
      .unapply(methodName)
      .getOrElse(
        Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
      )
    val method: Method[A, Int] = Type[A].methods.filter(_.value.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil =>
        import method.Underlying as Returned
        if (!(Returned <:< Type.of[Int])) Environment.reportErrorAndAbort(s"Method $name returns not an Int")
        else method.value.asInstanceOf[Method[A, Int]]
      case _ => Environment.reportErrorAndAbort(s"Method $name is not unique")
    }
    method match {
      case noInstance: Method.NoInstance[Int] @unchecked =>
        val providedParams = params.toVector
        val arguments = noInstance.parameters.flatten.zipWithIndex.flatMap { case ((name, param), index) =>
          providedParams.lift(index) match {
            case _ if !(param.tpe.Underlying <:< Type.of[Int]) =>
              Environment.reportErrorAndAbort(s"Parameter $name has wrong type: ${param.tpe.plainPrint} is not an Int")
            case Some(value)              => Some(name -> value.as_??)
            case None if param.hasDefault => None
            case _ => Environment.reportErrorAndAbort(s"Missing parameter for $name (not default value as well)")
          }
        }.toMap
        noInstance.apply(arguments) match {
          case Right(result) => result
          case Left(error)   => Environment.reportErrorAndAbort(s"Failed to call method $name: $error")
        }
      case _: Method.OfInstance[A, Int] @unchecked =>
        Environment.reportErrorAndAbort(s"Method $name is not a no-instance method")
      case unsupported: Method.Unsupported[A, Int] @unchecked =>
        Environment.reportErrorAndAbort(s"Method $name is unsupported: ${unsupported.reasonForUnsupported}")
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
    val method: Method[A, Int] = Type[A].methods.filter(_.value.name == name) match {
      case Nil           => Environment.reportErrorAndAbort(s"Method $name not found")
      case method :: Nil =>
        import method.Underlying as Returned
        if (!(Returned <:< Type.of[Int])) Environment.reportErrorAndAbort(s"Method $name returns not an Int")
        else method.value.asInstanceOf[Method[A, Int]]
      case _ => Environment.reportErrorAndAbort(s"Method $name is not unique")
    }
    method match {
      case _: Method.NoInstance[Int] @unchecked =>
        Environment.reportErrorAndAbort(s"Method $name is not an instance method")
      case ofInstance: Method.OfInstance[A, Int] @unchecked =>
        val providedParams = params.toVector
        val arguments = ofInstance.parameters.flatten.zipWithIndex.flatMap { case ((name, param), index) =>
          providedParams.lift(index) match {
            case _ if !(param.tpe.Underlying <:< Type.of[Int]) =>
              Environment.reportErrorAndAbort(s"Parameter $name has wrong type: ${param.tpe.plainPrint} is not an Int")
            case Some(value)              => Some(name -> value.as_??)
            case None if param.hasDefault => None
            case _ => Environment.reportErrorAndAbort(s"Missing parameter for $name (not default value as well)")
          }
        }.toMap
        ofInstance.apply(instance, arguments) match {
          case Right(result) => result
          case Left(error)   => Environment.reportErrorAndAbort(s"Failed to call method $name: $error")
        }
      case unsupported: Method.Unsupported[A, Int] @unchecked =>
        Environment.reportErrorAndAbort(s"Method $name is unsupported: ${unsupported.reasonForUnsupported}")
    }
  }
}
