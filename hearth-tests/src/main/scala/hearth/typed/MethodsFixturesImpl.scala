package hearth
package typed

import hearth.data.Data

/** Fixtured for testing [[MethodsSpec]]. */
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
            "isAccessor" -> Data(method.isAccessor)
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
}
