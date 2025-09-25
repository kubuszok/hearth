package hearth
package typed

import hearth.data.Data

/** Fixtured for testing [[MethodsSpec]]. */
trait MethodsFixturesImpl { this: MacroCommons =>

  def testMethodsExtraction[A: Type](excluding: Seq[String]): Expr[Data] = {
    val methods = Type[A].methods
    val filtered = methods.filterNot(m => excluding.contains(m.value.name))
    Expr(renderMethods(filtered))
  }
  // Scala 2 varargs pass Seq[Expr[String]]
  def testMethodsExtractionS2Adapter[A: Type](excluding: Seq[Expr[String]]): Expr[Data] =
    testMethodsExtraction[A](
      excluding.map {
        case Expr(excluding) => excluding
        case unmatched       =>
          Environment.reportErrorAndAbort(
            s"Excluded methods names must be a sequence of strings literals, got ${unmatched.prettyPrint}"
          )
      }
    )
  // Scala 3 varargs pass Expr[Seq[String]]
  def testMethodsExtractionS3Adapter[A: Type](excluding: Expr[Seq[String]]): Expr[Data] = {
    implicit val StringType: Type[String] = mkStringType // Make visible for ExprCodec[Seq[String]]
    excluding match {
      case Expr(excluding) =>
        testMethodsExtraction[A](excluding)
      case _ =>
        Environment.reportErrorAndAbort(
          s"Excluded methods names must be a sequence of strings literals, got ${excluding.prettyPrint}"
        )
    }
  }

  def testMethodProperties[A: Type](methodName: Expr[String]): Expr[Data] = methodName match {
    case Expr(methodName) =>
      val method = Type[A].methods.filter(_.value.name == methodName)
      Expr(renderMethods(method))
    case _ =>
      Environment.reportErrorAndAbort(s"Method name must be a string literal, got ${methodName.prettyPrint}")
  }

  private def mkStringType: Type[String] = Type.of[String]

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
          // TODO: for now we're only printing file, since we didn't standardize how to print the position yet
          val position =
            method.position
              .flatMap(_.file)
              .map(_.toString)
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
}
