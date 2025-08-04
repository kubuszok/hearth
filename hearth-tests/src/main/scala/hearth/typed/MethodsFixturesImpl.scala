package hearth
package typed

import hearth.testdata.{Data, DataSupports}

trait MethodsFixturesImpl { this: MacroCommons & DataSupports =>

  def testMethodsExtraction[A: Type]: Expr[Data] = Expr(
    Data(
      Method
        .methodsOf[A]
        .groupMapReduce(_.value.name) { m =>
          import m.value as method
          val signature = method.parameters
            .map { params =>
              params.map { case (_, p) => s"${p.tpe.shortName}" }.mkString("(", ", ", ")")
            }
            .mkString(" ")
          val props = Data(
            Map(
              "invocation" -> Data(method.untyped.invocation.toString),
              "hasTypeParameters" -> Data(method.untyped.hasTypeParameters),
              "position" -> Data(method.position.toString),
              // annotations // TODO
              "isVal" -> Data(method.isVal),
              "isVar" -> Data(method.isVar),
              "isLazy" -> Data(method.isLazy),
              "isDef" -> Data(method.isDef),
              "isInherited" -> Data(method.isInherited),
              "isImplicit" -> Data(method.isImplicit),
              "isAvailable(Everywhere)" -> Data(method.isAvailable(Everywhere)),
              "arity" -> Data(method.arity),
              "isNullary" -> Data(method.isNullary),
              "isUnary" -> Data(method.isUnary),
              "isBinary" -> Data(method.isBinary),
              // "isConstructorArgument" -> Data(method.isConstructorArgument), // TODO
              // "isCaseField" -> Data(method.isCaseField), // TODO
              "isScalaGetter" -> Data(method.isScalaGetter),
              "isScalaSetter" -> Data(method.isScalaSetter),
              "isScalaAccessor" -> Data(method.isScalaAccessor),
              "isJavaGetter" -> Data(method.isJavaGetter),
              "isJavaSetter" -> Data(method.isJavaSetter),
              "isJavaAccessor" -> Data(method.isJavaAccessor),
              "isAccessor" -> Data(method.isAccessor)
            )
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
  )
}
