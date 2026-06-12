package hearth
package typed

import hearth.data.Data

trait DestructuredExprsFixturesImpl { this: MacroCommons =>

  def testParseGeneral[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    Expr(renderNode(parsed))
  }

  def testParseFieldPath[A: Type, B: Type](lambda: Expr[A => B]): Expr[Data] =
    DestructuredExpr.extractFieldPath[A, B](lambda) match {
      case Right(fieldPath) =>
        Expr(
          Data.map(
            "fieldNames" -> Data(fieldPath.fieldNames.map(Data(_))),
            "depth" -> Data(fieldPath.depth),
            "plainPrint" -> Data(fieldPath.plainPrint),
            "rootType" -> Data(fieldPath.root.plainPrint),
            "leafType" -> Data(fieldPath.leafType.plainPrint),
            "segments" -> Data(fieldPath.segments.map { seg =>
              Data.map(
                "name" -> Data(seg.name),
                "sourceType" -> Data(seg.sourceType.plainPrint),
                "resultType" -> Data(seg.resultType.plainPrint),
                "methodName" -> Data(seg.method.name)
              )
            })
          )
        )
      case Left(error) =>
        Expr(Data.map("error" -> Data(error)))
    }

  def testParseLambda[A: Type](expr: Expr[A]): Expr[Data] =
    DestructuredExpr.extractLambda(expr) match {
      case Right(info) =>
        Expr(
          Data.map(
            "params" -> Data(info.params.map { p =>
              Data.map(
                "name" -> Data(p.name),
                "type" -> Data(p.tpe.plainPrint)
              )
            }),
            "body" -> renderNode(info.body)
          )
        )
      case Left(error) =>
        Expr(Data.map("error" -> Data(error)))
    }

  def testCollectMethodCalls[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    val names = parsed.collect { case mc: DestructuredExpr.MethodCall => mc.method.name }
    Expr(Data(names.map(Data(_))))
  }

  def testOutermostMethodCall[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    def findOutermost(e: DestructuredExpr): Option[DestructuredExpr.MethodCall] = e match {
      case mc: DestructuredExpr.MethodCall => Some(mc)
      case lam: DestructuredExpr.Lambda    => findOutermost(lam.body)
      case _                               => None
    }
    findOutermost(parsed) match {
      case Some(mc) =>
        Expr(
          Data.map(
            "methodName" -> Data(mc.method.name),
            "appliedKinds" -> Data(mc.applied.map {
              case _: DestructuredExpr.MethodCall.AppliedInstance => Data("Instance")
              case _: DestructuredExpr.MethodCall.AppliedTypes    => Data("Types")
              case _: DestructuredExpr.MethodCall.AppliedValues   => Data("Values")
            })
          )
        )
      case None =>
        Expr(Data.map("error" -> Data("No outermost MethodCall found")))
    }
  }

  def testParseDetailed[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    Expr(renderDetailed(parsed))
  }

  private def renderNode(parsed: DestructuredExpr): Data = {
    val nodeType = parsed match {
      case _: DestructuredExpr.MethodCall        => "MethodCall"
      case _: DestructuredExpr.Lambda            => "Lambda"
      case _: DestructuredExpr.Lambda.ParamRef   => "ParamRef"
      case _: DestructuredExpr.Literal           => "Literal"
      case _: DestructuredExpr.Singleton         => "Singleton"
      case _: DestructuredExpr.Block             => "Block"
      case _: DestructuredExpr.Varargs           => "Varargs"
      case _: DestructuredExpr.NonDestructurable => "NonDestructurable"
    }
    val extra: List[(String, Data)] = parsed match {
      case mc: DestructuredExpr.MethodCall =>
        List(
          "methodName" -> Data(mc.method.name),
          "appliedCount" -> Data(mc.applied.size)
        )
      case ref: DestructuredExpr.Lambda.ParamRef =>
        List("paramName" -> Data(ref.param.name))
      case lit: DestructuredExpr.Literal =>
        List("value" -> Data(lit.plainPrint))
      case _ => Nil
    }
    Data.map((List("nodeType" -> Data(nodeType), "plainPrint" -> Data(parsed.plainPrint)) ++ extra)*)
  }

  private def renderDetailed(parsed: DestructuredExpr): Data = parsed match {
    case mc: DestructuredExpr.MethodCall =>
      Data.map(
        "nodeType" -> Data("MethodCall"),
        "methodName" -> Data(mc.method.name),
        "returnType" -> Data(mc.tpe.plainPrint),
        "applied" -> Data(mc.applied.map {
          case ai: DestructuredExpr.MethodCall.AppliedInstance =>
            Data.map("kind" -> Data("Instance"), "value" -> renderDetailed(ai.value))
          case at: DestructuredExpr.MethodCall.AppliedTypes =>
            Data.map("kind" -> Data("Types"), "typeArgs" -> Data(at.typeArgs.map(t => Data(t.plainPrint))))
          case av: DestructuredExpr.MethodCall.AppliedValues =>
            Data.map("kind" -> Data("Values"), "args" -> Data(av.args.map(renderDetailed)))
        })
      )
    case lam: DestructuredExpr.Lambda =>
      Data.map(
        "nodeType" -> Data("Lambda"),
        "params" -> Data(lam.params.map(p => Data.map("name" -> Data(p.name), "type" -> Data(p.tpe.plainPrint)))),
        "body" -> renderDetailed(lam.body)
      )
    case ref: DestructuredExpr.Lambda.ParamRef =>
      Data.map("nodeType" -> Data("ParamRef"), "paramName" -> Data(ref.param.name))
    case lit: DestructuredExpr.Literal =>
      Data.map("nodeType" -> Data("Literal"), "value" -> Data(lit.plainPrint))
    case s: DestructuredExpr.Singleton =>
      Data.map("nodeType" -> Data("Singleton"), "name" -> Data(s.name))
    case b: DestructuredExpr.Block =>
      Data.map(
        "nodeType" -> Data("Block"),
        "statements" -> Data(b.statements.map(renderDetailed)),
        "result" -> renderDetailed(b.result)
      )
    case va: DestructuredExpr.Varargs =>
      Data.map(
        "nodeType" -> Data("Varargs"),
        "type" -> Data(va.tpe.plainPrint),
        "elements" -> Data(va.elements.map(renderDetailed))
      )
    case nd: DestructuredExpr.NonDestructurable =>
      Data.map("nodeType" -> Data("NonDestructurable"), "description" -> Data(nd.description))
  }
}
