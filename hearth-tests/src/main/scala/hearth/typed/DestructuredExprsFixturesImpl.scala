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

  /** Optics-style marker path recognition.
    *
    * A macro author building a Monocle/quicklens-style optic destructures the lambda and recognizes optics MARKERS
    * (`.each`, `.when[Sub]`/`.as[Sub]`, `.some`) purely from structure. Because the cross-platform DSL exposes these as
    * `implicit class` extension methods, a marker call appears as a `MethodCall` whose name is the marker and whose
    * receiver Instance is the implicit-wrapper `MethodCall` (`EachOps`/`WhenOps`/`SomeOps`); the wrapper's FIRST value
    * argument is the real underlying expression the optic focuses on. (The exact desugaring differs between Scala 2 and
    * Scala 3 — extra module-path refs, evidence terms, wrapper applied-kinds — but the marker call and the real
    * receiver are recoverable identically on both.) Type arguments of a prism (`.as[Dog]`) surface as an `AppliedTypes`
    * on the marker call itself. Plain field selects are `MethodCall`s with only a receiver Instance.
    *
    * This fixture walks the body, unwrapping the implicit-class plumbing, and emits a clean root -> leaf path where
    * each segment is classified `field` or `marker` (with the marker's recovered type arguments) — proving that optics
    * markers are cleanly recognizable regardless of platform desugaring.
    */
  def testMarkerPath[A: Type](expr: Expr[A]): Expr[Data] = {
    val markerWrappers = Map("each" -> "EachOps", "when" -> "WhenOps", "as" -> "WhenOps", "some" -> "SomeOps")

    def instanceOf(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] =
      mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value }

    def firstValueArg(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] =
      mc.applied.collectFirst { case av: DestructuredExpr.MethodCall.AppliedValues if av.args.nonEmpty => av.args.head }

    // Is `node` an implicit-wrapper MethodCall for the given marker? If so, return the real focused expression.
    def unwrapWrapper(node: DestructuredExpr, marker: String): Option[DestructuredExpr] = node match {
      case mc: DestructuredExpr.MethodCall if markerWrappers.get(marker).contains(mc.method.name) =>
        firstValueArg(mc)
      case _ => None
    }

    def segmentOf(mc: DestructuredExpr.MethodCall): Data = {
      val name = mc.method.name
      val isMarker = markerWrappers.contains(name)
      val typeArgs = mc.applied.collect { case at: DestructuredExpr.MethodCall.AppliedTypes => at.typeArgs }.flatten
      Data.map(
        "name" -> Data(name),
        "kind" -> Data(if (isMarker) "marker" else "field"),
        "typeArgs" -> Data(typeArgs.map(t => Data(t.plainPrint)))
      )
    }

    // Walk from leaf back to root, peeling one logical segment at a time, returning segments root -> leaf.
    def walk(node: DestructuredExpr, acc: List[Data]): Either[String, List[Data]] = node match {
      case _: DestructuredExpr.Lambda.ParamRef => Right(acc)
      case mc: DestructuredExpr.MethodCall     =>
        val name = mc.method.name
        val seg = segmentOf(mc)
        val nextReceiver: Option[DestructuredExpr] =
          if (markerWrappers.contains(name))
            // For a marker call, the true receiver is hidden inside the implicit wrapper held as the marker's Instance.
            instanceOf(mc).flatMap(unwrapWrapper(_, name))
          else
            instanceOf(mc)
        nextReceiver match {
          case Some(r) => walk(r, seg :: acc)
          case None    => Right(seg :: acc)
        }
      case other => Left(s"Unexpected node in path: ${other.plainPrint}")
    }

    DestructuredExpr.extractLambda(expr) match {
      case Right(info) =>
        walk(info.body, Nil) match {
          case Right(segments) => Expr(Data.map("segments" -> Data(segments)))
          case Left(error)     => Expr(Data.map("error" -> Data(error)))
        }
      case Left(error) => Expr(Data.map("error" -> Data(error)))
    }
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
