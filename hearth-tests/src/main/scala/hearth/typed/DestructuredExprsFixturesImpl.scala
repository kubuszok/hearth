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
                "type" -> Data(p.tpe.plainPrint),
                "declaredType" -> Data(p.declaredTpe.plainPrint)
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

  /** Recognizes optics markers (`.each`, `.when[Sub]`/`.as[Sub]`, `.some`) the way a Monocle/quicklens-style macro
    * would. To be cross-platform the DSL exposes them as `implicit class` extension methods, so a marker call's
    * receiver is the implicit-wrapper `MethodCall` (`EachOps`/`WhenOps`/`SomeOps`), and the wrapper's FIRST value
    * argument is the real focused expression. The desugaring around it differs between Scala 2 and Scala 3 (extra
    * module-path refs, evidence terms, wrapper applied-kinds), yet the path has to come out the same on both.
    */
  def testMarkerPath[A: Type](expr: Expr[A]): Expr[Data] = {
    val markerWrappers = Map("each" -> "EachOps", "when" -> "WhenOps", "as" -> "WhenOps", "some" -> "SomeOps")

    def instanceOf(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] =
      mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value }

    def firstValueArg(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] =
      mc.applied.collectFirst { case av: DestructuredExpr.MethodCall.AppliedValues if av.args.nonEmpty => av.args.head }

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

  /** Renders local bindings (val definitions, references, lambda params) with identity-based ids: the same id means the
    * very same `Binding` instance (`eq`), so the test can verify definition/use linking and shadowing.
    */
  def testParseBindings[A: Type](expr: Expr[A]): Expr[Data] = {
    val ids = scala.collection.mutable.ListBuffer.empty[DestructuredExpr.Binding]
    def idOf(binding: DestructuredExpr.Binding): Data = {
      val existing = ids.indexWhere(_ eq binding)
      if (existing >= 0) Data(existing)
      else {
        ids += binding
        Data(ids.size - 1)
      }
    }
    def render(node: DestructuredExpr): Data = node match {
      case vd: DestructuredExpr.ValDefinition =>
        val b = vd.binding
        Data.map(
          "node" -> Data("ValDefinition"),
          "binding" -> idOf(b),
          "name" -> Data(b.name),
          "type" -> Data(b.tpe.plainPrint),
          "flags" -> Data(
            List(
              "mutable" -> b.isMutable,
              "lazy" -> b.isLazy,
              "implicit" -> b.isImplicit,
              "external" -> b.isExternal
            ).collect { case (flag, true) => Data(flag) }
          ),
          "hasPosition" -> Data(vd.position.isDefined),
          "rhs" -> render(vd.rhs)
        )
      case ref: DestructuredExpr.LocalReference =>
        Data.map(
          "node" -> Data("LocalReference"),
          "binding" -> idOf(ref.binding),
          "name" -> Data(ref.binding.name),
          "external" -> Data(ref.binding.isExternal)
        )
      case ref: DestructuredExpr.Lambda.ParamRef =>
        Data.map("node" -> Data("ParamRef"), "binding" -> idOf(ref.param), "name" -> Data(ref.param.name))
      case imp: DestructuredExpr.Import =>
        Data.map(
          "node" -> Data("Import"),
          "qualifier" -> render(imp.qualifier),
          "selectors" -> Data(imp.selectors.map(Data(_)))
        )
      case ld: DestructuredExpr.LocalDefinition =>
        Data.map("node" -> Data("LocalDefinition"), "kind" -> Data(ld.kind), "name" -> Data(ld.name))
      case b: DestructuredExpr.Block =>
        Data.map(
          "node" -> Data("Block"),
          "statements" -> Data(b.statements.map(render)),
          "result" -> render(b.result)
        )
      case lam: DestructuredExpr.Lambda =>
        Data.map(
          "node" -> Data("Lambda"),
          "params" -> Data(lam.params.map(p => Data.map("binding" -> idOf(p), "name" -> Data(p.name)))),
          "body" -> render(lam.body)
        )
      case mc: DestructuredExpr.MethodCall =>
        Data.map(
          "node" -> Data("MethodCall"),
          "name" -> Data(mc.method.name),
          "receiver" -> mc.receiver.fold(Data("<none>"))(render),
          "args" -> Data(mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues =>
            av.args.map(render)
          }.flatten)
        )
      case lit: DestructuredExpr.Literal => Data.map("node" -> Data("Literal"), "value" -> Data(lit.plainPrint))
      case other                         => Data.map("node" -> Data("Other"), "plainPrint" -> Data(other.plainPrint))
    }
    Expr(render(DestructuredExpr.parse(expr)))
  }

  /** Reports, for a lambda: its unused parameters and how many references each parameter and each block-local val has
    * anywhere in the body - including inside sub-trees that are `NonDestructurable` (pattern matches, etc.).
    */
  def testFindReferences[A: Type](expr: Expr[A]): Expr[Data] =
    DestructuredExpr.parse(expr) match {
      case lam: DestructuredExpr.Lambda =>
        val locals = lam.body.collect { case vd: DestructuredExpr.ValDefinition => vd.binding }
        val bindings: List[DestructuredExpr.Binding] = lam.params ++ locals
        val references = lam.body.findReferences(bindings)
        Expr(
          Data.map(
            "unusedParams" -> Data(lam.unusedParams.map(p => Data(p.name))),
            "referenceCounts" -> Data.map(bindings.map { binding =>
              binding.name -> Data(references.count(_.binding eq binding))
            }*),
            "allReferencesHavePositions" -> Data(references.forall(_.position.isDefined)),
            "firstParamReferenced" -> Data(lam.body.references(lam.params.head))
          )
        )
      case other => Expr(Data.map("error" -> Data(s"Expected a lambda, got ${other.plainPrint}")))
    }

  def testPlainPrints[A: Type](expr: Expr[A]): Expr[Data] =
    Expr(Data(DestructuredExpr.parse(expr).collect { case node => Data(node.plainPrint) }))

  def testStatements[A: Type](expr: Expr[A]): Expr[Data] = DestructuredExpr.parse(expr) match {
    case block: DestructuredExpr.Block =>
      Expr(Data(block.statements.map { statement =>
        val rebuilt = UntypedExpr.as_??(statement.toUntypedExpr)
        Data.map(
          "plainPrint" -> Data(statement.plainPrint),
          "hasPosition" -> Data(statement.position.isDefined),
          "rebuiltAsUnit" -> Data(rebuilt.Underlying =:= Type.of[Unit])
        )
      }))
    case other => Expr(Data.map("error" -> Data(s"Expected a block, got ${other.plainPrint}")))
  }

  def testSkipContextualWrappers[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    def describe(node: DestructuredExpr): Data = node match {
      case lam: DestructuredExpr.Lambda =>
        Data.map(
          "node" -> Data("Lambda"),
          "contextual" -> Data(lam.isContextual),
          // contextual parameters usually have compiler-generated (unstable) names
          "params" -> Data(lam.params.map(p => Data(if (lam.isContextual) "<contextual>" else p.name)))
        )
      case other => Data.map("node" -> Data(other.getClass.getSimpleName))
    }
    Expr(
      Data.map(
        "parsed" -> describe(parsed),
        "skipped" -> describe(DestructuredExpr.skipContextualWrappers(parsed))
      )
    )
  }

  /** The same code has to read both `implicit class` (Scala 2 and 3) and `extension` (Scala 3) DSL methods. */
  def testReceiverChain[A: Type](expr: Expr[A]): Expr[Data] = {
    def walk(node: DestructuredExpr, acc: List[Data]): Data = node match {
      case _: DestructuredExpr.Lambda.ParamRef => Data(acc)
      case mc: DestructuredExpr.MethodCall     =>
        mc.receiver match {
          case Some(receiver) => walk(receiver, Data(mc.method.name) :: acc)
          case None           => Data.map("error" -> Data(s"no receiver for ${mc.plainPrint}"))
        }
      case other => Data.map("error" -> Data(s"unexpected ${other.plainPrint}"))
    }
    DestructuredExpr.extractLambda(expr) match {
      case Right(info) => Expr(walk(info.body, Nil))
      case Left(error) => Expr(Data.map("error" -> Data(error)))
    }
  }

  /** Every node in `collect` (pre-order) order with its type, whether its rebuilt tree conforms to that type, and the
    * line of its position relative to the line of the root node.
    */
  def testNodeSummaries[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    val rootLine = parsed.position.map(_.line)
    Expr(Data(parsed.collect { case node =>
      val rebuilt = UntypedExpr.as_??(node.toUntypedExpr)
      Data.map(
        "node" -> Data(nodeKind(node)),
        "plainPrint" -> Data(node.plainPrint),
        "type" -> Data(node.tpe.plainPrint),
        "rebuiltConforms" -> Data(rebuilt.Underlying <:< node.tpe.Underlying),
        "line" -> relativeLine(node.position, rootLine)
      )
    }))
  }

  /** For a lambda: every reference (in the order `findReferences` returns them) to its parameters and to the vals
    * defined in its body, plus the `references`/`unusedParams` views of the same information.
    */
  def testReferenceOrder[A: Type](expr: Expr[A]): Expr[Data] =
    DestructuredExpr.parse(expr) match {
      case lam: DestructuredExpr.Lambda =>
        val locals = lam.body.collect { case vd: DestructuredExpr.ValDefinition => vd.binding }
        val bindings: List[DestructuredExpr.Binding] = lam.params ++ locals
        val rootLine = lam.position.map(_.line)
        Expr(
          Data.map(
            "references" -> Data(lam.body.findReferences(bindings).map { ref =>
              Data.map("name" -> Data(ref.binding.name), "line" -> relativeLine(ref.position, rootLine))
            }),
            "paramsReferenced" -> Data(lam.params.map(p => Data(lam.body.references(p)))),
            "unusedParams" -> Data(lam.unusedParams.map(p => Data(p.name))),
            "noBindingsGiveNoReferences" -> Data(lam.body.findReferences(Nil).isEmpty)
          )
        )
      case other => Expr(Data.map("error" -> Data(s"Expected a lambda, got ${other.plainPrint}")))
    }

  /** The `receiver` of the outermost method call (looking through a lambda). */
  def testReceiver[A: Type](expr: Expr[A]): Expr[Data] = {
    def outermost(node: DestructuredExpr): Option[DestructuredExpr.MethodCall] = node match {
      case mc: DestructuredExpr.MethodCall => Some(mc)
      case lam: DestructuredExpr.Lambda    => outermost(lam.body)
      case _                               => None
    }
    outermost(DestructuredExpr.parse(expr)) match {
      case Some(mc) =>
        Expr(
          Data.map(
            "method" -> Data(mc.method.name),
            "receiver" -> mc.receiver.fold(Data("<none>")) { r =>
              Data.map("node" -> Data(nodeKind(r)), "plainPrint" -> Data(r.plainPrint))
            }
          )
        )
      case None => Expr(Data.map("error" -> Data("No outermost MethodCall found")))
    }
  }

  /** Every distinct `LocalBinding` (by identity) defined or referenced in the expression, with all of its properties.
    */
  def testLocalBindings[A: Type](expr: Expr[A]): Expr[Data] = {
    val parsed = DestructuredExpr.parse(expr)
    val all = parsed.collect {
      case vd: DestructuredExpr.ValDefinition   => vd.binding
      case ref: DestructuredExpr.LocalReference => ref.binding
    }
    val distinct = all.foldLeft(List.empty[DestructuredExpr.LocalBinding]) { (acc, b) =>
      if (acc.exists(_ eq b)) acc else acc :+ b
    }
    Expr(Data(distinct.map { b =>
      Data.map(
        "name" -> Data(b.name),
        "type" -> Data(b.tpe.plainPrint),
        "flags" -> Data(
          List(
            "mutable" -> b.isMutable,
            "lazy" -> b.isLazy,
            "implicit" -> b.isImplicit,
            "synthetic" -> b.isSynthetic,
            "external" -> b.isExternal
          ).collect { case (flag, true) => Data(flag) }
        ),
        "hasPosition" -> Data(b.position.isDefined)
      )
    }))
  }

  /** For every method call in the expression: the kinds of its `applied` steps next to the kinds of its method's
    * `expectations`, which they should mirror one-to-one.
    */
  def testAppliedMirrorsExpectations[A: Type](expr: Expr[A]): Expr[Data] =
    Expr(Data(DestructuredExpr.parse(expr).collect { case mc: DestructuredExpr.MethodCall =>
      Data.map(
        "method" -> Data(mc.method.name),
        "applied" -> Data(mc.applied.map {
          case _: DestructuredExpr.MethodCall.AppliedInstance => Data("Instance")
          case _: DestructuredExpr.MethodCall.AppliedTypes    => Data("Types")
          case _: DestructuredExpr.MethodCall.AppliedValues   => Data("Values")
        }),
        "expectations" -> Data(mc.method.expectations.map {
          case MethodExpectation.NeedsInstance  => Data("Instance")
          case _: MethodExpectation.NeedsTypes  => Data("Types")
          case _: MethodExpectation.NeedsValues => Data("Values")
        })
      )
    }))

  private def relativeLine(position: Option[Position], rootLine: Option[Int]): Data =
    (position, rootLine) match {
      case (Some(pos), Some(root)) => Data(pos.line - root)
      case (Some(_), None)         => Data("<root without position>")
      case (None, _)               => Data("<none>")
    }

  private def nodeKind(node: DestructuredExpr): String = node match {
    case _: DestructuredExpr.MethodCall        => "MethodCall"
    case _: DestructuredExpr.Lambda            => "Lambda"
    case _: DestructuredExpr.Lambda.ParamRef   => "ParamRef"
    case _: DestructuredExpr.Literal           => "Literal"
    case _: DestructuredExpr.Singleton         => "Singleton"
    case _: DestructuredExpr.Block             => "Block"
    case _: DestructuredExpr.Varargs           => "Varargs"
    case _: DestructuredExpr.ValDefinition     => "ValDefinition"
    case _: DestructuredExpr.LocalReference    => "LocalReference"
    case _: DestructuredExpr.Import            => "Import"
    case _: DestructuredExpr.LocalDefinition   => "LocalDefinition"
    case _: DestructuredExpr.NonDestructurable => "NonDestructurable"
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
      case _: DestructuredExpr.ValDefinition     => "ValDefinition"
      case _: DestructuredExpr.LocalReference    => "LocalReference"
      case _: DestructuredExpr.Import            => "Import"
      case _: DestructuredExpr.LocalDefinition   => "LocalDefinition"
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
    case vd: DestructuredExpr.ValDefinition =>
      Data.map("nodeType" -> Data("ValDefinition"), "name" -> Data(vd.binding.name), "rhs" -> renderDetailed(vd.rhs))
    case ref: DestructuredExpr.LocalReference =>
      Data.map("nodeType" -> Data("LocalReference"), "name" -> Data(ref.binding.name))
    case imp: DestructuredExpr.Import =>
      Data.map("nodeType" -> Data("Import"), "plainPrint" -> Data(imp.plainPrint))
    case ld: DestructuredExpr.LocalDefinition =>
      Data.map("nodeType" -> Data("LocalDefinition"), "kind" -> Data(ld.kind), "name" -> Data(ld.name))
    case nd: DestructuredExpr.NonDestructurable =>
      Data.map("nodeType" -> Data("NonDestructurable"), "description" -> Data(nd.description))
  }
}
