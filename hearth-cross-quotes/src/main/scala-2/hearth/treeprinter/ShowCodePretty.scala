package hearth.treeprinter

import java.io.{PrintWriter, StringWriter}
import scala.reflect.internal.ModifierFlags.*

trait ShowCodePretty {
  val c: scala.reflect.macros.blackbox.Context
  val st: scala.reflect.internal.SymbolTable = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]

  import st.*
  import SyntaxHighlight.*

  /** Better implementation of [[scala.reflect.internal.Printers#showCode]] that supports syntax highlighting. */
  def showCodePretty(
      any: Any,
      highlight: SyntaxHighlight,
      printTypes: BooleanFlag = None,
      printIds: BooleanFlag = None,
      printOwners: BooleanFlag = None,
      printPositions: BooleanFlag = None,
      printRootPkg: Boolean = false
  ): String =
    render(
      any,
      highlight,
      new HighlighedCodePrinter(_, printRootPkg, _),
      printTypes,
      printIds,
      printOwners,
      printKinds = None,
      printMirrors = None,
      printPositions
    )

  // Copy-pasted from scala.reflect.internal.Printers
  private def render(
      what: Any,
      highlight: SyntaxHighlight,
      mkPrinter: (PrintWriter, SyntaxHighlight) => InternalTreePrinter,
      printTypes: BooleanFlag = None,
      printIds: BooleanFlag = None,
      printOwners: BooleanFlag = None,
      printKinds: BooleanFlag = None,
      printMirrors: BooleanFlag = None,
      printPositions: BooleanFlag = None
  ): String = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)
    val printer = mkPrinter(writer, highlight)

    printTypes.value.foreach(if (_) printer.withTypes else printer.withoutTypes)
    printIds.value.foreach(if (_) printer.withIds else printer.withoutIds)
    printOwners.value.foreach(if (_) printer.withOwners else printer.withoutOwners)
    printKinds.value.foreach(if (_) printer.withKinds else printer.withoutKinds)
    printMirrors.value.foreach(if (_) printer.withMirrors else printer.withoutMirrors)
    printPositions.value.foreach(if (_) printer.withPositions else printer.withoutPositions)

    printer.print(what)
    writer.flush()
    buffer.toString
  }
  private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
    case null | NoSymbol => orElse
    case sym             => f(sym)
  }
  private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

  /** Better implementation of [[scala.reflect.internal.Printers#CodePrinter]] that supports syntax highlighting. */
  class HighlighedCodePrinter(out: PrintWriter, printRootPkg: Boolean, syntaxHighlight: SyntaxHighlight)
      extends CodePrinter(out, printRootPkg) {
    import syntaxHighlight.*

    // Overriden from CodePrinter:

    // colors printed name
    override protected def printedName(name: Name, decoded: Boolean = true) =
      if (name.isTypeName) highlightTypeDef(super.printedName(name, decoded))
      else highlightValDef(super.printedName(name, decoded))

    // replaces CodePrinter with HighlighedCodePrinter
    override protected def resolveSelect(t: Tree): String = t match {
      // case for: 1) (if (a) b else c).meth1.meth2 or 2) 1 + 5 should be represented as (1).+(5)
      case Select(qual, name)
          if (name.isTermName && needsParentheses(qual)(insideLabelDef = false)) || isIntLitWithDecodedOp(qual, name) =>
        s"(${resolveSelect(qual)}).${printedName(name)}"
      case Select(qual, name) if name.isTermName => s"${resolveSelect(qual)}.${printedName(name)}"
      case Select(qual, name) if name.isTypeName =>
        s"${resolveSelect(qual)}#${blankForOperatorName(name)}%${printedName(name)}"
      case Ident(name) => printedName(name)
      case _           => render(t, syntaxHighlight, new HighlighedCodePrinter(_, printRootPkg, _))
    }

    override def printFlags(mods: Modifiers, primaryCtorParam: Boolean = false): Unit = {
      print(KeywordColor)
      super.printFlags(mods, primaryCtorParam)
      print(NoColor)
    }

    override def printModifiers(mods: Modifiers, primaryCtorParam: Boolean): Unit = {
      print(KeywordColor)
      super.printModifiers(mods, primaryCtorParam)
      print(NoColor)
    }

    override def printVParam(vd: ValDef, primaryCtorParam: Boolean): Unit = {
      printPosition(vd)
      printAnnotations(vd)
      val mutableOrOverride = vd.mods.isOverride || vd.mods.isMutable
      val hideCtorMods = vd.mods.isParamAccessor && vd.mods.isPrivateLocal && !mutableOrOverride
      val hideCaseCtorMods = vd.mods.isCaseAccessor && vd.mods.isPublic && !mutableOrOverride

      if (primaryCtorParam && !(hideCtorMods || hideCaseCtorMods)) {
        printModifiers(vd.mods, primaryCtorParam)
        print(highlightKeyword(if (vd.mods.isMutable) "var" else "val") + " ")
      }
      print(printedName(vd.name), blankForName(vd.name))
      printOpt(": ", vd.tpt)
      printOpt(" = ", vd.rhs)
    }

    // Overriden from TreePrinter:

    override protected def printPackageDef(tree: PackageDef, separator: String) = {
      val PackageDef(packaged, stats) = tree
      printAnnotations(tree)
      print(highlightKeyword("package") + " " + packaged); printColumn(stats, " {", separator, "}")
    }

    override protected def printValDef(tree: ValDef, resultName: => String)(
        printTypeSignature: => Unit
    )(printRhs: => Unit) = {
      val ValDef(mods, _, _, _) = tree
      printAnnotations(tree)
      printModifiers(tree, mods)
      print(highlightKeyword(if (mods.isMutable) "var" else "val") + " ", resultName)
      printTypeSignature
      printRhs
    }

    override protected def printDefDef(tree: DefDef, resultName: => String)(
        printTypeSignature: => Unit
    )(printRhs: => Unit) = {
      val DefDef(mods, _, tparams, vparamss, _, _) = tree
      printAnnotations(tree)
      printModifiers(tree, mods)
      print(highlightKeyword("def") + " " + resultName)
      printTypeParams(tparams)
      vparamss foreach { printValueParams(_) }
      printTypeSignature
      printRhs
    }

    override protected def printTypeDef(tree: TypeDef, resultName: => String) = {
      val TypeDef(mods, _, tparams, rhs) = tree
      if (mods hasFlag (PARAM | DEFERRED)) {
        printAnnotations(tree)
        printModifiers(tree, mods)
        print(highlightKeyword("type") + " ")
        printTParam(tree)
      } else {
        printAnnotations(tree)
        printModifiers(tree, mods)
        print(highlightKeyword("type") + " " + resultName)
        printTypeParams(tparams)
        printOpt(" = ", rhs)
      }
    }

    override protected def printImport(tree: Import, resSelect: => String) = {
      val Import(_, selectors) = tree

      def selectorToString(s: ImportSelector): String = {
        def selectorName(n: Name): String = if (s.isWildcard) nme.WILDCARD.decoded else quotedName(n)
        val from = selectorName(s.name)
        if (s.isRename || s.isMask) from + "=>" + selectorName(s.rename)
        else from
      }
      print(highlightKeyword("import") + " " + resSelect + ".")
      selectors match {
        case List(s) =>
          // If there is just one selector and it is not renaming or masking a name, no braces are needed
          if (!s.isRename && !s.isMask) print(selectorToString(s))
          else print("{", selectorToString(s), "}")
        // If there is more than one selector braces are always needed
        case many =>
          print(many.map(selectorToString).mkString("{", ", ", "}"))
      }
    }

    override protected def printCaseDef(tree: CaseDef) = {
      val CaseDef(pat, guard, body) = tree
      print(highlightKeyword("case") + " ")
      print(pat)
      printOpt(" " + highlightKeyword("if") + " ", guard)
      print(" => ", body)
    }

    override protected def printFunction(tree: Function)(printValueParams: => Unit) = {
      val Function(_, body) = tree
      print("(")
      printValueParams
      print(" => ", body, ")")
      if (printIds && tree.symbol != null)
        comment {
          print("#" + tree.symbol.id)
        }

      if (printOwners && tree.symbol != null)
        comment {
          print("@" + tree.symbol.owner.id)
        }
    }

    override protected def printSuper(tree: Super, resultName: => String, checkSymbol: Boolean = true) = {
      val Super(This(qual), mix) = tree: @unchecked
      if (qual.nonEmpty || (checkSymbol && tree.symbol != NoSymbol)) print(resultName + ".")
      print(highlightValDef("super")) // TODO: check how it's colored in Scala 3
      if (mix.nonEmpty) print(s"[$mix]")
      else if (settings.isDebug) tree.tpe match {
        case st: SuperType => print(s"[${st.supertpe}]") // TODO: check how it's colored in Scala 3
        case tp: Type      => print(s"[$tp]") // TODO: check how it's colored in Scala 3
        case _             =>
      }
    }

    override protected def printThis(tree: This, resultName: => String) = {
      val This(qual) = tree
      if (qual.nonEmpty) print(resultName + ".")
      print(highlightValDef("this") + " ")
    }

    private[this] var currentOwner: Symbol = NoSymbol
    private[this] var selectorType: Type = NoType

    @scala.annotation.nowarn
    override def printTree(tree: Tree) = {
      tree match {
        case EmptyTree =>
          print("/*empty*/")

        case cd @ ClassDef(mods, name, tparams, impl) =>
          printAnnotations(cd)
          printModifiers(tree, mods)
          val word =
            if (mods.isTrait) highlightKeyword("trait")
            else if (ifSym(tree, _.isModuleClass)) highlightKeyword("object")
            else highlightKeyword("class")

          print(word, " ", symName(tree, name))
          printTypeParams(tparams)
          print(if (mods.isDeferred) " <: " else (" " + highlightKeyword("extends") + " "), impl)

        case pd @ PackageDef(packaged, stats) =>
          printPackageDef(pd, ";")

        case md @ ModuleDef(mods, name, impl) =>
          printAnnotations(md)
          printModifiers(tree, mods)
          print(highlightKeyword("object") + " " + symName(tree, name), " " + highlightKeyword("extends") + " ", impl)

        case vd @ ValDef(mods, name, tp, rhs) =>
          // TODO: check for ValDef : <init> in a trait, to remove it in some cases
          // TODO: check if ValDef is synthetic, to remove it in some cases
          printValDef(vd, symName(tree, name))(printOpt(": ", tp)) {
            if (!mods.isDeferred) print(" = ", if (rhs.isEmpty) "_" else rhs)
          }

        case dd @ DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printDefDef(dd, symName(tree, name)) {
            // place space after symbolic def name (def !: Unit does not compile)
            if (tparams.isEmpty && vparamss.isEmpty) printOpt(blankForName(name.encodedName) + ": ", tp)
            else printOpt(": ", tp)
          }(printOpt(" = ", rhs))

        case td @ TypeDef(mods, name, tparams, rhs) =>
          printTypeDef(td, symName(tree, name))

        case LabelDef(name, params, rhs) =>
          print(symName(tree, name)); printLabelParams(params); printBlock(rhs)

        case imp @ Import(expr, _) =>
          printImport(imp, backquotedPath(expr))

        case Template(parents, self, body) =>
          val currentOwner1 = currentOwner
          if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
          printRow(parents, " " + highlightKeyword("with") + " ")
          if (body.nonEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name); printOpt(": ", self.tpt); print(" => ")
            } else if (self.tpt.nonEmpty) {
              print(" { _ : ", self.tpt, " => ")
            } else {
              print(" {")
            }
            printColumn(body, "", ";", "}")
          }
          currentOwner = currentOwner1

        case Block(stats, expr) =>
          printBlock(stats, expr)

        case Match(selector, cases) =>
          val selectorType1 = selectorType
          selectorType = selector.tpe
          print(selector); printColumn(cases, " " + highlightKeyword("match") + " {", "", "}")
          selectorType = selectorType1

        case cd @ CaseDef(pat, guard, body) =>
          printCaseDef(cd)

        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          print("(", elem, ")*")

        case Bind(name, t) =>
          print("(", symName(tree, name), " @ ", t, ")")

        case UnApply(fun, args) =>
          // TODO: figure out how to print this
          print(fun, " <unapply> "); printRow(args, "(", ", ", ")")

        case ArrayValue(elemtpt, trees) =>
          print("Array[", elemtpt); printRow(trees, "]{", ", ", "}") // TODO

        case f @ Function(vparams, body) =>
          printFunction(f)(printValueParams(vparams))

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case NamedArg(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ")"); indent(); println() // TODO
          print(thenp); undent()
          if (elsep.nonEmpty) {
            println(); print("else"); indent(); println(); print(elsep); undent() // TODO
          }

        case Return(expr) =>
          print("return ", expr) // TODO

        case Try(block, catches, finalizer) =>
          print("try "); printBlock(block) // TODO
          if (catches.nonEmpty) printColumn(catches, " catch {", "", "}") // TODO
          printOpt(" finally ", finalizer) // TODO

        case Throw(expr) =>
          print("throw ", expr) // TODO

        case New(tpe) =>
          print("new ", tpe) // TODO

        case Typed(expr, tp) =>
          print("(", expr, ": ", tp, ")")

        case TypeApply(fun, targs) =>
          print(fun); printRow(targs, "[", ", ", "]")

        case Apply(fun, vargs) =>
          print(fun); printRow(vargs, "(", ", ", ")")

        case ApplyDynamic(qual, vargs) =>
          // TODO: figure out how to print this
          print("<apply-dynamic>(", qual, "#", tree.symbol.nameString) // TODO
          printRow(vargs, ", (", ", ", "))")

        case st @ Super(This(qual), mix) =>
          printSuper(st, symName(tree, qual))

        case Super(qual, mix) =>
          print(qual, ".super") // TODO
          if (mix.nonEmpty)
            print("[" + mix + "]")

        case th @ This(qual) =>
          printThis(th, symName(tree, qual))

        case Select(qual: New, name) if !settings.isDebug =>
          print(qual)

        case Select(qualifier, name) =>
          print(backquotedPath(qualifier), ".", symName(tree, name))

        case id @ Ident(name) =>
          val str = symName(tree, name)
          print(if (id.isBackquoted) "`" + str + "`" else str)

        case Literal(x) =>
          print(x.escapedStringValue)

        case tt: TypeTree =>
          // TODO: figure out how to print this
          if ((tree.tpe eq null) || (printPositions && tt.original != null)) {
            if (tt.original != null) print("<type: ", tt.original, ">")
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString)
          } else {
            print(tree.tpe.toString)
          }

        case an @ Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot(): Unit = {
            print("@", tpt)
            if (args.nonEmpty)
              printRow(args, "(", ",", ")")
          }
          print(tree, if (tree.isType) " " else ": ")
          printAnnot()

        case SingletonTypeTree(ref) =>
          // TODO: figure out how to color this
          print(ref, ".type")

        case SelectFromTypeTree(qualifier, selector) =>
          print(qualifier, "#", symName(tree, selector))

        case CompoundTypeTree(templ) =>
          print(templ)

        case AppliedTypeTree(tp, args) =>
          print(tp); printRow(args, "[", ", ", "]")

        case TypeBoundsTree(lo, hi) =>
          // Avoid printing noisy empty typebounds everywhere
          // Untyped empty bounds are not printed by printOpt,
          // but after they are typed we have to exclude Nothing/Any.
          if ((lo.tpe eq null) || !(lo.tpe =:= definitions.NothingTpe))
            printOpt(" >: ", lo)

          if ((hi.tpe eq null) || !(hi.tpe =:= definitions.AnyTpe))
            printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print(tpt)
          printColumn(whereClauses, " forSome { ", ";", "}") // TODO

        // SelectFromArray is no longer visible in scala.reflect.internal.
        // eliminated until we figure out what we will do with both Printers and
        // SelectFromArray.
        // case SelectFromArray(qualifier, name, _) =>
        //   print(qualifier); print(".<arr>"); print(symName(tree, name))

        case tree =>
          // TODO: for unsupported, we should crash probably
          xprintTree(this, tree)
      }
      printTypesInfo(tree)
    }
  }
}
