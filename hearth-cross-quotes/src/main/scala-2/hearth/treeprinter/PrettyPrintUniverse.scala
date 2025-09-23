package hearth
package treeprinter

import java.io.PrintWriter

class PrettyPrintUniverse(u: scala.reflect.macros.Universe, syntaxHighlight: SyntaxHighlight)
    extends ExtensibleUniverse(u) {

  import syntaxHighlight.*
  import SyntaxHighlight.*

  def showCodeAnsi(
      tree: Tree,
      printTypes: BooleanFlag = None,
      printIds: BooleanFlag = None,
      printOwners: BooleanFlag = None,
      printPositions: BooleanFlag = None,
      printRootPkg: Boolean = false
  ): String = render(
    tree,
    new AnsiCodePrinter(_, printRootPkg),
    printTypes,
    printIds,
    printOwners,
    printKinds = None,
    printMirrors = None,
    printPositions
  )

  override def quotedName(name: Name, decode: Boolean): String = {
    val s = if (decode) name.decode else name.toString
    val term = name.toTermName
    val value = if (nme.keywords(term) && term != nme.USCOREkw) "`%s`" format s else s
    if (name.isTypeName) highlightTypeDef(value)
    else value
  }

  private def symNameInternal(tree: Tree, name: Name, decoded: Boolean): String = {
    val sym = tree.symbol
    def qname = quotedName(name.dropLocal, decoded)
    def qowner = quotedName(sym.owner.name.dropLocal, decoded)
    def qsymbol = quotedName(sym.nameString)
    if (sym == null || sym.isInstanceOf[NoSymbol]) qname
    else if (sym.isErroneous) s"<$qname: error>"
    else if (sym.isMixinConstructor) s"/*$qowner*/$qsymbol"
    else qsymbol
  }

  override def decodedSymName(tree: Tree, name: Name): String = symNameInternal(tree, name, decoded = true)
  override def symName(tree: Tree, name: Name): String = symNameInternal(tree, name, decoded = false)

  class AnsiCodePrinter(out: PrintWriter, printRootPkg: Boolean) extends CodePrinter(out, printRootPkg) {

    override def printAnnot(tree: Tree): Unit = {
      print(AnnotationColor)
      super.printAnnot(tree)
      print(NoColor)
    }

    override def printModifiers(mods: Modifiers, primaryCtorParam: Boolean): Unit = {
      print(KeywordColor)
      super.printModifiers(mods, primaryCtorParam)
      print(NoColor)
    }

    override def printVParam(vd: ValDef, primaryCtorParam: Boolean): Unit = {
      print(ValDefColor)
      super.printVParam(vd, primaryCtorParam)
      print(NoColor)
    }

    override protected def printTypeDef(tree: TypeDef, resultName: => String): Unit = {
      print(TypeColor)
      super.printTypeDef(tree, resultName)
      print(NoColor)
    }

    private def printRowColor(ts: List[Tree], sep: String, color: String): Unit = printRowColor(ts, "", sep, "", color)

    private def printRowColor(ts: List[Tree], start: String, sep: String, end: String, color: String): Unit = {
      print(start)
      printSeq(ts) { t =>
        print(color)
        print(t)
        print(NoColor)
      } {
        print(sep)
      }
      print(end)
    }

    private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
      case null                              => orElse
      case sym if sym.isInstanceOf[NoSymbol] => orElse
      case sym                               => f(sym)
    }
    private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

    private[this] var currentOwner: Symbol = NoSymbol
    private[this] var selectorType: Type = NoType

    /** Adaptation of [[scala.reflect.internal.Printers#printTree]] that supports syntax highlighting and avoids
      * printing incorrect code.
      * @param tree
      *   tree to print
      */
    @scala.annotation.nowarn
    override def printTree(tree: Tree): Unit = {
      tree match {
        case EmptyTree =>
          print("<empty>")

        case cd @ ClassDef(mods, name, tparams, impl) =>
          printAnnotations(cd)
          printModifiers(tree, mods)
          val word =
            if (mods.isTrait) highlightKeyword("trait")
            else if (ifSym(tree, _.isModuleClass)) highlightKeyword("object")
            else highlightKeyword("class")

          print(word, " ", symName(tree, name))
          printTypeParams(tparams)
          print(if (mods.isDeferred) " <: " else " " + highlightKeyword("extends") + " ", impl)

        case pd @ PackageDef(packaged, stats) =>
          printPackageDef(pd, ";")

        case md @ ModuleDef(mods, name, impl) =>
          printAnnotations(md)
          printModifiers(tree, mods)
          print(highlightKeyword("object") + " " + symName(tree, name), " " + highlightKeyword("extends") + " ", impl)

        case vd @ ValDef(mods, name, tp, rhs) =>
          printValDef(vd, highlightValDef(symName(tree, name)))(printOpt(": ", tp)) {
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
          printRowColor(parents, " " + highlightKeyword("with") + " ", TypeColor)
          if (body.nonEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name)
              printOpt(": ", self.tpt)
              print(" => ")
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
          print(selector)
          printColumn(cases, " " + highlightKeyword("match") + " {", "", "}")
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
          print(fun, " <unapply> "); printRow(args, "(", ", ", ")")

        case ArrayValue(elemtpt, trees) =>
          print(highlightTypeDef("scala.Array") + "[", elemtpt); printRowColor(trees, "]{", ", ", "}", ValDefColor)

        case f @ Function(vparams, body) =>
          printFunction(f)(printValueParams(vparams))

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case NamedArg(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case If(cond, thenp, elsep) =>
          print(highlightKeyword("if") + " (", cond, ")");
          indent();
          println()
          print(thenp);
          undent()
          if (elsep.nonEmpty) {
            println()
            print(highlightKeyword("else"));
            indent()
            println()
            print(elsep);
            undent()
          }

        case Return(expr) =>
          print(highlightKeyword("return") + " ", expr)

        case Try(block, catches, finalizer) =>
          print(highlightKeyword("try") + " ");
          printBlock(block)
          if (catches.nonEmpty) printColumn(catches, " catch {", "", "}")
          printOpt(" " + highlightKeyword("finally") + " ", finalizer)

        case Throw(expr) =>
          print(highlightKeyword("throw") + " ", expr)

        case New(tpe) =>
          print(highlightKeyword("new") + " ", tpe)

        case Typed(expr, tp) =>
          print("(", expr, ": ", tp, ")")

        case TypeApply(fun, targs) =>
          print(fun)
          printRow(targs, "[", ", ", "]")
        // printRowColor(targs, "[", ", ", "]", TypeColor)

        case Apply(fun, vargs) =>
          print(fun)
          printRow(vargs, "(", ", ", ")")
        // printRowColor(vargs, "(", ", ", ")", ValDefColor)

        case ApplyDynamic(qual, vargs) =>
          print("<apply-dynamic>(", qual, "#", tree.symbol.nameString)
          printRowColor(vargs, ", (", ", ", "))", ValDefColor)

        case st @ Super(This(qual), mix) =>
          printSuper(st, symName(tree, qual))

        case Super(qual, mix) =>
          print(qual, ".super")
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
          if (x.isNumeric) print(LiteralColor) else print(StringColor)
          print(x.escapedStringValue)
          print(NoColor)

        case tt: TypeTree =>
          print(TypeColor)
          if ((tree.tpe eq null) || (printPositions && tt.original != null)) {
            if (tt.original != null) print("<type: ", tt.original, ">")
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString)
          } else {
            print(tree.tpe.toString)
          }
          print(NoColor)

        case an @ Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot(): Unit = {
            print("@", tpt)
            if (args.nonEmpty)
              printRowColor(args, "(", ",", ")", ValDefColor)
          }

          print(tree, if (tree.isType) " " else ": ")
          printAnnot()

        case SingletonTypeTree(ref) =>
          print(TypeColor)
          print(ref, ".type")
          print(NoColor)

        case SelectFromTypeTree(qualifier, selector) =>
          print(qualifier, "#", symName(tree, selector))

        case CompoundTypeTree(templ) =>
          print(templ)

        case AppliedTypeTree(tp, args) =>
          print(tp); printRowColor(args, "[", ", ", "]", TypeColor)

        case TypeBoundsTree(lo, hi) =>
          // Avoid printing noisy empty type bounds everywhere
          // Untyped empty bounds are not printed by printOpt,
          // but after they are typed we have to exclude Nothing/Any.
          if ((lo.tpe eq null) || !(lo.tpe =:= definitions.NothingTpe))
            printOpt(" >: ", lo)

          if ((hi.tpe eq null) || !(hi.tpe =:= definitions.AnyTpe))
            printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print(tpt)
          printColumn(whereClauses, " forSome { ", ";", "}")

        // SelectFromArray is no longer visible in scala.reflect.internal.
        // eliminated until we figure out what we will do with both Printers and
        // SelectFromArray.
        // case SelectFromArray(qualifier, name, _) =>
        //   print(qualifier); print(".<arr>"); print(symName(tree, name))

        case tree =>
          xprintTree(this, tree)
      }
      printTypesInfo(tree)
    }

    override def print(args: Any*): Unit = args foreach {
      case tree if tree.isInstanceOf[Tree] =>
        printPosition(tree.asInstanceOf[Tree])
        printTree(tree.asInstanceOf[Tree])
      // case name: Name =>
      case name if name.isInstanceOf[Name] =>
        print(quotedName(name.asInstanceOf[Name]))
      case arg =>
        out.print(if (arg == null) "null" else arg.toString)
    }
  }
}
