package hearth.treeprinter

import java.io.{PrintWriter, StringWriter}
import scala.annotation.{nowarn, tailrec}
import scala.reflect.internal.Chars
import scala.reflect.internal.ModifierFlags.*

private[hearth] trait ShowCodePretty {
  val c: scala.reflect.macros.blackbox.Context
  import c.universe.BooleanFlag

  private val st: scala.reflect.internal.SymbolTable = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
  import st.{BooleanFlag as _, *}

  /** Better implementation of [[scala.reflect.internal.Printers#showCode]] that supports syntax highlighting. */
  private[hearth] def showCodePretty(
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

  // Copy-paste-modified from scala.reflect.internal.Printers:

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
  private class HighlighedCodePrinter(out: PrintWriter, printRootPkg: Boolean, syntaxHighlight: SyntaxHighlight)
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

    // Fixes printSeq to not print anonymous classes
    // Since printSeq is final we cannot override it
    @tailrec
    private def printSeqFixed(ls: List[Tree])(printelem: Tree => Unit)(printsep: => Unit): Unit =
      ls match {
        case List()  =>
        case List(x) => printelem(x)
        case ClassDef(mods, anonName1, tparams, impl) ::
            Apply(Select(New(Ident(anonName2)), termNames.CONSTRUCTOR), List()) :: rest if anonName1 == anonName2 =>
          printelem(New(impl)); printsep; printSeqFixed(rest)(printelem)(printsep)
        case x :: rest =>
          if (showCode(x).contains("final class $anon")) {
            scala.Predef.println(ls.take(2).map(showCode(_)).mkString("\n"))
            scala.Predef.println(ls.take(2).map(showRaw(_)).mkString("\n"))
          }
          printelem(x); printsep; printSeqFixed(rest)(printelem)(printsep)
      }

    override def printColumn(ts: List[Tree], start: String, sep: String, end: String) = {
      print(start); indent(); println()
      printSeqFixed(ts)(print(_)) { print(sep); println() }; undent(); println(); print(end)
    }

    override def printRow(ts: List[Tree], start: String, sep: String, end: String): Unit = {
      print(start); printSeqFixed(ts)(print(_))(print(sep)); print(end)
    }

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

    @nowarn
    override def processTreePrinting(tree: Tree): Unit = {

      /** This code was:
        *   - copy pasted from [[CodePrinter#processTreePrinting]]
        *   - places that called `super.printTree` [[TreePrinter#printTree]] were replaced with calls to our own
        *     printTree methods
        *   - OR inlined the code from [[TreePrinter#printTree]]
        *   - cases were reordered to what the original order of TreePrinter - perhaps it's less performant(?) but
        *     easier to maintain
        *   - added syntax highlighting
        *   - modified the code that would be printing invalid syntax
        */
      tree match {
        // don't remove synthetic ValDef/TypeDef
        case _ if syntheticToRemove(tree) =>

        case EmptyTree =>

        case cl @ ClassDef(mods, name, tparams, impl) =>
          if (mods.isJavaDefined) super.printTree(cl)
          printAnnotations(cl)
          // traits
          val clParents: List[Tree] = if (mods.isTrait) {
            // avoid abstract modifier for traits
            printModifiers(tree, mods &~ ABSTRACT)
            print(highlightKeyword("trait") + " " + printedName(name))
            printTypeParams(tparams)

            val build.SyntacticTraitDef(_, _, _, _, parents, _, _) = tree: @unchecked
            parents
            // classes
          } else {
            printModifiers(tree, mods)
            print(highlightKeyword("class") + " " + printedName(name))
            printTypeParams(tparams)

            cl match {
              case build.SyntacticClassDef(_, _, _, ctorMods, vparamss, earlyDefs, parents, selfType, body) =>
                // constructor's modifier
                if (ctorMods.hasFlag(AccessFlags) || ctorMods.hasAccessBoundary) {
                  print(" ")
                  printModifiers(ctorMods, primaryCtorParam = false)
                }

                def printConstrParams(ts: List[ValDef]): Unit =
                  parenthesize() {
                    printImplicitInParamsList(ts)
                    printSeq(ts)(printVParam(_, primaryCtorParam = true))(print(", "))
                  }
                // constructor's params processing (don't print single empty constructor param list)
                vparamss match {
                  case Nil | List(Nil) if !mods.isCase && !ctorMods.hasFlag(AccessFlags) =>
                  case _ => vparamss foreach printConstrParams
                }
                parents
              case _ =>
                // Can get here with erroneous code, like `{@deprecatedName `
                Nil
            }
          }

          // get trees without default classes and traits (when they are last)
          val printedParents =
            removeDefaultTypesFromList(clParents)()(if (mods.hasFlag(CASE)) defaultTraitsForCase else Nil)
          print(
            if (mods.isDeferred) "<: "
            else if (printedParents.nonEmpty) (" " + highlightKeyword("extends") + " ")
            else "",
            impl
          )

        case pd @ PackageDef(packaged, stats) =>
          packaged match {
            case Ident(name) if name == nme.EMPTY_PACKAGE_NAME =>
              printSeqFixed(stats) {
                print(_)
              } {
                println()
                println()
              };
            case _ =>
              printPackageDef(pd, scala.util.Properties.lineSeparator)
          }

        case md @ ModuleDef(mods, name, impl) =>
          printAnnotations(md)
          printModifiers(tree, mods)
          val Template(parents, _, _) = impl
          val parWithoutAnyRef = removeDefaultClassesFromList(parents)
          print(
            highlightKeyword("object") + " " + printedName(name),
            if (parWithoutAnyRef.nonEmpty) (" " + highlightKeyword("extends") + " ") else "",
            impl
          )

        case vd @ ValDef(mods, name, tp, rhs) =>
          printValDef(vd, printedName(name)) {
            // place space after symbolic def name (val *: Unit does not compile)
            printOpt(s"${blankForName(name)}: ", tp)
          } {
            if (!mods.isDeferred) print(" = ", if (rhs.isEmpty) "_" else rhs)
          }

        case dd @ DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printDefDef(dd, printedName(name)) {
            if (tparams.isEmpty && (vparamss.isEmpty || vparamss(0).isEmpty)) print(blankForName(name))
            printOpt(": ", tp)
          } {
            printOpt(" = " + (if (mods.isMacro) (highlightKeyword("macro") + " ") else ""), rhs)
          }

        case td @ TypeDef(mods, name, tparams, rhs) =>
          printTypeDef(td, printedName(name))

        case LabelDef(name, params, rhs) =>
          if (name.startsWith(nme.WHILE_PREFIX)) {
            val If(cond, thenp, _) = rhs: @unchecked
            print(highlightKeyword("while") + " (" + cond + ") ")
            val Block(list, wh) = thenp: @unchecked
            printColumn(list, "", ";", "")
          } else if (name.startsWith(nme.DO_WHILE_PREFIX)) {
            val Block(bodyList, If(cond, _, _)) = rhs: @unchecked
            print(highlightKeyword("do") + " ")
            printColumn(bodyList, "", ";", "")
            print(highlightKeyword("while") + " (" + cond + ") ")
          } else {
            print(printedName(name)); printLabelParams(params)
            printBlock(rhs)
          }

        case imp @ Import(expr, _) =>
          printImport(imp, resolveSelect(expr))

        case t @ Template(parents, self, tbody) =>
          val body =
            // Note: workaround - treeInfo.untypecheckedTemplBody(t) is private[internal]
            treeInfo.getClass
              .getMethod("untypecheckedTemplBody", classOf[Template])
              .invoke(treeInfo, t)
              .asInstanceOf[List[Tree]]
          val printedParents =
            currentParent map {
              case _: CompoundTypeTree                       => parents
              case ClassDef(mods, name, _, _) if mods.isCase =>
                removeDefaultTypesFromList(parents)()(defaultTraitsForCase)
              case _ => removeDefaultClassesFromList(parents)
            } getOrElse (parents)

          val primaryCtr = treeInfo.firstConstructor(body)
          val ap: Option[Apply] = primaryCtr match {
            case DefDef(_, _, _, _, _, Block(ctBody, _)) =>
              val earlyDefs = treeInfo.preSuperFields(ctBody) ::: body.filter {
                case td: TypeDef => treeInfo.isEarlyDef(td)
                case _           => false
              }
              if (earlyDefs.nonEmpty) {
                print("{")
                printColumn(earlyDefs, "", ";", "")
                print("} " + (if (printedParents.nonEmpty) (highlightKeyword("with") + " ") else ""))
              }
              ctBody collectFirst { case apply: Apply =>
                apply
              }
            case _ => None
          }

          if (printedParents.nonEmpty) {
            val (clParent :: traits) = printedParents: @unchecked
            print(clParent)

            val constrArgss = ap match {
              case Some(treeInfo.Applied(_, _, argss)) => argss
              case _                                   => Nil
            }
            printArgss(constrArgss)
            if (traits.nonEmpty) {
              printRow(traits, " " + highlightKeyword("with") + " ", " " + highlightKeyword("with") + " ", "")
            }
          }
          /* Remove primary constr def and constr val and var defs
           * right contains all constructors
           */
          val (left, right) = body.filter {
            // remove valdefs defined in constructor and presuper vals
            case vd: ValDef => !vd.mods.isParamAccessor && !treeInfo.isEarlyValDef(vd)
            // remove $this$ from traits
            case dd: DefDef  => dd.name != nme.MIXIN_CONSTRUCTOR
            case td: TypeDef => !treeInfo.isEarlyDef(td)
            case EmptyTree   => false
            case _           => true
          } span {
            case dd: DefDef => dd.name != nme.CONSTRUCTOR
            case _          => true
          }
          val modBody = (left ::: right.drop(1))
          val showBody = !(modBody.isEmpty && (self == noSelfType || self.isEmpty))
          if (showBody) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name)
              printOpt(": ", self.tpt)
              print(" =>")
            } else if (self.tpt.nonEmpty) {
              print(" { _ : ", self.tpt, " =>")
            } else {
              print(" {")
            }
            printColumn(modBody, "", ";", "}")
          }

        case bl @ Block(stats, expr) =>
          printBlock(
            // Note: workaround - treeInfo.untypecheckedBlockBody(t) is private[internal]
            treeInfo.getClass
              .getMethod("untypecheckedBlockBody", classOf[Block])
              .invoke(treeInfo, bl)
              .asInstanceOf[List[Tree]],
            expr
          )

        case Match(selector, cases) =>
          /* Insert braces if match is inner
           * make this function available for other cases
           * passing required type for checking
           */
          def insertBraces(body: => Unit): Unit =
            if (parentsStack.nonEmpty && parentsStack.tail.exists(_.isInstanceOf[Match])) {
              print("(")
              body
              print(")")
            } else body

          val printParentheses = needsParentheses(selector)(insideLabelDef = false)
          tree match {
            case Match(EmptyTree, cs) =>
              printColumn(cases, "{", "", "}")
            case _ =>
              insertBraces {
                parenthesize(printParentheses)(print(selector))
                printColumn(cases, " " + highlightKeyword("match") + " {", "", "}")
              }
          }

        case cd @ CaseDef(pat, guard, body) =>
          printCaseDef(cd)

        // Note: copy-paste-modified from TreePrinter.printTree
        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          print(elem, "*") // TODO: check coloring of that

        case Bind(name, t) =>
          if (t == EmptyTree) print("(", printedName(name), ")")
          else if (t.exists(_.isInstanceOf[Star])) print(printedName(name), " @ ", t)
          else print("(", printedName(name), " @ ", t, ")")

        case UnApply(fun, args) =>
          fun match {
            case treeInfo.Unapplied(body) =>
              body match {
                case Select(qual, name) if name == nme.unapply                                         => print(qual)
                case TypeApply(Select(qual, name), _) if name == nme.unapply || name == nme.unapplySeq =>
                  print(qual)
                case _ => print(body)
              }
            case _ => print(fun)
          }
          printRow(args, "(", ", ", ")")

        // Note: copy-paste-modified from TreePrinter.printTree
        case ArrayValue(elemtpt, trees) =>
          print("Array[", elemtpt); printRow(trees, "]{", ", ", "}") // TODO: check coloring of that

        case f @ Function(vparams, body) =>
          // parentheses are not allowed for val a: Int => Int = implicit x => x
          val printParentheses = vparams match {
            case head :: _ => !head.mods.isImplicit
            case _         => true
          }
          printFunction(f)(printValueParams(vparams, inParentheses = printParentheses))

        // Note: copy-paste-modified from TreePrinter.printTree
        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        // Note: copy-paste-modified from TreePrinter.printTree
        case NamedArg(lhs, rhs) =>
          print(lhs, " = ", rhs)

        // Note: copy-paste-modified from TreePrinter.printTree
        case If(cond, thenp, elsep) =>
          print(highlightKeyword("if") + " (", cond, ")"); indent(); println()
          print(thenp); undent()
          if (elsep.nonEmpty) {
            println(); print(highlightKeyword("else") + " "); indent(); println(); print(elsep); undent()
          }

        // Note: copy-paste-modified from TreePrinter.printTree
        case Return(expr) =>
          print(highlightKeyword("return") + " ", expr)

        // Note: copy-paste-modified from TreePrinter.printTree
        case Try(block, catches, finalizer) =>
          print(highlightKeyword("try") + " "); printBlock(block)
          if (catches.nonEmpty) printColumn(catches, " " + highlightKeyword("catch") + " {", "", "}")
          printOpt(" " + highlightKeyword("finally") + " ", finalizer)

        // Note: copy-paste-modified from TreePrinter.printTree
        case Throw(expr) =>
          print(highlightKeyword("throw") + " ", expr)

        // Note: copy-paste-modified from TreePrinter.printTree
        case New(tpe) =>
          print(highlightKeyword("new") + " ", tpe)

        case Typed(expr, tp) =>
          def printTp() = print("(", tp, ")")

          tp match {
            case EmptyTree | EmptyTypeTree() => printTp()
            // case for untypechecked trees
            case Annotated(annot, arg) if (expr ne null) && (arg ne null) && expr.equalsStructure(arg) =>
              printTp() // remove double arg - 5: 5: @unchecked
            case tt: TypeTree if tt.original.isInstanceOf[Annotated] => printTp()
            case Function(List(), EmptyTree)                         => print("(", expr, " _)") // func _
            // parentheses required when (a match {}) : Type
            case _ =>
              val safeToSkipExtraParens = expr match {
                case Match(_, _) => false
                case _           => true
              }
              if (safeToSkipExtraParens) print("(", expr, ": ", tp, ")")
              else print("((", expr, "): ", tp, ")")
          }

        // print only fun when targs are TypeTrees with empty original
        case TypeApply(fun, targs) =>
          if (targs.exists(isEmptyTree(_))) {
            print(fun)
          } else {
            // Note: copy-paste-modified (and inlined) from TreePrinter.printTree
            print(fun)
            printRow(targs, "[", ", ", "]")
          }

        case Apply(fun, vargs) =>
          tree match {
            // processing methods ending on colons (x \: list)
            case Apply(Block(l1 @ List(sVD: ValDef), a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))), l3)
                if sVD.mods.isSynthetic && nme.isLeftAssoc(methodName) && sVD.name == iVDName =>
              val printBlock = Block(l1, Apply(a1, l3))
              print(printBlock)
            case Apply(tree1, _) if (needsParentheses(tree1)(insideAnnotated = false)) =>
              parenthesize()(print(fun)); printRow(vargs, "(", ", ", ")")
            case _ =>
              // Note: copy-paste-modified (and inlined) from TreePrinter.printTree
              print(fun)
              printRow(vargs, "(", ", ", ")")
          }

        // Note: copy-paste-modified from TreePrinter.printTree
        case ApplyDynamic(qual, vargs) =>
          print(
            "<apply-dynamic>(",
            qual,
            "#",
            tree.symbol.nameString
          ) // TODO: check wth is this, and how should we print it?
          printRow(vargs, ", (", ", ", "))")

        case st @ Super(This(qual), mix) =>
          printSuper(st, printedName(qual), checkSymbol = false)

        // Note: copy-paste-modified from TreePrinter.printTree
        case Super(qual, mix) =>
          print(qual, ".super") // TODO: check coloring of that
          if (mix.nonEmpty)
            print("[" + mix + "]")

        case th @ This(qual) =>
          if (tree.hasExistingSymbol && tree.symbol.hasPackageFlag) print(tree.symbol.fullName)
          else printThis(th, printedName(qual))

        // remove this prefix from constructor invocation in typechecked trees: this.this -> this
        case Select(This(_), name @ nme.CONSTRUCTOR) => print(printedName(name))

        case Select(qual: New, name) =>
          print(qual)

        case Select(qual, name) =>
          @tailrec
          def checkRootPackage(tr: Tree): Boolean =
            (currentParent match { // check that Select is not for package def name
              case Some(_: PackageDef) => false
              case _                   => true
            }) && (tr match { // check that Select contains package
              case Select(q, _)       => checkRootPackage(q)
              case _: Ident | _: This =>
                val sym = tr.symbol
                tr.hasExistingSymbol && sym.hasPackageFlag && sym.name != nme.ROOTPKG
              case _ => false
            })

          if (printRootPkg && checkRootPackage(tree)) print(s"${printedName(nme.ROOTPKG)}.")
          val printParentheses = needsParentheses(qual)(insideAnnotated = false) || isIntLitWithDecodedOp(qual, name)
          if (printParentheses) print("(", resolveSelect(qual), ").", printedName(name))
          else print(resolveSelect(qual), ".", printedName(name))

        case id @ Ident(name) =>
          if (name.nonEmpty) {
            if (name == nme.dollarScope) {
              print(s"scala.xml.${nme.TopScope}")
            } else {
              val str = printedName(name)
              val strIsBackquoted = str.startsWith("`") && str.endsWith("`")
              print(if (id.isBackquoted && !strIsBackquoted) "`" + str + "`" else str)
            }
          } else {
            print("")
          }

        case Literal(k @ Constant(s: String)) if s.contains(Chars.LF) =>
          print(StringColor)
          val tq = "\"" * 3
          val lines = s.linesIterator.toList
          if (lines.lengthCompare(1) <= 0) print(k.escapedStringValue)
          else {
            val tqp = """["]{3}""".r
            val tqq = """""\\"""" // ""\" is triple-quote quoted
            print(tq)
            printSeq(lines.map(x => tqp.replaceAllIn(x, tqq)))(print(_))(print(Chars.LF))
            print(tq)
          }
          print(NoColor)

        case Literal(x) =>
          // processing Float constants
          val suffix = x.value match {
            case _: Float => "f" // Note: originally: "F"
            case _        => ""
          }
          print(highlightLiteral(s"${x.escapedStringValue}$suffix"))

        case tt: TypeTree =>
          if (!isEmptyTree(tt)) {
            val original = tt.original
            if (original != null) print(original)
            // Note: copy-paste-modified (and inlined) from TreePrinter.printTree
            else if ((tree.tpe eq null) || (printPositions && tt.original != null)) {
              if (tt.original != null) print(tt.original) // Note: originally: print("<type: ", tt.original, ">")
              else unsupportedTree(tree) // Note: originally: print("<type ?>")
            } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
              print(highlightTypeDef(tree.tpe.typeSymbol.toString)) // TODO: check if we can do better
            } else {
              print(highlightTypeDef(tree.tpe.toString)) // TODO: check if we can do better
            }
          }

        case an @ Annotated(ap, tree) =>
          val printParentheses = needsParentheses(tree)()
          parenthesize(printParentheses)(print(tree)); print(if (tree.isType) " " else ": ")
          printAnnot(ap)

        // Note: copy-paste-modified from TreePrinter.printTree
        case SingletonTypeTree(ref) =>
          print(ref, ".type")

        case SelectFromTypeTree(qualifier, selector) =>
          print(
            "(",
            qualifier,
            ")#",
            blankForOperatorName(selector),
            printedName(selector)
          )

        // Note: copy-paste-modified from TreePrinter.printTree
        case CompoundTypeTree(templ) =>
          print(templ) // TODO: check coloring of that

        case AppliedTypeTree(tp, args) =>
          // it's possible to have (=> String) => String type but Function1[=> String, String] is not correct
          val containsByNameTypeParam = args exists treeInfo.isByNameParamType

          if (containsByNameTypeParam) {
            print("(")
            printRow(args.init, "(", ", ", ")")
            print(" => ", args.last, ")")
          } else {
            if (treeInfo.isRepeatedParamType(tree) && args.nonEmpty) {
              print(args(0), "*")
            } else if (treeInfo.isByNameParamType(tree)) {
              print("=> ", if (args.isEmpty) "()" else args(0))
            } else
              // Note: copy-paste-modified (and inlined) from TreePrinter.printTree
              print(tp);
            printRow(args, "[", ", ", "]")
          }

        // Note: copy-paste-modified from TreePrinter.printTree
        case TypeBoundsTree(lo, hi) =>
          // Avoid printing noisy empty typebounds everywhere
          // Untyped empty bounds are not printed by printOpt,
          // but after they are typed we have to exclude Nothing/Any.
          if ((lo.tpe eq null) || !(lo.tpe =:= definitions.NothingTpe))
            printOpt(" >: ", lo)

          if ((hi.tpe eq null) || !(hi.tpe =:= definitions.AnyTpe))
            printOpt(" <: ", hi)

        // Note: copy-paste-modified from TreePrinter.printTree
        case ExistentialTypeTree(tpt, whereClauses) =>
          print("(", tpt)
          printColumn(whereClauses, " " + highlightKeyword("forSome") + " { ", ";", "})")

        case tree => // Note: originally: super.printTree(tree) but we cannot rely on TreePrinter for syntax highlighting
          unsupportedTree(tree)

      }
    }

    private def codeForError(tree: Tree): String =
      Option(tree).map(showCode(_)).filter(_.nonEmpty).getOrElse("<no tree ?>")

    private def unsupportedTree(tree: Tree): Nothing = throw new UnsupportedOperationException(
      s"""Unsupported tree for `showCodePretty`:
         |
         |
         |${Option(tree).map(showCode(_)).filter(_.nonEmpty).getOrElse("<no tree ?>")}
         |
         |constructed as:
         |
         |${showRaw(tree)}
         |
         |inside:
         |
         |${parentsStack.iterator.map(showCode(_)).filter(_.nonEmpty).nextOption().getOrElse("<no parents ?>")}
         |
         |(Code in this error message rendered using `showCode` instead of `showCodePretty`)
         |
         |Please, report an issue at https://github.com/MateuszKubuszok/hearth/issues
         |""".stripMargin
    )
  }
}
