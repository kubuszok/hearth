package hearth
package treeprinter

import java.io.{PrintWriter, Writer}
import scala.annotation.{nowarn, tailrec}
import scala.reflect.internal.Chars
import scala.reflect.internal.ModifierFlags.*

trait ShowCodePrettyScala2 {
  val c: scala.reflect.macros.blackbox.Context

  import c.universe.BooleanFlag, implementationDetails.*

  /** Better implementation of [[scala.reflect.internal.Printers#showCode]] that supports syntax highlighting.
    *
    * @since 0.2.0
    */
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
      new HighlighedCodePrinter(_, printRootPkg, _, _, notRunnableExprResult = isResultNotUsedAsRunnableCode(any)),
      printTypes,
      printIds,
      printOwners,
      printKinds = None,
      printMirrors = None,
      printPositions
    )

  /** Better implementation of [[scala.reflect.internal.Printers#showRaw]] that supports syntax highlighting.
    *
    * @since 0.2.0
    */
  def showRawPretty(
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
      new HighlighedRawTreePrinter(_, _, _),
      printTypes,
      printIds,
      printOwners,
      printKinds = None,
      printMirrors = None,
      printPositions
    )

  /** Adding new val/lazy val/var to the trait is breaking backward compatibility in the bytecode (SIC!).
    *
    * Therefore we should avoid that when possible - for private fields, `private` modifier does NOT help, since the
    * compiler still needs to do some magic to allow the val to refer to other vals which can be overridden.
    *
    * Instead we can create a private object - adding it, would break backward compatibility, but if it already exists,
    * and it's not available to the user, it's fine to add things there when needed.
    */
  private object implementationDetails {

    val st: scala.reflect.internal.SymbolTable = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    import st.{BooleanFlag as _, *}

    Constants.initSettings(c.settings)
    import Constants.*

    // Unholy workarounds

    // There are cases like `trait SomeTrait` defined inside a class/method:
    //  - we should print them as `SomeTrait` when printing expression that uses them,
    //  - but we want `com.package.name.OuterClass.SomeTrait` when printing type signature of the trait
    def isResultNotUsedAsRunnableCode(any: Any): Boolean = any.isInstanceOf[c.universe.TypeApi]

    // While CodePrinter hides `class $anon ... {}; new $anon`, printing this.methodOrField would still
    // need a workaround to not print `$anon.this.methodOrField` or similar. So we have to detect such cases.
    object AnonThis {
      def unapply(tree: Tree): Boolean = tree match {
        case This(anon) if tree.hasSymbolField && tree.symbol != NoSymbol =>
          lazy val typeName = anon.decoded.toString
          lazy val symbolName = tree.symbol.name.toString
          (typeName.isEmpty && symbolName.startsWith("$anon")) || typeName.startsWith("$anon")
        case _ => false
      }
    }
    object AnonThisType {
      def unapply(tpe: Type): Boolean = tpe match {
        case ThisType(anon) => anon.name.decoded.toString.startsWith("$anon")
        case _              => false
      }
    }

    // Without this for `import ee.{value as expr}` would print `ee.expr` instead of `ee.value`
    // (`expr` would look much better, but does not work for nested selects of nested imports).
    object RenamedSelect {
      def unapply(tree: Tree): Option[(Name, Name)] = tree match {
        case Select(Ident(qualName), name)
            if tree.hasSymbolField && tree.symbol != NoSymbol && tree.symbol.name.decoded != name.decoded =>
          Some((qualName, tree.symbol.name))
        case _ => None
      }
    }

    // Copy-pasted from TypesScala2.scala because we need it here as well.

    /** It is surprisingly ridiculous but I've found no other way of telling whether I am looking at enum abstract class
      * or its value, since EVERYTHING else looks the same: parent is not abstract, everyone is static, everyone has the
      * same baseClasses, everyone reports to have public primaryConstructor (which is <none>). The only different in
      * behavior is that one prints com.my.Enum and another com.my.Enum(MyValue).
      */
    val javaEnumRegexpFormat = raw"^(.+)\((.+)\)$$".r

    // Copy-paste-modified from scala.reflect.internal.Printers:

    def render(
        what: Any,
        highlight: SyntaxHighlight,
        mkPrinter: (PrintWriter, SyntaxHighlight, StringBuilder) => InternalTreePrinter,
        printTypes: BooleanFlag = None,
        printIds: BooleanFlag = None,
        printOwners: BooleanFlag = None,
        printKinds: BooleanFlag = None,
        printMirrors: BooleanFlag = None,
        printPositions: BooleanFlag = None
    ): String = {
      val buffer = new StringBuilderWriter() // originally: StringWriter but we need to be able to trim
      val writer = new PrintWriter(buffer)
      val printer = mkPrinter(writer, highlight, buffer.impl)

      printTypes.value.foreach(if (_) printer.withTypes else printer.withoutTypes)
      printIds.value.foreach(if (_) printer.withIds else printer.withoutIds)
      printOwners.value.foreach(if (_) printer.withOwners else printer.withoutOwners)
      printKinds.value.foreach(if (_) printer.withKinds else printer.withoutKinds)
      printMirrors.value.foreach(if (_) printer.withMirrors else printer.withoutMirrors)
      printPositions.value.foreach(if (_) printer.withPositions else printer.withoutPositions)

      try {
        printer.print(what)
        writer.flush()
        buffer.impl.toString
      } catch {
        // $COVERAGE-OFF$
        case StringBuildingTerminated(sb) =>
          val warningLength = stringBuilderLimitWarning.length
          val truncated =
            if (sb.length <= (stringBuilderHardLimit - warningLength)) sb
            else sb.take(stringBuilderHardLimit - warningLength)
          truncated.append(stringBuilderLimitWarning).toString
        // $COVERAGE-ON$
      }
    }

    private class StringBuilderWriter extends Writer {
      val impl: StringBuilder = new StringBuilder
      override def write(cbuf: Array[Char], off: Int, len: Int): Unit = impl.appendAll(cbuf, off, len)
      override def flush(): Unit = ()
      override def close(): Unit = ()
      override def toString(): String = impl.toString
    }

    private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
      case null | NoSymbol => orElse
      case sym             => f(sym)
    }
    private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

    /** Better implementation of [[scala.reflect.internal.Printers#CodePrinter]] that supports syntax highlighting. */
    final class HighlighedCodePrinter(
        out: PrintWriter,
        printRootPkg: Boolean,
        syntaxHighlight: SyntaxHighlight,
        sb: StringBuilder,
        notRunnableExprResult: Boolean
    ) extends CodePrinter(out, printRootPkg) {
      import syntaxHighlight.*

      // Colors printed name
      // If symbol name ends with "$" it is an object, and we replace it with ".type".
      // Exception: if it contains "[α-ω]$[digit]$" it is a parameter of a type lambda generated by kind-projector, and we do not want to print .type.
      private val lambdaParamPattern = raw"[α-ω]\$$\d+\$$".r

      // Overriden from CodePrinter:
      override protected def printedName(name: Name, decoded: Boolean = true) = {
        val (result, dotType) = {
          val x = super.printedName(name, decoded)
          if (x == "`_*`") ("_*", false) // special case for VarArgs
          else if (lambdaParamPattern.matches(x)) (x, false)
          else if (x.endsWith("$")) (x.dropRight("$".length), true)
          else (x, false)
        }
        val hl = if (name.isTypeName) highlightTypeDef(result) else highlightValDef(result)
        if (dotType) hl + ".type" else hl
      }

      // replaces CodePrinter with HighlighedCodePrinter
      override protected def resolveSelect(t: Tree): String = t match {
        // Skip $anon.this - syntethic $anon class was removed so we need to remove the prefix as well.
        case Select(AnonThis(), name) => printedName(name)
        // Handle this-types.
        case Select(t2 @ This(thisName), name) =>
          val printed = printedName(thisName)
          s"${if (printed.nonEmpty) (printed + ".") else ""}${if (!t2.symbol.hasPackageFlag) "this." else ""}${printedName(name)}"
        // Without this for `import ee.{value as expr}` would print `ee.expr` instead of `ee.value`
        // (`expr` would look much better, but does not work for nested selects of nested imports).
        case RenamedSelect(qualName, name) =>
          s"${printedName(qualName)}.${printedName(name)}"
        // Handle regular selects.
        case Select(qual, name) =>
          val resolved = resolveSelect(qual)
          val printed = printedName(name)
          if (resolved.isEmpty) printed
          else if (name == termNames.PACKAGE) resolved
          // case for:
          // 1) (if (a) b else c).meth1.meth2 or
          // 2) 1 + 5 should be represented as (1).+(5)
          else if (
            (name.isTermName && needsParentheses(qual)(insideLabelDef = false)) || isIntLitWithDecodedOp(qual, name)
          )
            // $COVERAGE-OFF$ It happens but I have no idea how to test it
            s"($resolved).$printed"
          // $COVERAGE-ON$
          else if (name.isTypeName)
            // $COVERAGE-OFF$ It happens but I have no idea how to test it
            s"$resolved#${blankForOperatorName(name)}%$printed"
          // $COVERAGE-ON$
          else
            s"$resolved.$printed"
        case Ident(name) =>
          printedName(name)
        // Fallback - since we need a String, we cannot just use print, so we create a nested printer.
        case _ => render(t, syntaxHighlight, new HighlighedCodePrinter(_, printRootPkg, _, _, notRunnableExprResult))
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
        print(highlightValDef("this"))
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
          *   - added handling for imported
          */
        tree match {
          // To prevent "UTF8 string too large" errors, we limit the size of the StringBuilder.
          case _ if sb.length > stringBuilderSoftLimit =>
            // To test the whole tree, we can clear the StringBuilder and continue printing.
            // The result makes no sense, but we can look for exceptions on unhandled cases.
            // $COVERAGE-OFF$
            if (areWeInTests) {
              sb.clear()
              processTreePrinting(tree)
            } else throw StringBuildingTerminated(sb)
          // $COVERAGE-ON$

          // don't remove synthetic ValDef/TypeDef
          case _ if syntheticToRemove(tree) =>

          // skip anon - $anon was removed so we need to remove the prefix as well
          case Select(AnonThis(), name) =>
            print(printedName(name))

          // handle this-types: Select(This(qual), name) should print as "qual.this.name"
          case Select(This(thisName), name) =>
            val printed = printedName(thisName)

            if (printed.nonEmpty) {
              print(printed, ".this.", printedName(name))
            } else {
              // $COVERAGE-OFF$ It happens but I have no idea how to test it
              print("this.", printedName(name))
              // $COVERAGE-ON$
            }

          // Without this for `import ee.{value as expr}` would print `ee.expr` instead of `ee.value`
          // (`expr` would look much better, but does not work for nested selects of nested imports).
          case RenamedSelect(qualName, name) =>
            print(printedName(qualName), ".", printedName(name))

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
              // $COVERAGE-OFF$ It happens but I have no idea how to test it
              processTreePrinting(fun)
              // $COVERAGE-ON$
            } else {
              // Note: copy-paste-modified (and inlined) from TreePrinter.printTree
              processTreePrinting(fun)
              printRow(targs, "[", ", ", "]")
            }

          case Apply(fun, vargs) =>
            tree match {
              // processing methods ending on colons (x \: list)
              case Apply(
                    Block(l1 @ List(sVD: ValDef), a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))),
                    l3
                  ) if sVD.mods.isSynthetic && nme.isLeftAssoc(methodName) && sVD.name == iVDName =>
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

            lazy val resolved = resolveSelect(qual)
            lazy val printed = printedName(name)

            if (printRootPkg && checkRootPackage(tree)) print(s"${printedName(nme.ROOTPKG)}.")
            val printParentheses = needsParentheses(qual)(insideAnnotated = false) || isIntLitWithDecodedOp(qual, name)
            if (resolved.isEmpty) print(printed)
            else if (printed == "`package`") print(resolved)
            else if (printParentheses) print("(", resolved, ").", printed)
            else print(resolved, ".", printed)

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
            val result = s"${x.escapedStringValue}$suffix"
            print(if (x.value.isInstanceOf[String]) highlightString(result) else highlightLiteral(result))

          case tt: TypeTree =>
            if (!isEmptyTree(tt)) {
              val original = tt.original
              // Prevents assertionErrors when printing stubs used by Cross-Quotes.
              def isAnonymousClass = bestEffort {
                (tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass
              }(false)
              if (original != null) print(original)
              // Note: copy-paste-modified (and inlined) from TreePrinter.printTree
              else if ((tree.tpe eq null) || (printPositions && tt.original != null)) {
                // $COVERAGE-OFF$ It happens but I have no idea how to test it
                if (tt.original != null) print(tt.original) // Note: originally: print("<type: ", tt.original, ">")
                else if (failOnUnsupportedTree) unsupportedTree(tree) // Note: originally: print("<type ?>")
                else print("<type ?>")
                // $COVERAGE-ON$
              } else if (isAnonymousClass) {
                // $COVERAGE-OFF$ It happens but I have no idea how to test it
                print(highlightTypeDef(tree.tpe.typeSymbol.toString)) // TODO: check if we can do better
                // $COVERAGE-ON$
              } else {
                processTypePrinting(tree.tpe, isLastInChain = true)
              }
            }

          case an @ Annotated(ap, tree) =>
            val printParentheses = needsParentheses(tree)()
            parenthesize(printParentheses)(print(tree)); print(if (tree.isType) " " else ": ")
            printAnnot(ap)

          // Note: copy-paste-modified from TreePrinter.printTree
          case SingletonTypeTree(ref) =>
            print(ref, ".type")

          case SelectFromTypeTree(qual, sel) =>
            print("(", qual, ")#", blankForOperatorName(sel), printedName(sel))

          // Note: copy-paste-modified from TreePrinter.printTree
          case CompoundTypeTree(templ) =>
            print(templ) // TODO: check coloring of that

          case AppliedTypeTree(tp, args) =>
            // Prevents assertionErrors when printing stubs used by Cross-Quotes.
            def isByNameParam(tree: Tree) = bestEffort {
              treeInfo.isByNameParamType(tree)
            }(false)

            // it's possible to have (=> String) => String type but Function1[=> String, String] is not correct
            val containsByNameTypeParam = args exists isByNameParam

            if (containsByNameTypeParam) {
              // $COVERAGE-OFF$ It happens but I have no idea how to test it
              print("(")
              printRow(args.init, "(", ", ", ")")
              print(" => ", args.last, ")")
              // $COVERAGE-ON$
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

          // Note: originally: super.printTree(tree) but we cannot rely on TreePrinter for syntax highlighting
          case tree =>
            if (failOnUnsupportedTree) unsupportedTree(tree)
            else super.printTree(tree) // best effort fallback
        }
      }

      // This code has to be invented from scratch because TreePrinter did not dealt with types.

      private def printTypePrefix(pre: Type, sym: Symbol, printThisType: Boolean = true): Unit = pre match {
        // Do nothing - there is a difference between NoPrefix and NoType, we should safely skip printing on NoPrefix.
        case NoPrefix =>
        // Do nothing - no need to prepend `<root>`.
        case root if root.typeSymbol.isRootSymbol =>
        // Do nothing - $anon was removed so we need to remove the prefix as well
        case TypeRef(AnonThisType(), _, _) =>
        case AnonThisType()                =>
        // Skip pre[.this].sym UNLESS it is a module or a package.
        case TypeRef(_: ThisType, _, _) if !printThisType =>
        case _: ThisType if !printThisType                =>
        // Skip `package` - no use in having .`package`. in the middle of type.
        case TypeRef(pre2, pkg, _) if super.printedName(pkg.name, decoded = true) == "`package`" =>
          printTypePrefix(pre2, sym)
        case SingleType(pre2, sym2) if super.printedName(sym2.name, decoded = true) == "`package`" =>
          printTypePrefix(pre2, sym2)
        // When we have `import x.sth as importedName`, we would print `importedName.value`, even though, it should be just `importedName`.
        // case SingleType(SingleType(NoPrefix, importedSym), _) if importedSym.keyString.isEmpty =>
        //   // scala.Predef.println(s"""Our case: $pre
        //   // |tree: ${showRaw(pre)}
        //   // |sym: $sym2
        //   // |isType: ${sym2.isType}
        //   // |isTerm: ${sym2.isTerm}
        //   // |isMethod: ${sym2.isMethod}
        //   // |isModule: ${sym2.isModule}
        //   // |isModuleClass: ${sym2.isModuleClass}
        //   // |isClass: ${sym2.isClass}
        //   // |isPackage: ${sym2.hasPackageFlag}
        //   // |""")
        //   scala.Predef.println(s"printing $importedSym instead of $pre")
        //   printTypePrefix(NoPrefix, importedSym)
        // Probably something was imported and if we stopped now, we would be missing the prefix, so we should recreate it.
        case NoType =>
          val reconstructedPrefix = sym.fullName.split('.').init.filterNot(Set("`<root>`", "`package`"))
          if (reconstructedPrefix.nonEmpty) print(reconstructedPrefix.mkString("."), ".")
        // Normal case - just print the prefix.
        case _ =>
          val (wrapWithParens, useHash) = pre match {
            case _: RefinedType => (true, true)
            case _: TypeRef     => (false, sym.isAbstractType)
            case _              => (false, false)
          }
          if (wrapWithParens) print("(")
          processTypePrinting(pre, isLastInChain = false)
          if (wrapWithParens) print(")")
          if (useHash) print("#") else print(".")
      }

      private def printTypeNameFromSymbol(sym: Symbol, isLastInChain: Boolean): Unit = {
        val name = printedName(sym.name, decoded = true)
        val isEnumValue = sym.isJavaEnum && javaEnumRegexpFormat.matches(sym.toString)
        print(name)
        // We might need to add .type if sym.name was NOT ending with "$", but sym indicates that it is an object.
        val needsDotType =
          isLastInChain && (sym.isTerm || sym.isModuleOrModuleClass || isEnumValue) && !name.endsWith(".type")
        if (needsDotType) print(".type")
      }

      private def processTypePrinting(tpe: Type, isLastInChain: Boolean): Unit = tpe match {
        // Added, as it was not handled in the original TreePrinter.
        case FoldableConstantType(k @ Constant(s: String)) if s.contains(Chars.LF) =>
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

        case FoldableConstantType(x) =>
          x.value match {
            case sym: Symbol =>
              printTypePrefix(NoType, sym)
              printTypeNameFromSymbol(sym, isLastInChain)
            case primitive =>
              // processing Float constants
              val suffix = primitive match {
                case _: Float => "f" // Note: originally: "F"
                case _        => ""
              }
              val result = s"${x.escapedStringValue}$suffix"
              print(if (x.value.isInstanceOf[String]) highlightString(result) else highlightLiteral(result))
          }

        case RefinedType(parents, decls) =>
          val it = parents.iterator
          while (it.hasNext) {
            print(it.next())
            if (it.hasNext) print(" " + highlightKeyword("with") + " ")
          }
          if (decls.nonEmpty) {
            print(" {\n")
            decls.foreach { symbol =>
              print(
                "  ",
                highlightKeyword(symbol.keyString) + " " + highlightTypeDef(symbol.name.decoded),
                highlightTypeDef(symbol.infoString(symbol.info)),
                "\n"
              )
            }
            print("}")
          }

        case AnnotatedType(annot, arg) =>
          print(arg)
          annot.foreach { a =>
            // a.original looks better but starts with "@new annotation" instead of just "@annotation"
            a.original match {
              // best effort fix
              case Apply(Select(New(tree), termNames.CONSTRUCTOR), args) =>
                print(" @", Apply(tree, args))
              // something that we know works always
              case _ =>
                print(" @", a)
            }
          }

        case TypeRef(pre, sym, args) =>
          // There are cases like `trait SomeTrait` defined inside a class/method:
          //  - we should print them as `SomeTrait` when printing expression that uses them,
          //  - but we want `com.package.name.OuterClass.SomeTrait` when printing type signature of the trait
          val weShouldSkipThisType = sym.isDeferred
          val preToUse = if (notRunnableExprResult && pre == NoPrefix) NoType else pre

          printTypePrefix(preToUse, sym, printThisType = !weShouldSkipThisType)
          printTypeNameFromSymbol(sym, isLastInChain)
          if (args.nonEmpty) {
            print("[")
            val it = args.iterator
            while (it.hasNext) {
              val next = it.next()
              // Prevents assertionErrors when printing stubs used by Cross-Quotes.
              print(bestEffort(next.dealias)(next)) // TODO: why it does not propagate?
              if (it.hasNext) print(", ")
            }
            print("]")
          }

        case SingleType(pre, sym) =>
          printTypePrefix(pre, sym)
          printTypeNameFromSymbol(sym, isLastInChain)

        case ThisType(sym) =>
          printTypePrefix(NoType, sym)
          printTypeNameFromSymbol(sym, isLastInChain)

        case SuperType(thisType, superType) =>
          // This one I am not sure about
          printTypePrefix(thisType, superType.typeSymbol) // we want to print the full name of the symbol
          printTypeNameFromSymbol(superType.typeSymbol, isLastInChain)

        case PolyType(typeParams, resultType) if tpe.toString.startsWith("[") =>
          // Handle type lambdas in Scala 2 (structural type syntax)
          print("({type λ[")
          val it = typeParams.iterator
          while (it.hasNext) {
            val sym = it.next()
            printTypeNameFromSymbol(sym, isLastInChain = false) // We do not want to print .type here
            if (it.hasNext) print(", ")
          }
          print("] = ", resultType, "})#λ")

        case _ =>
          // $COVERAGE-OFF$ If we knew when it happens, we would handle it, so that it wouldn't happen in the first place.
          if (failOnUnsupportedTree) unsupportedType(tpe)
          else {
            // best effort fallback
            def helper(tpe: Type): String =
              tpe.toString match {
                case javaEnumRegexpFormat(enumName, valueName) if tpe.typeSymbol.isJavaEnum =>
                  s"${highlightTypeDef(enumName)}.${highlightValDef(valueName)}.type"
                case _ =>
                  val tpes = tpe.typeArgs.map(helper)
                  val tpeArgs = if (tpes.isEmpty) "" else s"[${tpes.mkString(", ")}]"
                  val dealiased = tpe.dealias
                  val naiveAttempt = dealiased.toString.takeWhile(_ != '[') // does not work for e.g. primitive types
                  def typeSymbolAttempt = dealiased.typeSymbol.fullName // does not work for e.g. path-dependent types
                  val fullName = if (naiveAttempt.exists(_ == '.')) naiveAttempt else typeSymbolAttempt
                  highlightTypeDef(fullName) + tpeArgs
              }
            print(helper(tpe)) // TODO: is this even printed?
          }
        // $COVERAGE-ON$
      }

      override def print(args: Any*): Unit = args foreach {
        case tpe: Type  => processTypePrinting(bestEffort(tpe.dealias)(tpe), isLastInChain = true)
        case tree: Tree => processTreePrinting(tree)
        case arg        => super.print(arg)
      }

      // Prevents assertionErrors when printing stubs used by Cross-Quotes.
      private def bestEffort[A](attempt: => A)(fallback: => A): A =
        try
          attempt
        catch {
          case _: Throwable => fallback
        }

      // $COVERAGE-OFF$ Should only be triggered by error we don't know about
      private def codeForError(tree: Tree): String =
        Option(tree).map(showCode(_)).filter(_.nonEmpty).getOrElse("<no tree ?>")

      private def unsupportedTree(tree: Tree): Nothing = throw new UnsupportedOperationException(
        s"""Unsupported tree for `showCodePretty`:
           |
           |${Option(tree).map(showCode(_)).filter(_.nonEmpty).getOrElse("<no tree ?>")}
           |
           |constructed as:
           |
           |${showRawPretty(tree, SyntaxHighlight.ANSI)}
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

      private def unsupportedType(tpe: Type): Nothing = throw new UnsupportedOperationException(
        s"""Unsupported type for `showCodePretty`:
           |
           |${Option(tpe).map(_.toString()).filter(_.nonEmpty).getOrElse("<no type ?>")}
           |
           |constructed as:
           |
           |${showRawPretty(tpe, SyntaxHighlight.ANSI)}
           |
           |inside:
           |
           |${parentsStack.iterator.map(showCode(_)).filter(_.nonEmpty).nextOption().getOrElse("<no parents ?>")}
           |
           |(Code in this error message rendered using `toString` instead of `showCodePretty`)
           |
           |Please, report an issue at https://github.com/MateuszKubuszok/hearth/issues
           |""".stripMargin
      )
      // $COVERAGE-ON$
    }

    /** Better implementation of [[scala.reflect.internal.Printers#RawTreePrinter]] that supports syntax highlighting
      * and indentation.
      *
      * It skips printing type footnotes and mirrors.
      */
    final class HighlighedRawTreePrinter(out: PrintWriter, syntaxHighlight: SyntaxHighlight, sb: StringBuilder)
        extends InternalTreePrinter(out) {
      import syntaxHighlight.*

      private var indentLevel = 0

      private def indented[A](body: => A): A = {
        indentLevel += 1
        val result = body
        indentLevel -= 1
        result
      }

      // This code was copy-paste-edited from scala.reflect.internal.Printers#RawTreePrinter.

      // private[this] var depth = 0
      // private[this] var printTypesInFootnotes = true
      // private[this] var printingFootnotes = false
      // private[this] val footnotes = new Footnotes()

      @nowarn
      override def print(args: Any*): Unit = {
        // // don't print type footnotes if the argument is a mere type
        // if (depth == 0 && args.length == 1 && args(0) != null && args(0).isInstanceOf[Type])
        //   printTypesInFootnotes = false

        // depth += 1
        args foreach {
          // To prevent "UTF8 string too large" errors, we limit the size of the StringBuilder.
          case arg if sb.length > stringBuilderSoftLimit =>
            // To test the whole tree, we can clear the StringBuilder and continue printing.
            // The result makes no sense, but we can look for exceptions on unhandled cases.
            // $COVERAGE-OFF$
            if (areWeInTests) {
              sb.clear()
              print(arg)
            } else throw StringBuildingTerminated(sb)
          // $COVERAGE-ON$
          case null          => print(highlightLiteral("null"))
          case expr: Expr[?] =>
            print(highlightTypeDef("Expr"))
            if (printTypes) print(expr.staticType)
            print("(")
            indented {
              print('\n', expr.tree)
            }
            print('\n', ")")
          case EmptyTree =>
            print(highlightValDef("EmptyTree"))
          case st.noSelfType =>
            print(highlightValDef("noSelfType"))
          case st.pendingSuperCall =>
            print(highlightValDef("pendingSuperCall"))
          case name: Name =>
            print(show(name))
          case typeTree: TypeTree if typeTree.tpe != null =>
            print(highlightTypeDef("TypeTree"), "(")
            indented {
              print('\n', typeTree.tpe)
            }
            print('\n', ")")
          case tree: Tree =>
            def hasSymbolField = tree.hasSymbolField && tree.symbol != NoSymbol
            val isError = hasSymbolField && (tree.symbol.name string_== nme.ERROR)
            printProduct(
              tree,
              preamble = _ => {
                if (printPositions) print(tree.pos.show)
                print(highlightTypeDef(tree.productPrefix))
                if (printTypes && tree.tpe != null) print(highlightTypeDef(tree.tpe.toString))
              },
              body = {
                case name: Name =>
                  if (isError) {
                    if (isError) print("<")
                    print(name)
                    if (isError) print(": error>")
                  } else if (hasSymbolField) {
                    tree match {
                      case refTree: RefTree =>
                        if (tree.symbol.name != refTree.name)
                          print(
                            "[",
                            tree.symbol,
                            " aka ",
                            refTree.name,
                            "]"
                          )
                        else print(tree.symbol)
                      case defTree: DefTree =>
                        print(tree.symbol)
                      case _ =>
                        print(tree.symbol.name)
                    }
                  } else {
                    print(name)
                  }
                case k @ Constant(_: String) =>
                  print(highlightTypeDef("Constant") + "(" + highlightString(k.escapedStringValue) + ")")
                case Constant(null) =>
                  print(highlightTypeDef("Constant") + "(" + highlightLiteral("null") + ")")
                case k @ Constant(value) =>
                  print(highlightTypeDef("Constant") + "(" + highlightLiteral(k.escapedStringValue) + ")")
                case arg =>
                  print(arg)
              },
              postamble = {
                case tree @ TypeTree() if tree.original != null => print(".setOriginal(", tree.original, ")")
                case _                                          => // do nothing
              }
            )
          case sym: Symbol =>
            if (sym == NoSymbol) print(highlightValDef("NoSymbol"))
            else
              print(
                highlightTripleQs,
                " /* Symbol of ", {
                  val kw = sym.keyString
                  if (kw.isEmpty) ""
                  else (highlightKeyword(kw) + " ")
                },
                if (sym.isStatic && (sym.isClass || sym.isModule)) highlightTypeDef(sym.fullName)
                else highlightValDef(sym.name.decoded.toString), {
                  val f = sym.debugFlagString
                  if (f.isEmpty) ""
                  else s" (flags: ${highlightKeyword(f)})"
                },
                " */"
              )
            if (printIds) print("#", highlightValDef(sym.id.toString))
            if (printOwners) print("@", highlightValDef(sym.owner.id.toString))
          // if (printKinds) print("#", sym.abbreviatedKindString)
          // if (printMirrors) print("%M", footnotes.put[scala.reflect.api.Mirror[_]](mirrorThatLoaded(sym)))
          case tag: TypeTag[?] =>
            print(highlightTypeDef("TypeTag") + "(")
            indented {
              print(tag.tpe)
            }
            print(")")
          case tag: WeakTypeTag[?] =>
            print(highlightTypeDef("WeakTypeTag") + "(")
            indented {
              print(tag.tpe)
            }
            print(")")
          case tpe: Type =>
            // val defer = printTypesInFootnotes && !printingFootnotes
            // if (defer) print("[", footnotes.put(tpe), "]")
            /*else*/
            tpe match {
              case NoType   => print(highlightValDef("NoType"))
              case NoPrefix => print(highlightValDef("NoPrefix"))
              case _        => printProduct(tpe.asInstanceOf[Product])
            }
          case mods: Modifiers =>
            print(highlightTypeDef("Modifiers") + "(")
            if (mods.flags != NoFlags || mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty)
              print(show(mods.flags))
            if (mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty) {
              print(", "); print(mods.privateWithin)
            }
            if (mods.annotations.nonEmpty) { print(", "); print(mods.annotations); }
            print(")")
          case name: Name =>
            print(show(name))
          case scope: Scope =>
            print(highlightTypeDef("Scope"))
            printIterable(scope.toList)
          case list: List[?] =>
            print(highlightTypeDef("List"))
            printIterable(list)
          case product: Product =>
            printProduct(product)
          case arg =>
            out.print(arg.toString.replaceAll("\n", "\n" + "  " * indentLevel))
        }
        // depth -= 1
        /*
        if (depth == 0 && !printingFootnotes) {
          printingFootnotes = true
          footnotes.print[Type](this)
          footnotes.print[scala.reflect.api.Mirror[_]](this)
          printingFootnotes = false
        }
         */
      }

      // This code was copy-paste-edited from scala.reflect.internal.Printers.

      def printProduct(
          p: Product,
          preamble: Product => Unit = p => print(highlightTypeDef(p.productPrefix)),
          body: Any => Unit = print(_),
          postamble: Product => Unit = p => print("")
      ): Unit = {
        preamble(p)
        printIterable(p.productIterator.toList, body = body)
        postamble(p)
      }

      def printIterable(
          iterable: List[?],
          preamble: => Unit = print(""),
          body: Any => Unit = print(_),
          postamble: => Unit = print("")
      ): Unit = {
        preamble
        print("(")
        indented {
          val it = iterable.iterator
          if (iterable.nonEmpty) {
            print('\n')
          }
          while (it.hasNext) {
            body(it.next())
            print(if (it.hasNext) ",\n" else "")
          }
        }
        if (iterable.nonEmpty) {
          print('\n')
        }
        print(")")
        postamble
      }

      def show(name: Name): String = name match {
        case tpnme.WILDCARD      => highlightValDef("typeNames") + "." + highlightValDef("WILDCARD")
        case tpnme.EMPTY         => highlightValDef("typeNames") + "." + highlightValDef("EMPTY")
        case tpnme.ERROR         => highlightValDef("typeNames") + "." + highlightValDef("ERROR")
        case tpnme.PACKAGE       => highlightValDef("typeNames") + "." + highlightValDef("PACKAGE")
        case tpnme.WILDCARD_STAR => highlightValDef("typeNames") + "." + highlightValDef("WILDCARD_STAR")
        case nme.WILDCARD        => highlightValDef("termNames") + "." + highlightValDef("WILDCARD")
        case nme.EMPTY           => highlightValDef("termNames") + "." + highlightValDef("EMPTY")
        case nme.ERROR           => highlightValDef("termNames") + "." + highlightValDef("ERROR")
        case nme.PACKAGE         => highlightValDef("termNames") + "." + highlightValDef("PACKAGE")
        case nme.CONSTRUCTOR     => highlightValDef("termNames") + "." + highlightValDef("CONSTRUCTOR")
        case nme.ROOTPKG         => highlightValDef("termNames") + "." + highlightValDef("ROOTPKG")
        case _                   =>
          highlightTypeDef(if (name.isTermName) "TermName" else "TypeName") + "(" +
            highlightString("\"" + name.toString + "\"") + ")"
      }

      def show(flags: FlagSet): String =
        if (flags == NoFlags) highlightValDef(nme.NoFlags.toString)
        else {
          val s_flags = new scala.collection.mutable.ListBuffer[String]
          def hasFlag(left: Long, right: Long): Boolean = (left & right) != 0
          for (i <- 0 to 63 if hasFlag(flags, 1L << i))
            s_flags += highlightValDef(flagToString(1L << i).replace("<", "").replace(">", "").toUpperCase)
          s_flags mkString " | "
        }

      def show(position: Position): String =
        position.show

      def showDecl(sym: Symbol): String = {
        if (!isCompilerUniverse) definitions.fullyInitializeSymbol(sym)
        sym.defString // TODO: check if we can improve this?
      }
    }
  }
}
