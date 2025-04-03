package hearth
package cq

import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.*
import dotc.*
import dotc.ast.tpd.*
import core.Contexts.*
import core.Definitions
import core.Names.*
import core.Symbols.*
import core.Types.*
import core.StdNames.*
import core.Constants.Constant
import core.Flags.*
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.parsing.Parser
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.untpd.UntypedTreeTraverser
import dotty.tools.dotc.ast.untpd.UntypedTreeMap

// https://gist.github.com/WojciechMazur/029ec8fa915bcd099fa41d2dfcbf6cd6

object CrossQuotesPlugin {
  val name = "cross-quotes"
  val description = "Rewrites Expr.quote/splice into native quotes"

  def init(options: List[String]): List[PluginPhase] = List(new CrossQuotesPhase)
}

final class CrossQuotesPhase extends PluginPhase {
  override def runsAfter: Set[String] = Set(Parser.name)
  override def runsBefore: Set[String] = Set("typer")
  override def phaseName: String = "hearth:cross-quotes"

  override def changesMembers: Boolean = true
  override def run(using Context): Unit = {
    ctx.compilationUnit.untpdTree = new UntypedTreeMap {
      override def transform(tree: untpd.Tree)(using Context): untpd.Tree =
        tree match {
          /*
          case td @ untpd.TypeDef(_, templ: Template)
          if templ.parents.exists { case name: Ident => name.name == typeName("CUnion") } =>
            val applyMethods = templ.constr.paramss.head.map: param =>
              val paramC: ParamClause = param match {
                case vd: ValDef => cpy.ValDef(vd)().withMods(untpd.Modifiers(flags = Flags.Param)) :: Nil
              }
              val params: List[ParamClause] = List(paramC)
              untpd.DefDef(nme.apply, paramss = params, tpt = untpd.Ident(td.name), rhs = untpd.ref(defn.Predef_undefined))
            cpy.TypeDef(td)(rhs =
              cpy.Template(templ)(body =
                templ.body ++ applyMethods.toList
              )
            )
           */
          case t => super.transform(t)
        }
    }.transform(ctx.compilationUnit.untpdTree)
    super.run
  }
}
