package hearth
package cq

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.{Global, Phase}

class CrossQuotesPlugin(val global: Global) extends Plugin { plugin =>
  val name = "cross-quotes"
  val description = "Rewrites Expr.quote/splice into native quotes"

  val components = List[PluginComponent](CrossQuotesComponent)

  import global.*

  private object CrossQuotesComponent extends PluginComponent {
    override val global: plugin.global.type = plugin.global
    override val phaseName = "hearth:cross-quotes"
    override def description = plugin.description

    val runsAfter = List[String]("refchecks") // TODO: check if this is correct, should be typecheck

    def newPhase(prev: Phase) = new CrossQuotesPhase(prev)

    final class CrossQuotesPhase(prev: Phase) extends StdPhase(prev) {
      override def name = phaseName

      override def apply(unit: CompilationUnit): Unit =
        ()
    }
  }
}
