package hearth
package treeprinter

import scala.reflect.ClassTag
import scala.reflect.internal.{Reporter, TreeInfo}
import scala.reflect.internal.util.Statistics

// Allows using parts of c.Universe that are not exposed by the public API

class ExtensibleUniverse(u: scala.reflect.macros.Universe) extends scala.reflect.internal.SymbolTable {

  private lazy val delegatedUniverse: scala.reflect.internal.SymbolTable =
    u.asInstanceOf[scala.reflect.internal.SymbolTable]

  override def isCompilerUniverse = true

  // Members declared in scala.reflect.internal.FreshNames
  def currentFreshNameCreator: scala.reflect.internal.util.FreshNameCreator =
    delegatedUniverse.currentFreshNameCreator

  // Members declared in scala.reflect.api.ImplicitTags
  implicit val TreeCopierTag: ClassTag[TreeCopier] = delegatedUniverse.TreeCopierTag.asInstanceOf[ClassTag[TreeCopier]]
  implicit val RuntimeClassTag: ClassTag[RuntimeClass] =
    delegatedUniverse.RuntimeClassTag.asInstanceOf[ClassTag[RuntimeClass]]
  implicit val MirrorTag: ClassTag[Mirror] = delegatedUniverse.MirrorTag.asInstanceOf[ClassTag[Mirror]]

  // Members declared in scala.reflect.api.Mirrors
  val rootMirror: Mirror = delegatedUniverse.rootMirror.asInstanceOf[Mirror]

  // Members declared in scala.reflect.internal.Reporting
  def reporter: Reporter = delegatedUniverse.reporter.asInstanceOf[Reporter]
  def currentRun: RunReporting = delegatedUniverse.currentRun.asInstanceOf[RunReporting]
  protected def PerRunReporting: PerRunReporting = new PerRunReportingBase {
    def deprecationWarning(pos: Position, msg: String, since: String, site: String, origin: String): Unit = ()
    def deprecationWarning(
        pos: ExtensibleUniverse.this.Position,
        msg: String,
        since: String,
        site: String,
        origin: String,
        actions: List[scala.reflect.internal.util.CodeAction]
    ): Unit = ()
  }.asInstanceOf[PerRunReporting]

  // Members declared in scala.reflect.internal.SymbolTable
  def currentRunId: RunId = delegatedUniverse.currentRunId
  def erasurePhase: scala.reflect.internal.Phase = delegatedUniverse.erasurePhase
  def log(msg: => AnyRef): Unit = delegatedUniverse.log(msg)
  def mirrorThatLoaded(sym: Symbol): Mirror =
    delegatedUniverse.mirrorThatLoaded(sym.asInstanceOf[delegatedUniverse.Symbol]).asInstanceOf[Mirror]
  def picklerPhase: scala.reflect.internal.Phase = delegatedUniverse.picklerPhase
  def settings: scala.reflect.internal.settings.MutableSettings = delegatedUniverse.settings
  val statistics: Statistics & ReflectStats = delegatedUniverse.statistics.asInstanceOf[Statistics & ReflectStats]
  val treeInfo: TreeInfo { val global: ExtensibleUniverse.this.type } =
    delegatedUniverse.treeInfo.asInstanceOf[TreeInfo { val global: ExtensibleUniverse.this.type }]

  // Members declared in scala.reflect.internal.Trees
  def newLazyTreeCopier: TreeCopier = delegatedUniverse.newLazyTreeCopier.asInstanceOf[TreeCopier]
  def newStrictTreeCopier: TreeCopier = delegatedUniverse.newStrictTreeCopier.asInstanceOf[TreeCopier]
}
