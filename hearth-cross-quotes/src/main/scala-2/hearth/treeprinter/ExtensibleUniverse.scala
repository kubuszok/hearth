package hearth
package treeprinter

import scala.reflect.ClassTag
import scala.reflect.internal.{Reporter, TreeInfo}
import scala.reflect.internal.util.Statistics

/** Allows using parts of c.Universe that are not exposed by the public API.
  *
  * @param u
  *   universe to wrap
  */
class ExtensibleUniverse(u: scala.reflect.macros.Universe) extends scala.reflect.internal.SymbolTable {

  private lazy val delegatedUniverse: scala.reflect.internal.SymbolTable =
    u.asInstanceOf[scala.reflect.internal.SymbolTable]

  private def forward[A](f: delegatedUniverse.type => Any): A = f(delegatedUniverse).asInstanceOf[A]

  private def forwardReflect[A](methodName: String): (Seq[Any] => A) = {
    val clazz = delegatedUniverse.getClass
    val method = clazz.getMethod(methodName)
    method.setAccessible(true)
    (args: Seq[Any]) => method.invoke(delegatedUniverse, args*).asInstanceOf[A]
  }

  override def isCompilerUniverse = true

  // Members declared in scala.reflect.internal.FreshNames
  def currentFreshNameCreator: scala.reflect.internal.util.FreshNameCreator = forward(_.currentFreshNameCreator)

  // Members declared in scala.reflect.api.ImplicitTags
  implicit val TreeCopierTag: ClassTag[TreeCopier] = forward[ClassTag[TreeCopier]](_.TreeCopierTag)

  implicit val RuntimeClassTag: ClassTag[RuntimeClass] = forward[ClassTag[RuntimeClass]](_.RuntimeClassTag)

  implicit val MirrorTag: ClassTag[Mirror] = forward[ClassTag[Mirror]](_.MirrorTag)

  // Members declared in scala.reflect.api.Mirrors
  val rootMirror: Mirror = forward[Mirror](_.rootMirror)

  // Members declared in scala.reflect.internal.Reporting
  def reporter: Reporter = forward[Reporter](_.reporter)

  def currentRun: RunReporting = forward[RunReporting](_.currentRun)

  protected def PerRunReporting: PerRunReporting = PerRunReportingImpl(Seq.empty)
  private def PerRunReportingImpl = forwardReflect[PerRunReporting]("PerRunReporting")

  // Members declared in scala.reflect.internal.SymbolTable
  def currentRunId: RunId = forward[RunId](_.currentRunId)

  def erasurePhase: scala.reflect.internal.Phase = forward[scala.reflect.internal.Phase](_.erasurePhase)

  def log(msg: => AnyRef): Unit = forward[Unit](_.log(msg))

  def mirrorThatLoaded(sym: Symbol): Mirror =
    forward[Mirror](_.mirrorThatLoaded(sym.asInstanceOf[delegatedUniverse.Symbol]))

  def picklerPhase: scala.reflect.internal.Phase = forward[scala.reflect.internal.Phase](_.picklerPhase)

  def settings: scala.reflect.internal.settings.MutableSettings =
    forward[scala.reflect.internal.settings.MutableSettings](_.settings)

  val statistics: Statistics & ReflectStats = forward[Statistics & ReflectStats](_.statistics)

  val treeInfo: TreeInfo { val global: ExtensibleUniverse.this.type } =
    forward[TreeInfo { val global: ExtensibleUniverse.this.type }](_.treeInfo)

  // Members declared in scala.reflect.internal.Trees
  def newLazyTreeCopier: TreeCopier = forward[TreeCopier](_.newLazyTreeCopier)

  def newStrictTreeCopier: TreeCopier = forward[TreeCopier](_.newStrictTreeCopier)
}
