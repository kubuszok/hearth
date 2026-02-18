import sbt.*
import sbt.Keys.*

/** AutoPlugin for generating arity-based Hearth source files.
  *
  * Enable on modules that need generated sources (e.g. `hearth`, `hearthCrossQuotes`, `hearthTests`). Override the
  * `generateHearthSources` task to specify which generators to run.
  */
object SourceGenPlugin extends AutoPlugin {
  object autoImport {
    val generateHearthSources = taskKey[Seq[File]]("Generate arity-based Hearth sources")
  }
  import autoImport.*

  // Must be explicitly enabled per module
  override def trigger = noTrigger

  override def projectSettings: Seq[Def.Setting[?]] = Seq(
    // Default: generate nothing. Override per-module.
    generateHearthSources := Seq.empty[File],
    // Wire generated sources into compilation
    Compile / sourceGenerators += generateHearthSources.taskValue
  )
}
