import sbt.*
import sbt.Keys.*
import sbt.internal.ProjectMatrix
import org.scalajs.sbtplugin.ScalaJSPlugin
import scala.scalanative.sbtplugin.ScalaNativePlugin
import sbtide.Keys.ideSkipProject
import org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile
import kubuszok.sbt.KubuszokPlugin.autoImport.isCI

object Scala2Rows {

  def allPlatforms(
      pm: ProjectMatrix,
      scala2Axis: VirtualAxis.ScalaVersionAxis,
      ideSkip: String => Boolean,
      configJvm: Project => Project = identity,
      configJs: Project => Project = identity,
      configNat: Project => Project = identity
  ): ProjectMatrix = {
    def ideSettings(platform: String): Seq[Setting[?]] = {
      val skip = ideSkip(platform)
      Seq(ideSkipProject := skip, bspEnabled := !skip, scalafmtOnCompile := !skip && !isCI)
    }
    pm.customRow(true, Seq(scala2Axis, VirtualAxis.jvm), (p: Project) => configJvm(p).settings(ideSettings("jvm")*))
      .customRow(
        true,
        Seq(scala2Axis, VirtualAxis.js),
        (p: Project) => configJs(p).enablePlugins(ScalaJSPlugin).settings(ideSettings("js")*)
      )
      .customRow(
        true,
        Seq(scala2Axis, VirtualAxis.native),
        (p: Project) => configNat(p).enablePlugins(ScalaNativePlugin).settings(ideSettings("native")*)
      )
  }

  def jvmOnly(
      pm: ProjectMatrix,
      scala2Axis: VirtualAxis.ScalaVersionAxis,
      ideSkip: String => Boolean,
      configure: Project => Project = identity
  ): ProjectMatrix = {
    val skip = ideSkip("jvm")
    pm.customRow(
      true,
      Seq(scala2Axis, VirtualAxis.jvm),
      (p: Project) =>
        configure(p).settings(ideSkipProject := skip, bspEnabled := !skip, scalafmtOnCompile := !skip && !isCI)
    )
  }
}
