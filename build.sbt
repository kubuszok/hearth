import sbtwelcome.UsefulTask
import commandmatrix.extra.*
import kubuszok.sbt._
import kubuszok.sbt.KubuszokPlugin.autoImport._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._
import PipeOps._

// Versions:

val versions = new {
  // Versions we are publishing for.
  val scala213 = "2.13.16"
  val scala3 = "3.3.8"

  // Regression test versions (always-visible tier rows, not env-var-controlled).
  val scala213NextLts = "2.13.18"
  val scala3NextLts = "3.9.0"
  val scala3Next = "3.10.0-RC1"

  // Which versions should be cross-compiled for publishing.
  val scalas = List(scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  val kindProjector = "0.13.4"
  val munit = "1.3.6"
  val scalacheck = "1.20.0"
  val scalaXml = "2.4.0"
}

// Scala 3 is the default axis (no suffix in project IDs). Scala 2.13 gets the "2" suffix.
val scala3DefaultAxes: Seq[VirtualAxis] = Seq(VirtualAxis.jvm, VirtualAxis.scalaABIVersion("3.0.0"))

// Scala 2 axis with short "2" suffix (someVariations would produce "2_13" from the binary version).
val scala2Axis: VirtualAxis.ScalaVersionAxis = VirtualAxis.scalaVersionAxis(versions.scala213, "2")

// Tier axes for regression testing. Test modules declare these alongside the LTS scalaVersion axis,
// then override scalaVersion to the regression version in their row's configure callback.
val tierNextLts = ScalaTierAxis("NextLts", "next-lts")
val tierNext = ScalaTierAxis("Next", "next")

// Version-range helpers for scalacOptions.
def scala3Minor(sv: String): Option[Int] =
  if (sv.startsWith("3.")) Some(sv.stripPrefix("3.").takeWhile(_.isDigit).toInt) else None
def isScala3RequiresJdk17(sv: String): Boolean = scala3Minor(sv).exists(_ >= 8)
def isScala3PostLts(sv: String): Boolean = scala3Minor(sv).exists(_ >= 5)

// Development settings:

val dev = new DevProperties(
  scala213 = Some(versions.scala213),
  scala3 = Some(versions.scala3),
  platforms = versions.platforms
)

val only1VersionInIDE = dev.only1VersionInIDE :+ MatrixAction
  .ForPlatform(VirtualAxis.jvm)
  .Configure(
    _.settings(
      // There is a bug in Scala Native 0.5.12 which crashes when seeing this flag, so we disable it for non-JVM.
      scalacOptions ++= { if (scalaVersion.value == versions.scala3) Seq("-Yfuture-lazy-vals") else Seq.empty }
    )
  )

// IDE skip predicate for Scala 2 rows.
val scala2IdeSkip: String => Boolean = {
  val ideScala = dev.props.getProperty("ide.scala", "3")
  val idePlatform = dev.props.getProperty("ide.platform", "jvm")
  platform => ideScala != "2.13" || idePlatform != platform
}

val logCrossQuotes =
  dev.props.getProperty("log.cross-quotes") match {
    case "true"                          => true
    case "false"                         => false
    case otherwise if otherwise.nonEmpty => otherwise
    case _                               => !isCI
  }

// Common settings:

// Allow munit's Scala 2.13.18 dependency when we compile with 2.13.16 (backwards compatible per SIP-51)
// and Scala.js 1.21.0 dependency (it requires 2.13.17)
Global / allowUnsafeScalaLibUpgrade := true

// The hearth-cross-quotes on Scala 3 are plugins (JVM-only). Scala 2 cross-quotes are macros
// (all platforms) — wired manually in each module's scala2Rows configure callback.
val defineCrossQuotes = List(
  MatrixAction {
    case (version, List(VirtualAxis.js))     => version.isScala3
    case (version, List(VirtualAxis.native)) => version.isScala3
    case _                                   => false
  }.Skip
)

// Cross-quotes Scala 3 compiler plugin wiring (someVariations only handles Scala 3 now).
val useCrossQuotes = List(
  MatrixAction
    .ForScala(_.isScala3)
    .Configure(
      _.settings(
        scalacOptions ++= {
          val jar = (hearthCrossQuotes.jvm(versions.scala3) / Compile / packageBin).value
          Seq(
            s"-Xplugin:${jar.getAbsolutePath}",
            s"-Jdummy=${jar.lastModified}",
            s"-P:hearth.cross-quotes:logging=$logCrossQuotes"
          )
        }
      )
    )
)

// Scala 2 cross-quotes wiring per platform (passed to scala2Rows configure callbacks).
def scala2CqJvm(p: Project): Project =
  p.dependsOn(hearthCrossQuotes.jvm(versions.scala213))
    .settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=$logCrossQuotes")
def scala2CqJs(p: Project): Project =
  p.dependsOn(hearthCrossQuotes.js(versions.scala213))
    .settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=$logCrossQuotes")
def scala2CqNative(p: Project): Project =
  p.dependsOn(hearthCrossQuotes.native(versions.scala213))
    .settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=$logCrossQuotes")

val settings = Seq(
  scalacOptions ++= {
    val sv = scalaVersion.value
    foldVersion(sv)(
      for3 = Seq(
        // format: off
        "-encoding", "UTF-8",
        "-release", if (isScala3RequiresJdk17(sv)) "17" else "11",
        // format: on
      ) ++ (if (sv == versions.scala3) Seq("-rewrite", "-source", "3.3-migration")
            else Seq()) ++ Seq(
        "-unchecked",
        "-deprecation",
        "-explain",
        "-explain-cyclic",
        "-explain-types",
        "-feature",
        "-no-indent",
        "-Wconf:msg=Unreachable case:s",
        "-Wconf:msg=Missing symbol position:s",
        "-Wconf:msg=should be provided with a .using. clause:s",
        "-Wconf:msg=with as a type operator has been deprecated:s",
        "-Wnonunit-statement",
        "-Wunused:privates",
        "-Wunused:locals",
        "-Wunused:explicits",
        "-Wunused:implicits",
        "-Wunused:params",
        "-Wvalue-discard",
        "-Werror",
        "-Xcheck-macros"
      ) ++ (if (isScala3PostLts(sv)) Seq("-Xkind-projector:underscores")
            else Seq("-Ykind-projector:underscores")),
      for2_13 = Seq(
        // format: off
        "-encoding", "UTF-8",
        "-release", "11",
        // format: on
        "-unchecked",
        "-deprecation",
        "-explaintypes",
        "-feature",
        "-language:higherKinds",
        "-Wconf:cat=scala3-migration:s",
        "-Wconf:cat=deprecation&origin=hearth.*:s",
        "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s",
        "-Wconf:msg=discarding unmoored doc comment:s",
        "-Wconf:msg=Could not find any member to link for:s",
        "-Wconf:msg=Variable .+ undefined in comment for:s",
        "-Wunused:patvars",
        "-Xfatal-warnings",
        "-Xlint:adapted-args",
        "-Xlint:delayedinit-select",
        "-Xlint:doc-detached",
        "-Xlint:inaccessible",
        "-Xlint:infer-any",
        "-Xlint:nullary-unit",
        "-Xlint:option-implicit",
        "-Xlint:package-object-classes",
        "-Xlint:poly-implicit-overload",
        "-Xlint:private-shadow",
        "-Xlint:stars-align",
        "-Xlint:type-parameter-shadow",
        "-Xsource:3",
        "-Yrangepos",
        "-Ywarn-dead-code",
        "-Ywarn-numeric-widen",
        "-Ywarn-unused:locals",
        "-Ywarn-unused:imports",
        "-Ywarn-macros:after",
        "-Ytasty-reader"
      ) ++
        (if (sv == versions.scala213NextLts)
           Seq(
             "-Wconf:msg=a type was inferred to be kind-polymorphic `Nothing` to conform to:s",
             "-Xsource-features:eta-expand-always"
           )
         else Seq.empty)
    )
  }
)

val jvmOnlySettings = Seq(
  scalacOptions ++= { if (scalaVersion.value == versions.scala3) Seq("-Yfuture-lazy-vals") else Seq.empty }
)

// Source directory settings for each test tier. LTS rows include scala-lts* dirs (shared with Next-LTS).
// Next-LTS rows additionally include scala-next-lts* dirs. Next rows include scala-next* dirs only.
def tierSourceDirs(tiers: String*): Seq[Setting[?]] = {
  def resolveDirs(srcDir: java.nio.file.Path, scope: String, sv: String): Seq[File] =
    tiers.flatMap { tier =>
      Seq(srcDir.resolve(s"$scope/scala-$tier").toFile) ++
        foldVersion(sv)(
          for3 = Seq(srcDir.resolve(s"$scope/scala-$tier-3").toFile),
          for2_13 = Seq(srcDir.resolve(s"$scope/scala-$tier-2").toFile)
        )
    }
  Seq(
    Compile / unmanagedSourceDirectories ++= resolveDirs(sourceDirectory.value.toPath, "main", scalaVersion.value),
    Test / unmanagedSourceDirectories ++= resolveDirs(sourceDirectory.value.toPath, "test", scalaVersion.value)
  )
}

val ltsSourceDirs: Seq[Setting[?]] = tierSourceDirs("lts")
val nextLtsSourceDirs: Seq[Setting[?]] = tierSourceDirs("lts", "next-lts")
val nextSourceDirs: Seq[Setting[?]] = tierSourceDirs("next")

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % versions.munit % Test,
    "org.scalacheck" %%% "scalacheck" % versions.scalacheck % Test
  ),
  libraryDependencies ++= foldVersion(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
    )
  )
)

val publishSettings = Seq(
  organization := "com.kubuszok",
  homepage := Some(url("https://scala-hearth.readthedocs.io")),
  organizationHomepage := Some(url("https://kubuszok.com")),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/kubuszok/hearth/"),
      "scm:git:git@github.com:kubuszok/hearth.git"
    )
  ),
  startYear := Some(2025),
  developers := List(
    Developer("MateuszKubuszok", "Mateusz Kubuszok", "", url("https://kubuszok.com"))
  ),
  pomExtra := (
    <issueManagement>
      <system>GitHub issues</system>
      <url>https://github.com/kubuszok/hearth/issues</url>
    </issueManagement>
  ),
  projectType := ProjectType.ScalaLibrary
)

// The last released version we check binary compatibility against. Bump this on every release.
val mimaPreviousVersion = "0.4.2"

val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    val previousVersions = moduleName.value match {
      case "hearth-better-printers" | "hearth-cross-quotes" | "hearth-micro-fp" | "hearth" | "hearth-munit" =>
        Set(mimaPreviousVersion)
      case "hearth-build" | "hearth-tests" | "hearth-sandwich-examples-213" | "hearth-sandwich-examples-3" |
          "hearth-sandwich-tests" | "debug-hearth-better-printers" | "debug-hearth" =>
        Set()
      case name => sys.error(s"All modules should be explicitly checked or ignored for MiMa, missing: $name")
    }
    // `.cross(crossVersion.value)` applies the project's own, platform-aware cross-version (CrossVersion.binary on
    // JVM, ScalaJSCrossVersion.binary on JS, ScalaNativeCrossVersion.binary on Native) so that MiMa compares each
    // artifact against the matching previous one (`hearth_sjs1_2.13`, `hearth_native0.5_2.13`, ...) instead of always
    // resolving the JVM artifact (`hearth_2.13`) - which would report all JVM-only classes as "missing".
    previousVersions.map(v => (organization.value % moduleName.value % v).cross(crossVersion.value))
  },
  mimaFailOnNoPrevious := {
    moduleName.value match {
      case "hearth-better-printers" | "hearth-cross-quotes" | "hearth-micro-fp" | "hearth" | "hearth-munit" =>
        true
      case "hearth-build" | "hearth-tests" | "hearth-sandwich-examples-213" | "hearth-sandwich-examples-3" |
          "hearth-sandwich-tests" | "debug-hearth-better-printers" | "debug-hearth" =>
        false
      case name => sys.error(s"All modules should be explicitly checked or ignored for MiMa, missing: $name")
    }
  },
  // Binary-compatibility exceptions.
  //
  // Hearth's public surface is a set of traits (MacroCommons and the traits it mixes in) that ONLY we implement and
  // that USERS mix into their own macro bundles. For such mix-ins the rule that matters is:
  //
  //   * A NEW TOP-LEVEL member (val/var/object, or an abstract def) added to a mixed-in trait IS breaking: during
  //     linearization the user's bundle must now provide/forward it, so a bundle compiled against the old version
  //     fails to link against the new one. MiMa flags these as ReversedMissingMethodProblem / (Inherited)
  //     NewAbstractMethodProblem - keep them.
  //   * A member added inside a NESTED scope (a method/val/object on a nested class or object, e.g. a method on
  //     `Classes#CaseClass`) is NOT observable by users: the only code that instantiates or mixes in those nested
  //     definitions is Hearth's own, which is evicted together with the interface. A user upgrading gets both the
  //     new interface and its new implementation atomically, so the change can never be witnessed as a link error.
  //     (MiMa does not even report adding a method to a nested *class* - only trait members turn into forwarders.)
  //
  // Only the second kind may be excluded here, and each exclusion must cite why it is not user-observable. See
  // docs/contributing/binary-compatibility-and-mixins.md for the full policy and worked examples.
  //
  // Example of an allowed exclusion (a member added to a NESTED trait/object that MiMa would otherwise flag):
  //   exclude[ReversedMissingMethodProblem]("hearth.typed.SomeTrait#SomeNestedTrait.newHelper")
  mimaBinaryIssueFilters ++= Seq(
    // NOTE: `skipped` briefly went by-name here (filters existed for the String -> Function0 signature change) but
    // that BROKE pre-compiled provider extensions at link time - kindlings' cats-integration threw NoSuchMethodError
    // from its `parse` when loaded by a newer Hearth. The "every extension is recompiled against its Hearth version"
    // justification was wrong: extensions are ServiceLoader-loaded binaries. The strict `skipped(String)` signatures
    // are restored (laziness lives in the new `skippedLazily`), so those filters are gone.
    // #329: `lastMatchProvenance` provider-provenance state added to the NESTED trait `StdExtensions#ProvidedCompanion`.
    // That trait is part of the MacroCommons cake and is implemented ONLY by Hearth's own std companion objects
    // (IsCollection/IsOption/IsEither/IsValueType/CtorLikes), which live in the same trait - so the interface and its
    // implementations are always evicted together and no user has a standalone implementation to break. The public
    // accessor is a `final def`; these two are just the synthetic getter/setter of its `private var` backing field.
    exclude[ReversedMissingMethodProblem](
      "hearth.std.StdExtensions#ProvidedCompanion.hearth$std$StdExtensions$ProvidedCompanion$$lastMatchProvenanceValue"
    ),
    exclude[ReversedMissingMethodProblem](
      "hearth.std.StdExtensions#ProvidedCompanion.hearth$std$StdExtensions$ProvidedCompanion$$lastMatchProvenanceValue_="
    ),
    // #334: `annotated` added to the NESTED trait `Exprs#ExprModule`. That trait is part of the MacroCommons cake and
    // is implemented ONLY by Hearth's own `Expr` object (in ExprsScala2/ExprsScala3), so the interface and its
    // implementation are always evicted together - no user has a standalone `ExprModule` implementation to break.
    exclude[ReversedMissingMethodProblem]("hearth.typed.Exprs#ExprModule.annotated"),
    // Perf: `ClassViewResult.Incompatible` changed from a `case class` to a plain class so its `reason` (an
    // expensive `Type.prettyPrint`, discarded by most callers) can be computed lazily. The former binary surface is
    // preserved as far as shims can reach: `apply(String)`, the primary constructor `this(String)`, and
    // `copy(String)` / `copy$default$1()` are all reintroduced as `private[ClassViewResult]` members, which Scala
    // emits as PUBLIC bytecode (so 0.4.0-compiled callers still link) while hiding them from new source (so new call
    // sites resolve to the lazy `apply(=> String)` and `reason` stays lazy). Those therefore need NO filter.
    //
    // What remains below is STRUCTURAL - a plain class simply cannot carry it, and no shim can restore it without
    // either undoing the laziness or making it a `case class` again:
    //   * `unapply` returns `Some` instead of `Option`, so `case Incompatible(reason)` stays irrefutable and sealed
    //     matches stay exhaustive (returning `Option` would emit non-exhaustiveness warnings in downstream `-Werror`
    //     builds - a worse regression than this filter). Return-type overloading cannot provide both.
    //   * the companion loses its `AbstractFunction1[String, Incompatible]` parent (restoring it would force a PUBLIC
    //     `apply(String)`, reintroducing the overload ambiguity the shim exists to avoid).
    //   * Scala 3 `Mirror.Sum` (+`ordinal`) on `ClassViewResult` and `Mirror.Product` (`_1`, `fromProduct`) on
    //     `Incompatible` - case-class/Mirror machinery a plain class does not generate.
    // `ClassViewResult` is an internal parsing-result type that ONLY Hearth constructs; no user builds one via the
    // synthesized `Function1`/`Mirror`, so none of the below is user-observable.
    exclude[MissingTypesProblem]("hearth.typed.Classes$ClassViewResult$Incompatible$"),
    exclude[IncompatibleResultTypeProblem]("hearth.typed.Classes#ClassViewResult#Incompatible.unapply"),
    exclude[MissingTypesProblem]("hearth.typed.Classes$ClassViewResult$"),
    exclude[DirectMissingMethodProblem]("hearth.typed.Classes#ClassViewResult.ordinal"),
    exclude[DirectMissingMethodProblem]("hearth.typed.Classes#ClassViewResult#Incompatible._1"),
    exclude[DirectMissingMethodProblem]("hearth.typed.Classes#ClassViewResult#Incompatible.fromProduct"),
    // Perf: `Type.Cache` (a type-keyed memo) added to the NESTED trait `Types#TypeModule`, which is part of the
    // MacroCommons cake and implemented ONLY by Hearth's own platform `Type` object (TypesScala2/TypesScala3), so
    // interface and implementation are always evicted together - no user has a standalone `TypeModule` to break.
    exclude[ReversedMissingMethodProblem]("hearth.typed.Types#TypeModule.Cache"),
    // Perf: `unsortedMethods` (methods without the expensive position-resolving `sortMethods` pass) added to the
    // NESTED trait `UntypedMethods#UntypedMethodModule`, part of the MacroCommons cake and implemented ONLY by
    // Hearth's own platform `UntypedMethod` object (UntypedMethodsScala2/Scala3) - interface and implementation are
    // always evicted together, so no user has a standalone implementation to break.
    exclude[ReversedMissingMethodProblem]("hearth.untyped.UntypedMethods#UntypedMethodModule.unsortedMethods"),
    // Perf: `cacheBucketKey` (cheap Type.Cache bucket discriminator - dealiased type symbol) added to the NESTED
    // trait `UntypedTypes#UntypedTypeModule`, part of the MacroCommons cake and implemented ONLY by Hearth's own
    // platform `UntypedType` objects (UntypedTypesScala2/Scala3) - interface and implementation are always evicted
    // together, so no user has a standalone implementation to break. `private[hearth]` besides.
    exclude[ReversedMissingMethodProblem]("hearth.untyped.UntypedTypes#UntypedTypeModule.cacheBucketKey"),
    // Type.Lazy support: `withMacroEntryContext`/`macroEntryContextKey` added to the NESTED trait
    // `Environments#CrossQuotesModule`, implemented ONLY by Hearth's own platform `CrossQuotes` objects
    // (EnvironmentsScala2/Scala3) - interface and implementations are always evicted together, so no user has a
    // standalone implementation to break. `private[hearth]` besides.
    exclude[ReversedMissingMethodProblem]("hearth.Environments#CrossQuotesModule.withMacroEntryContext"),
    exclude[ReversedMissingMethodProblem]("hearth.Environments#CrossQuotesModule.macroEntryContextKey"),
    // Type.Lazy: the accessor of the NEW nested `object Lazy` inside `Types#TypeModule` (a Scala-2 trait-nested-module
    // artifact). TypeModule is part of the MacroCommons cake and implemented ONLY by Hearth's own platform `Type`
    // objects - interface and implementations are always evicted together, so no user has a standalone implementation
    // to break.
    exclude[ReversedMissingMethodProblem]("hearth.typed.Types#TypeModule.Lazy")
  )
)

val noPublishSettings =
  Seq(projectType := ProjectType.NonPublished)

// Command generation

// We keep a custom alias object since hearth has non-standard CI logic (hearthCrossQuotes is JVM-only, etc.)
lazy val al = new Aliases(
  published = Seq(hearthBetterPrinters, hearthCrossQuotes, hearthMicroFp, hearth, hearthMunit),
  testOnly = Seq(hearthTests, hearthSandwichTests)
) {
  private def isTierRow(id: String): Boolean =
    id.contains("NextLts") || id.endsWith("Next")

  override protected def projectIds(
      matrices: Seq[sbt.internal.ProjectMatrix],
      platform: String,
      scalaBinary: String
  ): Vector[String] =
    super.projectIds(matrices, platform, scalaBinary).filterNot(isTierRow)

  override def ci(platform: String, scalaBinary: String): String = {
    val base = super.ci(platform, scalaBinary)
    val mimaIds = projectIds(published, platform, scalaBinary)
      .filterNot(_.startsWith("hearthCrossQuotes"))
    val mimaReport = mimaIds.map(id => s"$id/mimaReportBinaryIssues")
    if (mimaReport.nonEmpty) s"$base ; ${mimaReport.mkString(" ; ")}"
    else base
  }
}

// Modules

lazy val root = (project in file("."))
  .enablePlugins(KubuszokRootPlugin)
  .settings(noPublishSettings)
  .settings(mimaSettings)
  .aggregate(hearthBetterPrinters.projectRefs *)
  .aggregate(hearthCrossQuotes.projectRefs *)
  .aggregate(hearthMicroFp.projectRefs *)
  .aggregate(hearthMunit.projectRefs *)
  .aggregate(hearth.projectRefs *)
  .aggregate(hearthTests.projectRefs *)
  .aggregate(hearthSandwichTests.projectRefs *)
  .settings(
    moduleName := "hearth-build",
    name := "hearth-build",
    description := "Build setup for Hearth modules",
    logo :=
      s"""Hearth ${(version).value} build for (${versions.scala213}, ${versions.scala3}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         | - Regression tiers: Next-LTS (${versions.scala213NextLts} / ${versions.scala3NextLts}), Next (${versions.scala3Next})
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
         | - Scala 3 is the default (no suffix in project names)
         | - Scala 2.13 adds the "2" suffix
         | - Scala.js adds the "JS" suffix, Scala Native adds "Native"
         |
         |Test source directory tiers (in hearth-tests):
         | - scala/, scala-2/, scala-3/                             — all versions (must compile everywhere)
         | - scala-lts/, scala-lts-2/, scala-lts-3/                 — LTS + Next-LTS only (2.13.16+, and 3.3+, not 3.10+ - test code that cross compile on 2.13, and 3.3-3.9)
         | - scala-next-lts/, scala-next-lts-2/, scala-next-lts-3/  — Next-LTS only (2.13.18+, and 3.9 - test code that would work only at later versios that lowest supported, but wasn't deprecated by 3.10)
         | - scala-next/, scala-next-3/                             — Next only (3.10+ - test code that works at 3.10+, and contains no features deprecated by 3.10 or later)
         |The goal is to publish for the lowest supported Scala versions, but make sure that the code works on all the following Scala versions, even if e.g. some of syntax would become deprecated by later versions.
         |
         |When working with IntelliJ or Scala Metals, edit dev.properties to control which Scala version you're currently working with.
         |
         |If you need to test library locally in a different project, use publish-local-for-tests or manually publishLocal:
         | - hearth-better-printers (obligatory)
         | - hearth-cross-quotes (obligatory)
         | - hearth-micro-fp (obligatory)
         | - hearth (obligatory)
         | - hearth-munit (optional)
         |for the right Scala version and platform (see projects task).
         |""".stripMargin,
    usefulTasks := al.usefulTasks(
      extra = Seq(
        UsefulTask(
          (al.publishLocal("JVM", "2.13") ++ al.publishLocal("JVM", "3") :+ "show hearth/version").mkString(" ; "),
          "Publishes all Scala 2.13 and Scala 3 JVM artifacts to test snippets in documentation"
        ).alias("publish-local-for-tests"),
        UsefulTask(
          "hearthTests/test ; hearthTests2/test ; hearthSandwichTests/test ; hearthSandwichTests2/test",
          "Quickly run LTS JVM tests"
        ).alias("quick-test"),
        UsefulTask(
          "hearthTests/clean ; hearthTests2/clean ; hearthSandwichTests/clean ; hearthSandwichTests2/clean",
          "Quickly clean LTS JVM tests (useful to force-recompile macros)"
        ).alias("quick-clean"),
        UsefulTask(
          "hearthTestsNextLts/test ; hearthTests2NextLts/test ; hearthSandwichTestsNextLts/test",
          "Quickly run Next-LTS (regression) JVM tests"
        ).alias("quick-test-next-lts"),
        UsefulTask(
          "hearthTestsNextLts/clean ; hearthTests2NextLts/clean ; hearthSandwichTestsNextLts/clean",
          "Quickly clean Next-LTS JVM tests"
        ).alias("quick-clean-next-lts"),
        UsefulTask(
          "hearthTestsNext/test",
          "Quickly run Next (forward) JVM tests"
        ).alias("quick-test-next"),
        UsefulTask(
          "hearthTestsNext/clean",
          "Quickly clean Next JVM tests"
        ).alias("quick-clean-next")
      )
    )
  )

lazy val hearthBetterPrinters = projectMatrix
  .in(file("hearth-better-printers"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), versions.platforms)(only1VersionInIDE *)
  .pipe(Scala2Rows.allPlatforms(_, scala2Axis, scala2IdeSkip))
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-better-printers",
    name := "hearth-better-printers",
    description := "Better alternatiteves to Scala 2's showCode and showRaw, and Scala 3's Printer.TreeStructure",
    libraryDependencies ++= foldVersion(scalaVersion.value)(
      for3 = Seq(),
      for2_13 = Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value)
    )
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(mimaSettings *)

lazy val hearthCrossQuotes = projectMatrix
  .in(file("hearth-cross-quotes"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), versions.platforms)((defineCrossQuotes ++ only1VersionInIDE) *)
  .pipe(Scala2Rows.allPlatforms(_, scala2Axis, scala2IdeSkip))
  .enablePlugins(SourceGenPlugin)
  .disablePlugins(WelcomePlugin, MimaPlugin)
  .settings(
    moduleName := "hearth-cross-quotes",
    name := "hearth-cross-quotes",
    description := "Utilities for hurting little kittens",
    libraryDependencies ++= foldVersion(scalaVersion.value)(
      for3 = Seq("org.scala-lang" %% "scala3-compiler" % scalaVersion.value),
      for2_13 = Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value)
    ),
    SourceGenPlugin.autoImport.generateHearthSources := {
      val outDir = (Compile / sourceManaged).value
      val isScala3 = scalaVersion.value.startsWith("3.")
      if (isScala3) Seq.empty[File]
      else {
        val content = CrossQuotesMacrosGen.generate()
        val file = outDir / "hearth" / "cq" / "CrossQuotesMacrosCtorMethods.scala"
        ArityGen.writeIfChanged(file, content)
        Seq(file)
      }
    }
  )
  .settings(settings *)
  .settings(publishSettings *)
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  .dependsOn(hearthBetterPrinters)

lazy val hearthMicroFp = projectMatrix
  .in(file("hearth-micro-fp"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), versions.platforms)(only1VersionInIDE *)
  .pipe(Scala2Rows.allPlatforms(_, scala2Axis, scala2IdeSkip))
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-micro-fp",
    name := "hearth-micro-fp",
    description := "Micro FP library, for using a few useful extension without fetching a whole FP library",
    scalacOptions ++= foldVersion(scalaVersion.value)(
      for3 = Seq.empty,
      for2_13 = Seq("-opt:l:inline") // we have a few @inline for micro-optimisations in micro-fp
    )
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)

lazy val hearth = projectMatrix
  .in(file("hearth"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), versions.platforms)(((only1VersionInIDE ++ useCrossQuotes)) *)
  .pipe(
    Scala2Rows.allPlatforms(
      _,
      scala2Axis,
      scala2IdeSkip,
      configJvm = scala2CqJvm,
      configJs = scala2CqJs,
      configNat = scala2CqNative
    )
  )
  .enablePlugins(SourceGenPlugin)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth",
    name := "hearth",
    description := "Utilities for writing cross-platform macro logic",
    SourceGenPlugin.autoImport.generateHearthSources := {
      val outDir = (Compile / sourceManaged).value
      val isScala3 = scalaVersion.value.startsWith("3.")
      val content = if (isScala3) TypeConstructorsGen.scala3() else TypeConstructorsGen.scala2()
      val file = outDir / "hearth" / "typed" / "TypeConstructors.scala"
      ArityGen.writeIfChanged(file, content)
      Seq(file)
    }
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .settings(macroExtensionTraits := Seq("hearth.std.StandardMacroExtension"))
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  .dependsOn(hearthMicroFp)
  .dependsOn(hearthBetterPrinters)

lazy val hearthMunit = projectMatrix
  .in(file("hearth-munit"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), versions.platforms)(only1VersionInIDE *)
  .pipe(Scala2Rows.allPlatforms(_, scala2Axis, scala2IdeSkip))
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-munit",
    name := "hearth-munit",
    description := "MUnit testing utilities for Hearth-based tests",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % versions.munit,
      "org.scalacheck" %%% "scalacheck" % versions.scalacheck
    ),
    // Allow eviction of test-interface for Scala Native (munit requires 0.5.11, scalacheck requires 0.5.8)
    // Since test-interface 0.5.11 is backwards compatible with 0.5.8, we can safely use the newer version
    evictionErrorLevel := Level.Warn,
    // Allow munit's Scala 2.13.18 dependency when we compile with 2.13.16 (backwards compatible per SIP-51)
    allowUnsafeScalaLibUpgrade := true
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(mimaSettings *)
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  .dependsOn(hearth)

// Test normal use cases

lazy val hearthTests = projectMatrix
  .in(file("hearth-tests"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), versions.platforms)((only1VersionInIDE ++ useCrossQuotes) *)
  .pipe(
    Scala2Rows.allPlatforms(
      _,
      scala2Axis,
      scala2IdeSkip,
      configJvm = scala2CqJvm,
      configJs = scala2CqJs,
      configNat = scala2CqNative
    )
  )
  .enablePlugins(SourceGenPlugin)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-tests",
    name := "hearth-tests",
    description := "Tests for hearth utilities",
    SourceGenPlugin.autoImport.generateHearthSources := {
      val outDir = (Compile / sourceManaged).value
      val isScala3 = scalaVersion.value.startsWith("3.")
      val implGen = outDir / "hearth" / "crossquotes" / "CrossCtorInjectionFixturesImplGen.scala"
      ArityGen.writeIfChanged(implGen, CrossCtorTestGen.generate())
      val bridge = outDir / "hearth" / "crossquotes" / "CrossCtorInjectionFixtures.scala"
      val bridgeContent =
        if (isScala3) CrossCtorTestGen.generateScala3Bridge()
        else CrossCtorTestGen.generateScala2Bridge()
      ArityGen.writeIfChanged(bridge, bridgeContent)
      Seq(implGen, bridge)
    },
    Test / sourceGenerators += Def.task {
      val outDir = (Test / sourceManaged).value
      val spec = outDir / "hearth" / "crossquotes" / "CrossCtorInjectionSpec.scala"
      ArityGen.writeIfChanged(spec, CrossCtorTestGen.generateSpec())
      Seq(spec)
    }.taskValue,
    libraryDependencies ++= foldVersion(scalaVersion.value)(
      for3 = Seq(),
      for2_13 = Seq("org.scala-lang.modules" %% "scala-xml" % versions.scalaXml)
    ),
    coverageExcludedFiles := ".*Fixtures;.*FixturesImpl",
    evictionErrorLevel := Level.Warn,
    scalacOptions ++= Seq(
      "-Xmacro-settings:hearth.betterPrintersShouldFailOnUnsupportedTree=true",
      "-Xmacro-settings:hearth-tests.primitives.int=1024",
      "-Xmacro-settings:hearth-tests.primitives.long=65536L",
      "-Xmacro-settings:hearth-tests.primitives.float=3.14f",
      "-Xmacro-settings:hearth-tests.primitives.double=2.71828",
      "-Xmacro-settings:hearth-tests.primitives.boolean=true",
      "-Xmacro-settings:hearth-tests.primitives.explicit-string=\"hello\"",
      "-Xmacro-settings:hearth-tests.primitives.implicit-string=hello",
      "-Xmacro-settings:hearth.mioBenchmarkScopes=true",
      s"-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=${crossTarget.value / "flame-graphs"}"
    )
  )
  .settings(settings *)
  .settings(ltsSourceDirs *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .settings(
    macroExtensionTraits := Seq(
      "hearth.SuccessfulMacroExtension",
      "hearth.PartiallyFailedMacroExtension",
      "hearth.TotallyFailedMacroExtension"
    )
  )
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  .dependsOn(hearth)
  .dependsOn(hearthMunit)
  // Next-LTS tier: 3.9 regression (LTS axis trick — scalaABIVersion matches hearth's LTS row)
  .customRow(true, None, Seq(versions.scala3), Seq(tierNextLts, VirtualAxis.jvm)) { p =>
    p.settings(scalaVersion := versions.scala3NextLts)
      .settings(noPublishSettings *)
      .settings(nextLtsSourceDirs *)
      .settings(
        ideSkipProject := true,
        bspEnabled := false,
        scalafmtOnCompile := false,
        scalacOptions ++= {
          val jar = (hearthCrossQuotes.jvm(versions.scala3) / Compile / packageBin).value
          Seq(
            s"-Xplugin:${jar.getAbsolutePath}",
            s"-Jdummy=${jar.lastModified}",
            s"-P:hearth.cross-quotes:logging=$logCrossQuotes"
          )
        }
      )
      .disablePlugins(MimaPlugin, WelcomePlugin)
  }
  // Next-LTS tier: 2.13.18 regression (uses scala2Axis for "2" suffix)
  .customRow(
    true,
    Seq(scala2Axis, tierNextLts, VirtualAxis.jvm),
    (p: Project) =>
      p.settings(scalaVersion := versions.scala213NextLts)
        .settings(noPublishSettings *)
        .settings(nextLtsSourceDirs *)
        .settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false)
        .disablePlugins(MimaPlugin, WelcomePlugin)
  )
  // Next tier: 3.10 forward (no 2.13 — 3.10 drops cross-compilation)
  .customRow(true, None, Seq(versions.scala3), Seq(tierNext, VirtualAxis.jvm)) { p =>
    p.settings(scalaVersion := versions.scala3Next)
      .settings(noPublishSettings *)
      .settings(nextSourceDirs *)
      .settings(
        ideSkipProject := true,
        bspEnabled := false,
        scalafmtOnCompile := false,
        scalacOptions ++= {
          val jar = (hearthCrossQuotes.jvm(versions.scala3) / Compile / packageBin).value
          Seq(
            s"-Xplugin:${jar.getAbsolutePath}",
            s"-Jdummy=${jar.lastModified}",
            s"-P:hearth.cross-quotes:logging=$logCrossQuotes"
          )
        }
      )
      .disablePlugins(MimaPlugin, WelcomePlugin)
  }

// Test cross compilation: 2.13x3

lazy val hearthSandwichExamples213 = projectMatrix
  .in(file("hearth-sandwich-examples-213"))
  .defaultAxes(scala3DefaultAxes: _*)
  .pipe(Scala2Rows.jvmOnly(_, scala2Axis, scala2IdeSkip))
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "hearth-sandwich-examples-213",
    name := "hearth-sandwich-examples-213",
    description := "Tests cases compiled with Scala 2.13 to test macros in 2.13x3 cross-compilation (non-publishable)"
  )

lazy val hearthSandwichExamples3 = projectMatrix
  .in(file("hearth-sandwich-examples-3"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), List(VirtualAxis.jvm))()
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "hearth-sandwich-examples-3",
    name := "hearth-sandwich-examples-3",
    description := "Tests cases compiled with Scala 3 to test macros in 2.13x3 cross-compilation (non-publishable)"
  )
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  // Next-LTS tier: 3.9 sandwich examples (LTS axis trick)
  .customRow(true, None, Seq(versions.scala3), Seq(tierNextLts, VirtualAxis.jvm)) { p =>
    p.settings(scalaVersion := versions.scala3NextLts)
      .settings(noPublishSettings *)
      .settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false)
      .disablePlugins(MimaPlugin, WelcomePlugin)
  }

lazy val hearthSandwichTests = projectMatrix
  .in(file("hearth-sandwich-tests"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
  .pipe(Scala2Rows.jvmOnly(_, scala2Axis, scala2IdeSkip))
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "hearth-sandwich-tests",
    name := "hearth-sandwich-tests",
    description := "Tests macros in 2.13x3 cross-compilation (non-publishable)"
  )
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  .dependsOn(hearth % s"$Test->$Test;$Compile->$Compile")
  .dependsOn(hearthSandwichExamples213 % s"$Test->$Test;$Compile->$Compile")
  .dependsOn(hearthSandwichExamples3 % s"$Test->$Test;$Compile->$Compile")
  .dependsOn(hearthTests % s"$Test->$Test;$Compile->$Compile")
  // Next-LTS tier: 3.9 sandwich (Scala 3 only — 2.13 can't consume 3.9)
  .customRow(true, None, Seq(versions.scala3), Seq(tierNextLts, VirtualAxis.jvm)) { p =>
    p.settings(scalaVersion := versions.scala3NextLts)
      .settings(noPublishSettings *)
      .settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false)
      .disablePlugins(MimaPlugin, WelcomePlugin)
  }

// Modules for debugging cross-quotes, better-printers, etc while minimizing the number of code to recompile

lazy val debugHearthBetterPrinters = projectMatrix
  .in(file("debug-hearth-better-printers"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
  .pipe(Scala2Rows.jvmOnly(_, scala2Axis, scala2IdeSkip))
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "debug-hearth-better-printers",
    name := "debug-hearth-better-printers",
    description := "Debugging module for hearth-better-printers, intended to recompile as little as possible when working on better-printers issue, should not be published not contain any commited code"
  )
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  .dependsOn(hearthBetterPrinters)

lazy val debugHearth = projectMatrix
  .in(file("debug-hearth"))
  .defaultAxes(scala3DefaultAxes: _*)
  .someVariations(List(versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
  .pipe(Scala2Rows.jvmOnly(_, scala2Axis, scala2IdeSkip))
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
  .settings(mimaSettings *)
  .settings(
    moduleName := "debug-hearth",
    name := "debug-hearth",
    description := "Debugging module for hearth, intended to recompile as little as possible when working on hearth issue, should not be published not contain any commited code"
  )
  .jvmPlatform(Seq(versions.scala3), jvmOnlySettings)
  .dependsOn(hearth)

// when having memory/GC-related errors during build, uncommenting this may be useful:
Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Compile, 2) // only 2 compilations at once
)
