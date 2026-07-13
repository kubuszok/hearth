import sbtwelcome.UsefulTask
import commandmatrix.extra.*
import kubuszok.sbt._
import kubuszok.sbt.KubuszokPlugin.autoImport._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

// Used to compile tests against the newest Scala versions, to check for regressions.
lazy val isNewestScalaTests = sys.env.get("NEWEST_SCALA_TESTS").contains("true")

// Versions:

val versions = new {
  // Versions we are publishing for.
  val scala213 = "2.13.16"
  val scala3 = "3.3.8"

  // Versions we can compile tests against if needed, to check for regressions.
  val scala213Newest = "2.13.18"
  val scala3Newest = "3.8.4"

  // Which versions should be cross-compiled for publishing.
  val scalas = List(scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  val kindProjector = "0.13.4"
  val munit = "1.3.4"
  val scalacheck = "1.19.0"
  val scalaXml = "2.4.0"
}

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

// The hearth-cross-quotes:
//  - on Scala 2 are macros (defined for all platforms)
//  - and on Scala 3 are plugins (defined only for JVM).
val defineCrossQuotes = versions.scalas.flatMap(
  foldVersion(_)(
    // Scala 2: no skipping, we are defining projects for all platforms
    for2_13 = List.empty,
    // Scala 3: skip for JS and Native, we are defining projects only for JVM
    for3 = List(
      MatrixAction {
        case (version, List(VirtualAxis.js))     => version.isScala3
        case (version, List(VirtualAxis.native)) => version.isScala3
        case _                                   => false
      }.Skip
    )
  )
)

// The hearth-cross-quotes:
//  - on Scala 2 are macros (defined for all platforms)
//  - and on Scala 3 are plugins (defined only for JVM).
val useCrossQuotes = versions.scalas.flatMap { scalaVersion =>
  foldVersion(scalaVersion)(
    for2_13 = List(
      // Enable logging from cross-quotes.
      MatrixAction
        .ForScala(_.isScala2)
        .Configure(_.settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=$logCrossQuotes")),
      // Depends on cross-quotes specific for the platform.
      MatrixAction {
        case (version, List(VirtualAxis.jvm)) => version.isScala2
        case _                                => false
      }.Configure(_.dependsOn(hearthCrossQuotes.jvm(scalaVersion))),
      MatrixAction {
        case (version, List(VirtualAxis.js)) => version.isScala2
        case _                               => false
      }.Configure(_.dependsOn(hearthCrossQuotes.js(scalaVersion))),
      MatrixAction {
        case (version, List(VirtualAxis.native)) => version.isScala2
        case _                                   => false
      }.Configure(_.dependsOn(hearthCrossQuotes.native(scalaVersion)))
    ),
    for3 = List(
      MatrixAction
        .ForScala(_.isScala3)
        .Configure(
          _.settings(
            scalacOptions ++= {
              val jar = (hearthCrossQuotes.jvm(scalaVersion) / Compile / packageBin).value
              Seq(
                // Add the cross-quotes compiler plugin - the same for all platforms.
                s"-Xplugin:${jar.getAbsolutePath}",
                // Ensures recompilation.
                s"-Jdummy=${jar.lastModified}",
                // Enable logging from cross-quotes.
                s"-P:hearth.cross-quotes:logging=$logCrossQuotes"
              )
            }
          )
        )
    )
  )
}

val settings = Seq(
  scalacOptions ++= foldVersion(scalaVersion.value)(
    for3 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", if (scalaVersion.value == versions.scala3Newest) "17" else "11", // Scala 3.8+ requires JDK 17+
      // format: on
    ) ++ (if (scalaVersion.value != versions.scala3Newest) Seq("-rewrite", "-source", "3.3-migration")
          else Seq()) ++ Seq(
      "-unchecked",
      "-deprecation",
      "-explain",
      "-explain-cyclic",
      "-explain-types",
      "-feature",
      "-no-indent",
      "-Wconf:msg=Unreachable case:s", // suppress fake (?) errors in internal.compiletime
      "-Wconf:msg=Missing symbol position:s", // suppress warning https://github.com/scala/scala3/issues/21672
      "-Wconf:msg=should be provided with a .using. clause:s", // suppress migration warning after removing -source 3.3-migration
      "-Wconf:msg=with as a type operator has been deprecated:s", // suppress migration warning after removing -source 3.3-migration
      "-Wnonunit-statement",
      // "-Wunused:imports", // import x.Underlying as X is marked as unused even though it is! probably one of https://github.com/scala/scala3/issues/: #18564, #19252, #19657, #19912
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wunused:explicits",
      "-Wunused:implicits",
      "-Wunused:params",
      "-Wvalue-discard",
      "-Werror",
      "-Xcheck-macros"
    ) ++ (if (scalaVersion.value == versions.scala3Newest) Seq("-Xkind-projector:underscores")
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
      "-Wconf:cat=scala3-migration:s", // silence mainly issues with -Xsource:3 and private case class constructors
      "-Wconf:cat=deprecation&origin=hearth.*:s", // we want to be able to deprecate APIs and test them while they're deprecated
      "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s", // suppress fake(?) errors in internal.compiletime (adding origin breaks this suppression)
      "-Wconf:msg=discarding unmoored doc comment:s", // silence errors when scaladoc cannot comprehend nested vals
      "-Wconf:msg=Could not find any member to link for:s", // since errors when scaladoc cannot link to stdlib types or nested types
      "-Wconf:msg=Variable .+ undefined in comment for:s", // silence errors when there we're showing a buggy Expr in scaladoc comment
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
      (if (scalaVersion.value == versions.scala213Newest)
         Seq(
           "-Wconf:msg=a type was inferred to be kind-polymorphic `Nothing` to conform to:s", // silence warn that appeared after updating to Scala 2.13.17
           "-Xsource-features:eta-expand-always" // silence warn that appears since 2.13.17
         )
       else Seq.empty)
  )
)

val jvmOnlySettings = Seq(
  scalacOptions ++= { if (scalaVersion.value == versions.scala3) Seq("-Yfuture-lazy-vals") else Seq.empty }
)

val scalaNewestSettings = Seq(
  // Sets the Scala version to the newest supported version for the current platform.
  scalaVersion := {
    scalaVersion.value match {
      case versions.scala213 if isNewestScalaTests => versions.scala213Newest
      case versions.scala3 if isNewestScalaTests   => versions.scala3Newest
      case current                                 => current
    }
  },
  // Adds directories with sources that should only be tested with the newest Scala version.
  Compile / unmanagedSourceDirectories ++= {
    if (isNewestScalaTests) {
      val srcDir = sourceDirectory.value.toPath
      Seq(srcDir.resolve("main/scala-newest").toFile) ++
        foldVersion(scalaVersion.value)(
          for3 = Seq(srcDir.resolve("main/scala-newest-3").toFile),
          for2_13 = Seq(srcDir.resolve("main/scala-newest-2").toFile)
        )
    } else Seq.empty
  },
  Test / unmanagedSourceDirectories ++= {
    if (isNewestScalaTests) {
      val srcDir = sourceDirectory.value.toPath
      Seq(srcDir.resolve("test/scala-newest").toFile) ++
        foldVersion(scalaVersion.value)(
          for3 = Seq(srcDir.resolve("test/scala-newest-3").toFile),
          for2_13 = Seq(srcDir.resolve("test/scala-newest-2").toFile)
        )
    } else Seq.empty
  }
)

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
val mimaPreviousVersion = "0.4.1"

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
         |${if (isNewestScalaTests)
          s" - Testing against Scala ${versions.scala213Newest} and ${versions.scala3Newest} for forward compatibility and newest features support\n"
        else ""}
         |This build uses sbt-projectmatrix with sbt-commdmatrix helper:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         | - Scala 3 adds the suffix "3" to a project name seen in build.sbt
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
          "hearthTests/test ; hearthTests3/test ; hearthSandwichTests/test ; hearthSandwichTests3/test",
          "Quickly run JVM on all platforms"
        ).alias("quick-test"),
        UsefulTask(
          "hearthTests/clean ; hearthTests3/clean ; hearthSandwichTests/clean ; hearthSandwichTests3/clean",
          "Quickly clean JVM tests on all platforms (useful to force-recompile macros)"
        ).alias("quick-clean")
      )
    )
  )

lazy val hearthBetterPrinters = projectMatrix
  .in(file("hearth-better-printers"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
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
  .someVariations(versions.scalas, versions.platforms)((defineCrossQuotes ++ only1VersionInIDE) *)
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
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
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
  .someVariations(versions.scalas, versions.platforms)(((only1VersionInIDE ++ useCrossQuotes)) *)
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
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
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
  .someVariations(versions.scalas, versions.platforms)((only1VersionInIDE ++ useCrossQuotes) *)
  .enablePlugins(SourceGenPlugin)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-tests",
    name := "hearth-tests",
    description := "Tests for hearth utilities",
    SourceGenPlugin.autoImport.generateHearthSources := {
      val outDir = (Compile / sourceManaged).value
      val isScala3 = scalaVersion.value.startsWith("3.")
      // Shared impl gen trait
      val implGen = outDir / "hearth" / "crossquotes" / "CrossCtorInjectionFixturesImplGen.scala"
      ArityGen.writeIfChanged(implGen, CrossCtorTestGen.generate())
      // Version-specific bridge (one file, different content per Scala version)
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
    // Required for Scala 2.13 to test parsing of Scala XML.
    libraryDependencies ++= foldVersion(scalaVersion.value)(
      for3 = Seq(),
      for2_13 = Seq("org.scala-lang.modules" %% "scala-xml" % versions.scalaXml)
    ),
    // Do not cover Fixtures and FixturesImpl, they are used to test the library, not a part of it.
    coverageExcludedFiles := ".*Fixtures;.*FixturesImpl",
    // Allow eviction of test-interface for Scala Native - 0.5.11 is backwards compatible with 0.5.8
    evictionErrorLevel := Level.Warn,
    scalacOptions ++= Seq(
      // To make sure that we are not silently failing on unsupported trees
      "-Xmacro-settings:hearth.betterPrintersShouldFailOnUnsupportedTree=true",
      // To test parsing of scalacOptions
      "-Xmacro-settings:hearth-tests.primitives.int=1024",
      "-Xmacro-settings:hearth-tests.primitives.long=65536L",
      "-Xmacro-settings:hearth-tests.primitives.float=3.14f",
      "-Xmacro-settings:hearth-tests.primitives.double=2.71828",
      "-Xmacro-settings:hearth-tests.primitives.boolean=true",
      "-Xmacro-settings:hearth-tests.primitives.explicit-string=\"hello\"",
      "-Xmacro-settings:hearth-tests.primitives.implicit-string=hello",
      // Enable MIO scope benchmarking and flame graph generation
      "-Xmacro-settings:hearth.mioBenchmarkScopes=true",
      s"-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=${crossTarget.value / "flame-graphs"}"
    )
  )
  .settings(settings *)
  .settings(scalaNewestSettings *)
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

// Test cross compilation: 2.13x3

lazy val hearthSandwichExamples213 = projectMatrix
  .in(file("hearth-sandwich-examples-213"))
  .someVariations(List(versions.scala213), List(VirtualAxis.jvm))()
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

lazy val hearthSandwichTests = projectMatrix
  .in(file("hearth-sandwich-tests"))
  .someVariations(List(versions.scala213, versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
  .settings(settings *)
  .settings(scalaNewestSettings *)
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

// Modules for debugging cross-quotes, better-printers, etc while minimizing the number of code to recompile

lazy val debugHearthBetterPrinters = projectMatrix
  .in(file("debug-hearth-better-printers"))
  .someVariations(List(versions.scala213, versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
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
  .someVariations(List(versions.scala213, versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
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
