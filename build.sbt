import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import com.typesafe.tools.mima.core.{Problem, ProblemFilters}
import sbtwelcome.UsefulTask
import commandmatrix.extra.*
import org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile

// Used to configure the build so that it would format+compile during development but not on CI.
lazy val isCI = sys.env.get("CI").contains("true")
ThisBuild / scalafmtOnCompile := !isCI

// Used to compile tests against the newest Scala versions, to check for regressions.
lazy val isNewestScalaTests = sys.env.get("NEWEST_SCALA_TESTS").contains("true")

// Used to publish snapshots to Maven Central.
val mavenCentralSnapshots = "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"

// Versions:

val versions = new {
  // Versions we are publishing for.
  val scala213 = "2.13.16"
  val scala3 = "3.3.7"

  // Versions we can compile tests against if needed, to check for regressions.
  val scala213Newest = "2.13.18"
  val scala3Newest = "3.8.1"

  // Which versions should be cross-compiled for publishing.
  val scalas = List(scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  // Dependencies.
  val kindProjector = "0.13.4"
  val munit = "1.2.1"
  val scalacheck = "1.19.0"
  val scalaXml = "2.4.0"

  // Explicitly handle Scala 2.13 and Scala 3 separately.
  def fold[A](scalaVersion: String)(for2_13: => Seq[A], for3: => Seq[A]): Seq[A] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => for2_13
      case Some((3, _))  => for3
      case _             => Seq.empty // for sbt, apparently
    }
}

// Development settings:

val dev = new {

  val props = scala.util
    .Using(new java.io.FileInputStream("dev.properties")) { fis =>
      val props = new java.util.Properties()
      props.load(fis)
      props
    }
    .get

  // Which version should be used in IntelliJ
  val ideScala = props.getProperty("ide.scala") match {
    case "2.13" => versions.scala213
    case "3"    => versions.scala3
  }
  val idePlatform = props.getProperty("ide.platform") match {
    case "jvm"    => VirtualAxis.jvm
    case "js"     => VirtualAxis.js
    case "native" => VirtualAxis.native
  }

  val logCrossQuotes = props.getProperty("log.cross-quotes") match {
    case "true"                          => true
    case "false"                         => false
    case otherwise if otherwise.nonEmpty => otherwise
    case _                               => !isCI
  }

  def isIdeScala(scalaVersion: String): Boolean =
    CrossVersion.partialVersion(scalaVersion) == CrossVersion.partialVersion(ideScala)
  def isIdePlatform(platform: VirtualAxis): Boolean = platform == idePlatform
}

// Common settings:

Global / excludeLintKeys += git.useGitDescribe
Global / excludeLintKeys += ideSkipProject
val only1VersionInIDE =
  // For the platform we are working with, show only the project for the Scala version we are working with.
  MatrixAction
    .ForPlatform(dev.idePlatform)
    .Configure(
      _.settings(
        ideSkipProject := !dev.isIdeScala(scalaVersion.value),
        bspEnabled := dev.isIdeScala(scalaVersion.value),
        scalafmtOnCompile := !isCI
      )
    ) +:
    // Do not show in IDE and BSP projects for the platform we are not working with.
    versions.platforms.filterNot(dev.isIdePlatform).map { platform =>
      MatrixAction
        .ForPlatform(platform)
        .Configure(_.settings(ideSkipProject := true, bspEnabled := false, scalafmtOnCompile := false))
    }

// The hearth-cross-quotes:
//  - on Scala 2 are macros (defined for all platforms)
//  - and on Scala 3 are plugins (defined only for JVM).
val defineCrossQuotes = versions.scalas.flatMap(
  versions.fold(_)(
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
  versions.fold(scalaVersion)(
    for2_13 = List(
      // Enable logging from cross-quotes.
      MatrixAction
        .ForScala(_.isScala2)
        .Configure(_.settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=${dev.logCrossQuotes}")),
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
                s"-P:hearth.cross-quotes:logging=${dev.logCrossQuotes}"
              )
            }
          )
        )
    )
  )
}

val settings = Seq(
  git.useGitDescribe := true,
  git.uncommittedSignifier := None,
  scalacOptions ++= versions.fold(scalaVersion.value)(
    for3 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", if (scalaVersion.value == versions.scala3Newest) "17" else "11", // Scala 3.8+ requires JDK 17+
      "-rewrite",
      "-source", "3.3-migration",
      // format: on
      "-unchecked",
      "-deprecation",
      "-explain",
      "-explain-cyclic",
      "-explain-types",
      "-feature",
      "-no-indent",
      "-Wconf:msg=Unreachable case:s", // suppress fake (?) errors in internal.compiletime
      "-Wconf:msg=Missing symbol position:s", // suppress warning https://github.com/scala/scala3/issues/21672
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
  ),
  Compile / doc / scalacOptions ++= versions.fold(scalaVersion.value)(
    for3 = Seq("-Ygenerate-inkuire"), // type-based search for Scala 3, this option cannot go into compile
    for2_13 = Seq.empty
  ),
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
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
        versions.fold(scalaVersion.value)(
          for3 = Seq(srcDir.resolve("main/scala-newest-3").toFile),
          for2_13 = Seq(srcDir.resolve("main/scala-newest-2").toFile)
        )
    } else Seq.empty
  },
  Test / unmanagedSourceDirectories ++= {
    if (isNewestScalaTests) {
      val srcDir = sourceDirectory.value.toPath
      Seq(srcDir.resolve("test/scala-newest").toFile) ++
        versions.fold(scalaVersion.value)(
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
  libraryDependencies ++= versions.fold(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
      compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
    )
  )
)

val versionSchemeSettings = Seq(versionScheme := Some("early-semver"))

val publishSettings = Seq(
  organization := "com.kubuszok",
  homepage := Some(url("https://scala-hearth.readthedocs.io")),
  organizationHomepage := Some(url("https://kubuszok.com")),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/MateuszKubuszok/hearth/"),
      "scm:git:git@github.com:MateuszKubuszok/hearth.git"
    )
  ),
  startYear := Some(2025),
  developers := List(
    Developer("MateuszKubuszok", "Mateusz Kubuszok", "", url("https://github.com/MateuszKubuszok"))
  ),
  pomExtra := (
    <issueManagement>
      <system>GitHub issues</system>
      <url>https://github.com/MateuszKubuszok/hearth/issues</url>
    </issueManagement>
  ),
  publishTo := {
    if (isSnapshot.value) Some(mavenCentralSnapshots)
    else localStaging.value
  },
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ =>
    false
  },
  // Sonatype ignores isSnapshot setting and only looks at -SNAPSHOT suffix in version:
  //   https://central.sonatype.org/publish/publish-maven/#performing-a-snapshot-deployment
  // meanwhile sbt-git used to set up SNAPSHOT if there were uncommitted changes:
  //   https://github.com/sbt/sbt-git/issues/164
  // (now this suffix is empty by default) so we need to fix it manually.
  git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty,
  git.uncommittedSignifier := Some("SNAPSHOT")
)

val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    val previousVersions = moduleName.value match {
      case "hearth-better-printers" | "hearth-cross-quotes" | "hearth-micro-fp" | "hearth" =>
        Set() // fix after 0.2.0 release
      case "hearth-tests" | "hearth-sandwich-examples-213" | "hearth-sandwich-examples-3" | "hearth-sandwich-tests" |
          "debug-hearth-better-printers" | "debug-hearth" =>
        Set()
      case name => sys.error(s"All modules should be explicitly checked or ignored for MiMa, missing: $name")
    }
    previousVersions.map(organization.value %% moduleName.value % _)
  },
  mimaFailOnNoPrevious := {
    moduleName.value match {
      case "hearth-better-printers" | "hearth-cross-quotes" | "hearth-micro-fp" | "hearth" =>
        false // fix after 0.2.0 release
      case "hearth-tests" | "hearth-sandwich-examples-213" | "hearth-sandwich-examples-3" | "hearth-sandwich-tests" |
          "debug-hearth-better-printers" | "debug-hearth" =>
        false
      case name => sys.error(s"All modules should be explicitly checked or ignored for MiMa, missing: $name")
    }
  }
)

val noPublishSettings =
  Seq(publish / skip := true, publishArtifact := false)

// Command generation

val al = new {

  private val prodProjects = Vector("hearthBetterPrinters", "hearthCrossQuotes", "hearthMicroFp", "hearth")
  private val testProjects = Vector("hearthTests", "hearthSandwichTests")

  private def isJVM(platform: String): Boolean = platform == "JVM"

  private def projects(platform: String, scalaSuffix: String): Vector[String] =
    for {
      name <- prodProjects ++ testProjects
      if ((name != "hearthCrossQuotes" && name != "hearthSandwichTests") || isJVM(platform))
    } yield s"$name${if (isJVM(platform)) "" else platform}$scalaSuffix"

  def ci(platform: String, scalaSuffix: String): String = {
    def tasksOf(name: String): Vector[String] = projects(platform, scalaSuffix).flatMap {
      case project if project.startsWith("hearthCrossQuotes") && Set("mimaReportBinaryIssues").contains(name) =>
        Vector()
      case project => Vector(s"$project/$name")
    }

    val clean = Vector("clean")
    val compileAndTest = tasksOf("compile") ++ tasksOf("test")
    val coverageCompileAndTest =
      if (isJVM(platform)) "coverage" +: compileAndTest :+ "coverageAggregate" :+ "coverageOff" else compileAndTest
    val mimaReport = tasksOf("mimaReportBinaryIssues")

    val tasks = clean ++ coverageCompileAndTest ++ mimaReport
    tasks.mkString(" ; ")
  }

  def test(platform: String, scalaSuffix: String): String =
    projects(platform, scalaSuffix).map(project => s"$project/test").mkString(" ; ")

  def release(tag: Seq[String]): String =
    if (tag.nonEmpty) "publishSigned ; sonaRelease" else "publishSigned"

  def publishLocal(platform: String, scalaSuffix: String): Vector[String] =
    for {
      name <- prodProjects
    } yield s"$name${if (isJVM(platform)) "" else platform}$scalaSuffix/publishLocal"

  val publishLocalForTests =
    (publishLocal("JVM", "") ++ publishLocal("JVM", "3") ++ List("show hearth/version")).mkString(" ; ")
}

// Modules

lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .settings(settings)
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(hearthBetterPrinters.projectRefs *)
  .aggregate(hearthCrossQuotes.projectRefs *)
  .aggregate(hearthMicroFp.projectRefs *)
  .aggregate(hearth.projectRefs *)
  .aggregate(hearthTests.projectRefs *)
  .aggregate(hearthSandwichTests.projectRefs *)
  .settings(
    moduleName := "hearth-build",
    name := "hearth-build",
    description := "Build setup for Hearth modules",
    logo :=
      s"""Hearth ${(version).value} build for (${versions.scala213}, ${versions.scala3}) x (Scala JVM, Scala.js $scalaJSVersion, Scala Native $nativeVersion)
         |
         |This build uses sbt-projectmatrix with sbt-commandmatrix helper:
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
         |for the right Scala version and platform (see projects task).
         |""".stripMargin,
    usefulTasks := Seq(
      UsefulTask("projects", "List all projects generated by the build matrix").noAlias,
      UsefulTask(
        "test",
        "Compile and test all projects in all Scala versions and platforms (beware! it uses a lot of memory and might OOM!)"
      ).noAlias,
      UsefulTask(al.release(git.gitCurrentTags.value), "Publish everything to release or snapshot repository")
        .alias("ci-release"),
      UsefulTask(al.ci("JVM", "3"), "CI pipeline for Scala 3+JVM").alias("ci-jvm-3"),
      UsefulTask(al.ci("JVM", ""), "CI pipeline for Scala 2.13+JVM").alias("ci-jvm-2_13"),
      UsefulTask(al.ci("JS", "3"), "CI pipeline for Scala 3+Scala JS").alias("ci-js-3"),
      UsefulTask(al.ci("JS", ""), "CI pipeline for Scala 2.13+Scala JS").alias("ci-js-2_13"),
      UsefulTask(al.ci("Native", "3"), "CI pipeline for Scala 3+Scala Native").alias("ci-native-3"),
      UsefulTask(al.ci("Native", ""), "CI pipeline for Scala 2.13+Scala Native").alias("ci-native-2_13"),
      UsefulTask(al.test("JVM", "3"), "Test all projects in Scala 3+JVM").alias("test-jvm-3"),
      UsefulTask(al.test("JVM", ""), "Test all projects in Scala 2.13+JVM").alias("test-jvm-2_13"),
      UsefulTask(al.test("JS", "3"), "Test all projects in Scala 3+Scala JS").alias("test-js-3"),
      UsefulTask(al.test("JS", ""), "Test all projects in Scala 2.13+Scala JS").alias("test-js-2_13"),
      UsefulTask(al.test("Native", "3"), "Test all projects in Scala 3+Scala Native").alias("test-native-3"),
      UsefulTask(al.test("Native", ""), "Test all projects in Scala 2.13+Scala Native").alias("test-native-2_13"),
      UsefulTask(
        "hearthTests/test ; hearthTests3/test ; hearthSandwichTests/test ; hearthSandwichTests3/test",
        "Quickly run JVM on all platforms"
      ).alias("quick-test"),
      UsefulTask(
        "hearthTests/clean ; hearthTests3/clean ; hearthSandwichTests/clean ; hearthSandwichTests3/clean",
        "Quickly clean JVM tests on all platforms (useful to force-recompile macros)"
      ).alias("quick-clean"),
      UsefulTask(
        al.publishLocalForTests,
        "Publishes all Scala 2.13 and Scala 3 JVM artifacts to test snippets in documentation"
      )
        .alias("publish-local-for-tests")
    )
  )

lazy val hearthBetterPrinters = projectMatrix
  .in(file("hearth-better-printers"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-better-printers",
    name := "hearth-better-printers",
    description := "Better alternatiteves to Scala 2's showCode and showRaw, and Scala 3's Printer.TreeStructure",
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for3 = Seq(),
      for2_13 = Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value)
    )
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(mimaSettings *)

lazy val hearthCrossQuotes = projectMatrix
  .in(file("hearth-cross-quotes"))
  .someVariations(versions.scalas, versions.platforms)((defineCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin, MimaPlugin)
  .settings(
    moduleName := "hearth-cross-quotes",
    name := "hearth-cross-quotes",
    description := "Utilities for hurting little kittens",
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for3 = Seq("org.scala-lang" %% "scala3-compiler" % scalaVersion.value),
      for2_13 = Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value)
    )
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .dependsOn(hearthBetterPrinters)

lazy val hearthMicroFp = projectMatrix
  .in(file("hearth-micro-fp"))
  .someVariations(versions.scalas, versions.platforms)(only1VersionInIDE *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-micro-fp",
    name := "hearth-micro-fp",
    description := "Micro FP library, for using a few useful extension without fetching a whole FP library",
    scalacOptions ++= versions.fold(scalaVersion.value)(
      for3 = Seq.empty,
      for2_13 = Seq("-opt:l:inline") // we have a few @inline for micro-optimisations in micro-fp
    )
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)

lazy val hearth = projectMatrix
  .in(file("hearth"))
  .someVariations(versions.scalas, versions.platforms)(((only1VersionInIDE ++ useCrossQuotes)) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth",
    name := "hearth",
    description := "Utilities for writing cross-platform macro logic"
  )
  .settings(settings *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(dependencies *)
  .settings(mimaSettings *)
  .settings(macroExtensionTraits := Seq("hearth.std.StandardMacroExtension"))
  .dependsOn(hearthMicroFp)
  .dependsOn(hearthBetterPrinters)

// Test normal use cases

lazy val hearthTests = projectMatrix
  .in(file("hearth-tests"))
  .someVariations(versions.scalas, versions.platforms)((only1VersionInIDE ++ useCrossQuotes) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "hearth-tests",
    name := "hearth-tests",
    description := "Tests for hearth utilities",
    // Required for Scala 2.13 to test parsing of Scala XML.
    libraryDependencies ++= versions.fold(scalaVersion.value)(
      for3 = Seq(),
      for2_13 = Seq("org.scala-lang.modules" %% "scala-xml" % versions.scalaXml)
    ),
    // Do not cover Fixtures and FixturesImpl, they are used to test the library, not a part of it.
    coverageExcludedFiles := ".*Fixtures;.*FixturesImpl",
    scalacOptions ++= Seq(
      // To make sure that we are not silently failing on unspupported trees
      "-Xmacro-settings:hearth.betterPrintersShouldFailOnUnsupportedTree=true",
      // To test parsing of scalacOptions
      "-Xmacro-settings:hearth-tests.primitives.int=1024",
      "-Xmacro-settings:hearth-tests.primitives.long=65536L",
      "-Xmacro-settings:hearth-tests.primitives.float=3.14f",
      "-Xmacro-settings:hearth-tests.primitives.double=2.71828",
      "-Xmacro-settings:hearth-tests.primitives.boolean=true",
      "-Xmacro-settings:hearth-tests.primitives.explicit-string=\"hello\"",
      "-Xmacro-settings:hearth-tests.primitives.implicit-string=hello"
    )
  )
  .settings(settings *)
  .settings(scalaNewestSettings *)
  .settings(versionSchemeSettings *)
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
  .dependsOn(hearth)

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

lazy val hearthSandwichTests = projectMatrix
  .in(file("hearth-sandwich-tests"))
  .someVariations(List(versions.scala213, versions.scala3), List(VirtualAxis.jvm))(only1VersionInIDE *)
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
  .dependsOn(hearth)

// when having memory/GC-related errors during build, uncommenting this may be useful:
Global / concurrentRestrictions := Seq(
  Tags.limit(Tags.Compile, 2) // only 2 compilations at once
)
