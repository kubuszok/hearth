// kubuszok plugin (bundles: sbt-git, sbt-scalafmt, sbt-scoverage, sbt-projectmatrix, sbt-commandmatrix, sbt-pgp, sbt-mima-plugin, sbt-ide-settings, sbt-welcome, sbt-scalajs, sbt-scala-native)
// TODO: remove after sbt-kubuszok 0.2.5
resolvers += "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"
addSbtPlugin("com.kubuszok" % "sbt-kubuszok" % "0.2.4-4-g52226ed-SNAPSHOT")
// benchmarks
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
