// kubuszok plugin (bundles: sbt-git, sbt-scalafmt, sbt-scoverage, sbt-projectmatrix, sbt-commandmatrix, sbt-pgp, sbt-mima-plugin, sbt-ide-settings, sbt-welcome, sbt-scalajs, sbt-scala-native)
addSbtPlugin("com.kubuszok" % "sbt-kubuszok" % "0.2.3-9-ga68e33d-SNAPSHOT")
// benchmarks
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

// TEMP: sbt-kubuszok snapshot under test
resolvers += "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/"
