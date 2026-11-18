//> using scala 3.3.9
//> using jvm 17

// Checks that the FAQ's runtime/compile-time package tables cover every package of every published module,
// and that they do not list packages which no longer exist.
//
// Usage (from the repository root): scala-cli run scripts/check-runtime-packages.scala

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

val faqFile = Paths.get("docs/user-guide/faq.md")
val sectionHeading = "## Which parts of Hearth end up in my application at runtime?"

val rootPackage = "hearth"

// Test modules are not published, so they never end up in users' fat JARs.
def isPublished(moduleName: String): Boolean =
  !moduleName.contains("tests") && !moduleName.contains("examples")

val publishedModules: List[Path] = Files
  .list(Paths.get("."))
  .iterator()
  .asScala
  .filter(dir => dir.getFileName.toString.startsWith("hearth") && Files.isDirectory(dir.resolve("src/main")))
  .filter(dir => isPublished(dir.getFileName.toString))
  .toList
  .sortBy(_.getFileName.toString)

// Sources live in `src/main/scala`, `src/main/scala-2`, `src/main/scalajvm`, etc., all following the
// directory-per-package convention, so the path relative to such a root is the package name.
def packagesOf(module: Path): Set[String] = {
  val main = module.resolve("src/main")
  Files
    .list(main)
    .iterator()
    .asScala
    .filter(root => root.getFileName.toString.startsWith("scala"))
    .flatMap { sourceRoot =>
      Files
        .walk(sourceRoot)
        .iterator()
        .asScala
        .filter(_.toString.endsWith(".scala"))
        .map(file => sourceRoot.relativize(file.getParent).iterator().asScala.mkString("."))
    }
    .filter(_.nonEmpty)
    .toSet
}

val faqSection: List[String] = {
  val lines = Files.readAllLines(faqFile).asScala.toList
  val fromHeading = lines.dropWhile(line => !line.startsWith(sectionHeading))
  if (fromHeading.isEmpty) {
    System.err.println(s"Cannot find the section '$sectionHeading' in $faqFile")
    sys.exit(1)
  }
  fromHeading.head :: fromHeading.tail.takeWhile(line => !line.startsWith("## "))
}

val tableRow = """^\|(.*?)\|.*""".r
val backticked = """`([^`]+)`""".r
val packageName = """hearth(\.[a-z_]+)*""".r

def tableCells(column: Int): List[String] = faqSection.collect {
  case row if row.startsWith("|") && !row.startsWith("|--") =>
    row.split('|').lift(column + 1).getOrElse("")
}

// Only the first column lists packages: the "Artifact" column also says `hearth`, which must not count as a listing.
val listedPackages: Set[String] = tableCells(0)
  .flatMap(cell => backticked.findAllMatchIn(cell).map(_.group(1)))
  .filter(packageName.matches)
  .toSet

val mentionedTokens: Set[String] = faqSection.flatMap(line => backticked.findAllMatchIn(line).map(_.group(1))).toSet

// Listing a package covers its subpackages, so the FAQ does not have to change whenever one is added - except for the
// root package, which only covers the classes directly in it.
def isClassified(pkg: String): Boolean =
  listedPackages(pkg) || listedPackages.exists(listed => listed != rootPackage && pkg.startsWith(listed + "."))

val packagesByModule: Map[String, Set[String]] =
  publishedModules.map(module => module.getFileName.toString -> packagesOf(module)).toMap
val allPackages = packagesByModule.values.flatten.toSet

val unclassified = for {
  (module, packages) <- packagesByModule.toList.sortBy(_._1)
  pkg <- packages.toList.sorted
  if !isClassified(pkg)
} yield s"$pkg (module $module)"

val stale = listedPackages.toList.sorted.filterNot(listed => allPackages.exists(pkg => pkg == listed))

val unmentionedModules = packagesByModule.keys.toList.sorted.filterNot(mentionedTokens)

val problems = List(
  "Packages of published modules that the FAQ does not classify as runtime / test-only / compile-time-only" -> unclassified,
  "Packages listed in the FAQ that no published module contains" -> stale,
  "Published modules that the FAQ does not mention" -> unmentionedModules
).filter(_._2.nonEmpty)

@main def checkRuntimePackages(): Unit =
  if (problems.isEmpty) {
    println(
      s"All ${allPackages.size} packages of ${publishedModules.size} published modules are classified in $faqFile"
    )
  } else {
    problems.foreach { case (title, entries) =>
      System.err.println(s"$title:")
      entries.foreach(entry => System.err.println(s"  - $entry"))
    }
    System.err.println(s"Update the tables in the section '$sectionHeading' of $faqFile")
    sys.exit(1)
  }
