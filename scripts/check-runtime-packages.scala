//> using scala 3.3.9
//> using jvm 17

// Checks the FAQ's tables that sort the packages of the published modules into "used at runtime", "used at runtime,
// but probably only in tests" and "used only during macro expansion":
//
//  1. every package of every published module is in the tables, and the tables list no package that no longer exists,
//  2. (when compiled test classes are passed) the code that macros expanded into, inside test classes, refers only to
//     packages that the tables say are needed at runtime - otherwise users who remove the compile-time packages from
//     their fat JARs, as the FAQ suggests, would get a NoClassDefFoundError.
//
// Usage (from the repository root, after compiling the tests):
//
//   scala-cli run scripts/check-runtime-packages.scala -- \
//     --test-classes hearth-tests/target/jvm-3/test-classes \
//     --own-classes hearth-tests/target/jvm-3/classes
//
// `--test-classes` are checked; `--own-classes` are classes of the test module itself (fixtures, example types), which
// share packages with the library, so references to them are not references to the library. Both can be repeated.
// Without them, only check 1 runs.
//
// A downstream library can reuse this script by changing the configuration below.

import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

// Configuration

val faqFile = Paths.get("docs/user-guide/faq.md")
val sectionHeading = "## Which parts of Hearth end up in my application at runtime?"

sealed trait Group
object Group {
  case object Runtime extends Group
  case object TestOnly extends Group
  case object CompileTimeOnly extends Group
}
val groupHeadings: List[(String, Group)] = List(
  "### Used at runtime: keep" -> Group.Runtime,
  "### Used at runtime, but probably only in tests" -> Group.TestOnly,
  "### Used only during macro expansion" -> Group.CompileTimeOnly
)

val rootPackage = "hearth"

// Test modules are not published, so they never end up in users' fat JARs.
def isPublished(moduleName: String): Boolean =
  !moduleName.contains("tests") && !moduleName.contains("examples")

// Whole artifacts with a known purpose, whose own tests call them directly at runtime, so references to them from test
// classes do not come from expanded macros: micro-FP is meant for macro code only, MUnit suites only for tests.
val artifactsIgnoredInTestClasses: Set[String] = Set("hearth-micro-fp", "hearth-munit")

// Unit tests that call compile-time-only APIs directly, rather than through a macro.
val specsCallingCompileTimeApisDirectly: Set[String] = Set(
  "hearth.std.ProviderResultSpec",
  "hearth.std.RulesSpec"
)

// Published packages

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

val packagesByModule: Map[String, Set[String]] =
  publishedModules.map(module => module.getFileName.toString -> packagesOf(module)).toMap
val allPackages = packagesByModule.values.flatten.toSet

// The FAQ tables

val faqSection: List[String] = {
  val lines = Files.readAllLines(faqFile).asScala.toList
  val fromHeading = lines.dropWhile(line => !line.startsWith(sectionHeading))
  if (fromHeading.isEmpty) {
    System.err.println(s"Cannot find the section '$sectionHeading' in $faqFile")
    sys.exit(1)
  }
  fromHeading.head :: fromHeading.tail.takeWhile(line => !line.startsWith("## "))
}

val backticked = """`([^`]+)`""".r
val packageName = """hearth(\.[a-z_]+)*""".r

// Only the first column lists packages: the "Artifact" column also says `hearth`, which must not count as a listing.
def packagesInFirstColumn(line: String): List[String] =
  if (line.startsWith("|") && !line.startsWith("|--")) {
    val firstCell = line.split('|').lift(1).getOrElse("")
    backticked.findAllMatchIn(firstCell).map(_.group(1)).filter(packageName.matches).toList
  } else Nil

val listedPackages: Map[String, Group] = {
  val (_, listed) = faqSection.foldLeft((Option.empty[Group], Map.empty[String, Group])) {
    case ((_, listed), line) if line.startsWith("### ") =>
      (groupHeadings.collectFirst { case (heading, group) if line.startsWith(heading) => group }, listed)
    case ((Some(group), listed), line) =>
      (Some(group), listed ++ packagesInFirstColumn(line).map(_ -> group))
    case ((None, listed), _) =>
      (None, listed)
  }
  listed
}

val mentionedTokens: Set[String] = faqSection.flatMap(line => backticked.findAllMatchIn(line).map(_.group(1))).toSet

// Listing a package covers its subpackages, so the FAQ does not have to change whenever one is added - except for the
// root package, which only covers the classes directly in it.
def groupOf(pkg: String): Option[Group] =
  listedPackages.get(pkg).orElse {
    listedPackages.toList
      .collect { case (listed, group) if listed != rootPackage && pkg.startsWith(listed + ".") => listed -> group }
      .sortBy(-_._1.length)
      .headOption
      .map(_._2)
  }

// Check 1: the tables cover the published packages

val unclassified = for {
  (module, packages) <- packagesByModule.toList.sortBy(_._1)
  pkg <- packages.toList.sorted
  if groupOf(pkg).isEmpty
} yield s"$pkg (module $module)"

val stale = listedPackages.keys.toList.sorted.filterNot(allPackages)

val unmentionedModules = packagesByModule.keys.toList.sorted.filterNot(mentionedTokens)

// Check 2: expanded macros refer only to runtime packages

def classFilesIn(dir: Path): List[Path] =
  Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".class")).toList

def internalName(dir: Path, classFile: Path): String =
  dir.relativize(classFile).iterator().asScala.mkString("/").stripSuffix(".class")

def topLevelClassName(internalName: String): String =
  internalName.takeWhile(_ != '$').replace('/', '.')

def packageOfInternalName(internalName: String): String =
  internalName.split('/').dropRight(1).mkString(".")

val descriptorReference = """L(hearth/[^;<>]+)[;<]""".r

// The constant pool lists every class the code uses: in `Class` entries (instantiation, calls, casts) and in
// descriptors and generic signatures (field, parameter and result types).
def referencedClasses(classFile: Path): Set[String] = {
  val in = new DataInputStream(new ByteArrayInputStream(Files.readAllBytes(classFile)))
  in.skipBytes(8) // magic, minor and major version
  val count = in.readUnsignedShort()
  val utf8 = Array.ofDim[String](count)
  val classNameIndices = List.newBuilder[Int]
  var index = 1
  while (index < count) {
    in.readUnsignedByte() match {
      case 1                                  => utf8(index) = in.readUTF()
      case 7                                  => classNameIndices += in.readUnsignedShort()
      case 8 | 16 | 19 | 20                   => in.skipBytes(2)
      case 15                                 => in.skipBytes(3)
      case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 => in.skipBytes(4)
      case 5 | 6                              => in.skipBytes(8); index += 1 // longs and doubles take two slots
      case tag => throw new IllegalStateException(s"Unknown constant pool tag $tag in $classFile")
    }
    index += 1
  }
  val fromClassEntries = classNameIndices.result().map(utf8(_)).filter(_.startsWith("hearth/"))
  val fromDescriptors = utf8.iterator.filter(_ != null).flatMap(descriptorReference.findAllMatchIn(_).map(_.group(1)))
  (fromClassEntries ++ fromDescriptors).toSet
}

def directoriesAfter(flag: String, args: List[String]): List[Path] = args
  .sliding(2)
  .collect { case List(`flag`, dir) => Paths.get(dir) }
  .toList

def isOnlyInIgnoredArtifacts(pkg: String): Boolean = {
  val modules = packagesByModule.collect { case (module, packages) if packages(pkg) => module }
  modules.nonEmpty && modules.forall(artifactsIgnoredInTestClasses)
}

def referencesToCompileTimePackages(testClassDirs: List[Path], ownClassDirs: List[Path]): List[String] = {
  val ownClasses = (testClassDirs ++ ownClassDirs).flatMap(dir => classFilesIn(dir).map(internalName(dir, _))).toSet
  val references = for {
    dir <- testClassDirs
    classFile <- classFilesIn(dir)
    from = topLevelClassName(internalName(dir, classFile))
    if !specsCallingCompileTimeApisDirectly(from)
    referenced <- referencedClasses(classFile)
    if !ownClasses(referenced)
    pkg = packageOfInternalName(referenced)
    if !isOnlyInIgnoredArtifacts(pkg)
    group = groupOf(pkg)
    if !group.contains(Group.Runtime) && !group.contains(Group.TestOnly)
  } yield (referenced.replace('/', '.'), group, from)
  references
    .groupBy { case (referenced, group, _) => (referenced, group) }
    .toList
    .sortBy(_._1._1)
    .map { case ((referenced, group), usages) =>
      val groupName = group.fold("not in the tables")(_ => "used only during macro expansion")
      s"$referenced ($groupName), referred to from: ${usages.map(_._3).distinct.sorted.mkString(", ")}"
    }
}

@main def checkRuntimePackages(args: String*): Unit = {
  val testClassDirs = directoriesAfter("--test-classes", args.toList)
  val ownClassDirs = directoriesAfter("--own-classes", args.toList)
  val missingDirs = (testClassDirs ++ ownClassDirs).filterNot(Files.isDirectory(_))
  if (missingDirs.nonEmpty) {
    System.err.println(s"Not a directory: ${missingDirs.mkString(", ")} - compile the tests first")
    sys.exit(1)
  }

  val problems = List(
    "Packages of published modules that the FAQ does not classify as runtime / test-only / compile-time-only" -> unclassified,
    "Packages listed in the FAQ that no published module contains" -> stale,
    "Published modules that the FAQ does not mention" -> unmentionedModules,
    "Classes that code expanded by macros refers to, but that the FAQ does not list as needed at runtime" ->
      referencesToCompileTimePackages(testClassDirs, ownClassDirs)
  ).filter(_._2.nonEmpty)

  if (problems.isEmpty) {
    println(
      s"All ${allPackages.size} packages of ${publishedModules.size} published modules are classified in $faqFile"
    )
    if (testClassDirs.nonEmpty)
      println(s"Classes in ${testClassDirs.mkString(", ")} refer only to packages needed at runtime")
  } else {
    problems.foreach { case (title, entries) =>
      System.err.println(s"$title:")
      entries.foreach(entry => System.err.println(s"  - $entry"))
    }
    System.err.println(s"Update the tables in the section '$sectionHeading' of $faqFile")
    sys.exit(1)
  }
}
