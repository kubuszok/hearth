import sbt.*
import sbt.Keys.*
import sbt.internal.inc.Analysis
import xsbti.api.{Def as _, *}

// Vibe-codec solution to generate META-INF/services files from incremental compile analysis.
object ServiceResourceGenPlugin extends AutoPlugin {
  object autoImport {
    val macroExtensionTraits =
      settingKey[Seq[String]]("Traits for which META-INF/services/<name> files are generated (requires FCQN)")
    val generateServiceFile = taskKey[Seq[File]]("Generate META-INF/services files from incremental compile analysis")
  }
  import autoImport.*

  override def trigger = allRequirements

  override def projectSettings: Seq[Def.Setting[?]] = Seq(
    // set defaults; override per-module if needed
    macroExtensionTraits := Seq(),
    generateServiceFile := {
      if (macroExtensionTraits.value.isEmpty) Seq()
      else {
        val log = streams.value.log

        // Cached analysis from incremental compiler
        val analysis = (Compile / compile).value.asInstanceOf[Analysis]

        // Ensure the output directory exists
        val outDir = (Compile / resourceManaged).value / "META-INF" / "services"
        IO.createDirectory(outDir)

        val traits = macroExtensionTraits.value
        log.debug(s"Generating META-INF/services files for traits: ${traits.mkString(", ")} in ${outDir.getPath}")

        val apis = analysis.apis.internal

        // Helper to extract FQCN from a Path
        def pathToFqcn(path: Path): String = {
          val parts = path.components.collect {
            // Use fully-qualified type to avoid clash with sbt.Id
            case pc: xsbti.api.Id => pc.id
          }
          parts.mkString(".")
        }

        // Helper to extract FQCN from a Type
        def extractFqcn(tpe: Type): Option[String] =
          tpe match {
            case p: Parameterized => extractFqcn(p.baseType)
            case s: Singleton     =>
              // Singleton has a path that can be converted to FQCN
              Some(pathToFqcn(s.path))
            case pr: Projection =>
              // For Projection, build FQCN from prefix + id
              pr.prefix match {
                case s: Singleton =>
                  // If prefix is Singleton, extract path and combine with id
                  Some(pathToFqcn(s.path) + "." + pr.id)
                case _ =>
                  // Otherwise, try recursive extraction
                  extractFqcn(pr.prefix).map(_ + "." + pr.id).orElse {
                    // Fallback: if prefix extraction fails, try to use id alone if it looks like FQCN
                    if (pr.id.contains(".")) Some(pr.id) else None
                  }
              }
            case _: Structure => None // structural types don't have FQCNs
            case _            => None
          }

        // Helper to get parent FQCNs from a ClassLike
        def getParentFqcns(cl: ClassLike): Set[String] =
          cl.structure.parents.flatMap(extractFqcn).toSet

        // Helper to check if a class is a concrete (non-abstract) implementation
        def isConcreteImplementation(fqcn: String): Boolean =
          apis.get(fqcn).exists { api =>
            Option(api.api.classApi).exists { cl =>
              // Must be a class (not a trait/interface) and not abstract
              cl.definitionType == DefinitionType.ClassDef && !cl.modifiers.isAbstract
            }
          }

        // Find all direct implementations of a trait by checking all classes
        def findDirectImplementations(traitFqcn: String): Set[String] =
          apis
            .collect {
              case (childFqcn, api) if isConcreteImplementation(childFqcn) =>
                Option(api.api.classApi).collect {
                  case cl if getParentFqcns(cl).contains(traitFqcn) => childFqcn
                }
            }
            .flatten
            .toSet

        traits.map { macroExtensionTrait =>
          val outFile = outDir / macroExtensionTrait

          val providers: Seq[String] = findDirectImplementations(macroExtensionTrait).toSeq.sorted

          // Don’t rewrite unless content changed (keeps downstream caching stable)
          val old = if (outFile.exists) IO.readLines(outFile) else Nil
          if (old != providers) {
            IO.writeLines(outFile, providers)
            log.info(s"Wrote ${outFile.getPath} (${providers.size} providers)")
          } else {
            log.debug(s"Unchanged ${outFile.getPath}")
          }

          outFile
        }.toSeq
      }
    },

    // wire it into resources
    Compile / resourceGenerators += generateServiceFile.taskValue
  )
}
