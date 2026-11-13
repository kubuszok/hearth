package hearth
package typed

/** Tells whether a class loaded by a macro comes from the output directory of the compilation expanding it.
  *
  * Such a class was compiled from the same module by an earlier (incremental) compilation run, so it is stale: after a
  * clean build the same class does not exist yet when the macro expands.
  */
private[typed] object CompilationOutput {

  def canonicalPath(path: String): String =
    scala.util.Try(new java.io.File(path).getCanonicalPath).getOrElse(path)

  def isLoadedFrom(outputDirectories: List[String], clazz: java.lang.Class[?]): Boolean =
    outputDirectories.nonEmpty && locationOf(clazz).exists { location =>
      outputDirectories.exists(dir => location == dir || location.startsWith(dir + java.io.File.separator))
    }

  private def locationOf(clazz: java.lang.Class[?]): Option[String] =
    scala.util
      .Try(clazz.getProtectionDomain.getCodeSource.getLocation.toURI)
      .toOption
      .flatMap(uri => scala.util.Try(canonicalPath(new java.io.File(uri).getPath)).toOption)
}
