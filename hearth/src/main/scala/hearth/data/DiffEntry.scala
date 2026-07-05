package hearth
package data

/** A single mismatch in a [[Diff]]: `expected` vs `actual` at a `path`.
  *
  * @param expected
  *   the value expected at `path`
  * @param actual
  *   the value actually found at `path`
  * @param path
  *   location of the mismatch within the [[Data]] structure (map keys and list indices)
  * @since 0.1.0
  */
final case class DiffEntry(expected: Data, actual: Data, path: List[DiffEntry.Path] = Nil) {

  /** Prepends a list-index segment to the [[path]] (used while walking arrays outside-in).
    *
    * @since 0.1.0
    */
  def prependIndex(idx: Int): DiffEntry = copy(path = DiffEntry.Index(idx) :: path)

  /** Prepends a map-key segment to the [[path]] (used while walking objects outside-in).
    *
    * @since 0.1.0
    */
  def prependKey(key: String): DiffEntry = copy(path = DiffEntry.Key(key) :: path)

  lazy val renderedPath: String = DiffEntry.renderPath(path)

  /** Pretty multi-line rendering of this mismatch (path, expected in red, actual in green).
    *
    * @since 0.1.0
    */
  def render: String =
    s"""${Console.BLUE}$renderedPath${Console.RESET}:
       |${Console.RED}${expected.render.split("\n").map("  - " + _).mkString("\n")}${Console.RESET}
       |${Console.GREEN}${actual.render.split("\n").map("  + " + _).mkString("\n")}${Console.RESET}""".stripMargin
}
object DiffEntry {

  sealed trait Path
  case class Key(key: String) extends Path
  case class Index(idx: Int) extends Path

  private def renderPath(path: List[Path]): String = {
    val res = path.map {
      case Key(key)   => s".$key"
      case Index(idx) => s"[$idx]"
    }.mkString
    if (res.startsWith(".")) res.drop(1) else res
  }

  implicit val diffEntryOrdering: Ordering[DiffEntry] = Ordering.by(_.renderedPath)
}
