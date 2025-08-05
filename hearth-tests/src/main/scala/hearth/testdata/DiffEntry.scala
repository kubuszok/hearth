package hearth
package testdata

final case class DiffEntry(expected: Data, actual: Data, path: List[DiffEntry.Path] = Nil) {

  def prependIndex(idx: Int): DiffEntry = copy(path = DiffEntry.Index(idx) :: path)
  def prependKey(key: String): DiffEntry = copy(path = DiffEntry.Key(key) :: path)

  lazy val renderedPath: String = DiffEntry.renderPath(path)

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
