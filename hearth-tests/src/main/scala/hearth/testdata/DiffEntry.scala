package hearth
package testdata

final case class DiffEntry(expected: Data, actual: Data, path: List[DiffEntry.Path] = Nil) {

  def prependIndex(idx: Int): DiffEntry = copy(path = DiffEntry.Index(idx) :: path)
  def prependKey(key: String): DiffEntry = copy(path = DiffEntry.Key(key) :: path)

  def render: String = {
    val p = s"${Console.BLUE}${DiffEntry.renderPath(path)}${Console.RESET}"
    val e = expected.render.split("\n").map("  - " + _).mkString(Console.RED, "\n", Console.RESET)
    val a = actual.render.split("\n").map("  + " + _).mkString(Console.GREEN, "\n", Console.RESET)
    s"$p:\n$e\n$a"
  }
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
}
