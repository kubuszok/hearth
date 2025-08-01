package hearth.data

case class DiffEntry(path: String, expected: Data, actual: Data) {

  def prependIndex(idx: Int): DiffEntry = copy(path = s"[$idx]$path")
  def prependKey(key: String): DiffEntry = copy(path = s"$key.$path")

  def render: String = {
    val p = s"${Console.BLUE}$path${Console.RESET}"
    val e = expected.render.split("\n").map("  - " + _).mkString(Console.RED, "\n", Console.RESET)
    val a = actual.render.split("\n").map("  + " + _).mkString(Console.GREEN, "\n", Console.RESET)
    s"$p:\n$e\n$a"
  }
}
