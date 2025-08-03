package hearth.testdata

type Diff = List[DiffEntry]
extension (diff: Diff) {
  def render: String = diff.map(_.render).mkString("\n")
}
