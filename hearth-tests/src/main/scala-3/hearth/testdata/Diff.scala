package hearth.testdata

/** Diff of two [[Data]] values */
type Diff = List[DiffEntry]
extension (diff: Diff) {
  def render: String = diff.map(_.render).mkString("\n")
}
