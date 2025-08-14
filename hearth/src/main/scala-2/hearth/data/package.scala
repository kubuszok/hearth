package hearth

package object data {

  /** JSON-like data for usage in tests.
    *
    * Since macro can have only 1 result we can generate multiple things at once and return it as something easily
    * diffable.
    */
  type Data = Data.Impl

  /** Diff of two [[Data]] values */
  type Diff = List[DiffEntry]
  implicit final class DiffOps(private val diff: Diff) extends AnyVal {
    def render: String = diff.map(_.render).mkString("\n")
  }
}
