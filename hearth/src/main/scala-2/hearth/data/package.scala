package hearth

package object data {

  /** JSON-like data model used pervasively in Hearth tests.
    *
    * A macro can return only one value, so [[Data]] lets a fixture emit many results at once as one easily-diffable
    * structure. Build with `Data(x)` (primitives), `Data.list(...)`, `Data.map("f" -> Data(...))`; compare in specs
    * with the `<==>` operator, which reports a structured [[Diff]] on mismatch.
    *
    * @see
    *   [[Diff]] and [[DiffEntry]] for the mismatch model
    * @see
    *   `hearth.Suite` `<==>` assertion
    * @see
    *   docs/user-guide/basic-utilities.md
    * @since 0.1.0
    */
  type Data = Data.Impl

  /** Diff of two [[Data]] values */
  type Diff = List[DiffEntry]
  implicit final class DiffOps(private val diff: Diff) extends AnyVal {
    def render: String = diff.map(_.render).mkString("\n")
  }
}
