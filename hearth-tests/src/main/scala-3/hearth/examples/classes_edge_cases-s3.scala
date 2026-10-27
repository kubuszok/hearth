package hearth
package examples
package classesedge

case class CCUsingParam(i: Int)(using val s: String) {
  override def toString(): String = s"CCUsingParam($i)($s)"
}

trait AITraitWithUsing {
  def run(cmd: String)(using cfg: Int): String
}
