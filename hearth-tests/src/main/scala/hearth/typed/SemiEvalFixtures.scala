package hearth
package typed

case class SemiEvalConfig(value: Int, name: String) {

  def withMapper(f: String => String): String = f(name)
}
object SemiEvalConfig {

  val default: SemiEvalConfig = SemiEvalConfig(0, "default")

  def create(value: Int): SemiEvalConfig = SemiEvalConfig(value, "created")

  def transform(s: String): String = s.toUpperCase
}
